;; nostr-db.el --- Nostr event database management -*- lexical-binding: t; -*-

;;; Commentary:
;; Implements a SQLite-based storage system for Nostr events with integration
;; for nak and jq processing capabilities.

;;; Code:

(require 'sqlite)
(require 'nak-wrapper)
(require 'jq-wrapper)
(require 'json)
(eval-when-compile (require 'cl-lib))

;;; Configuration

(defgroup nostr-db nil
  "Nostr database configuration."
  :group 'nostr)

(defcustom nostr-db-location
  (expand-file-name "nostr.db"
                    (or (getenv "XDG_DATA_HOME")
                        (expand-file-name "~/.local/share")))
  "Location of the Nostr SQLite database."
  :type 'string
  :group 'nostr-db)

(defcustom nostr-db-cache-size 2000
  "Maximum number of events to cache in memory."
  :type 'integer
  :group 'nostr-db)

(defvar nostr-db--connection nil
  "Active database connection.")

(defvar nostr-db--event-cache (make-hash-table :test 'equal)
  "Cache for frequently accessed events.")

;;; Core Database Management

(defun nostr-db-ensure-connection ()
  "Ensure database connection is active and schema is initialized."
  (unless (and nostr-db--connection
               (sqlite-available-p nostr-db--connection))
    (setq nostr-db--connection
          (sqlite-open nostr-db-location))
    (nostr-db--init-schema)))

(defun nostr-db--init-schema ()
  "Initialize database schema if needed."
  (sqlite-execute
   nostr-db--connection
   "CREATE TABLE IF NOT EXISTS events (
      id TEXT PRIMARY KEY,
      pubkey TEXT NOT NULL,
      created_at INTEGER NOT NULL,
      kind INTEGER NOT NULL,
      content TEXT,
      sig TEXT NOT NULL,
      tags JSON,
      UNIQUE(pubkey, kind, created_at)
    );

    CREATE INDEX IF NOT EXISTS idx_events_pubkey ON events(pubkey);
    CREATE INDEX IF NOT EXISTS idx_events_kind ON events(kind);
    CREATE INDEX IF NOT EXISTS idx_events_created ON events(created_at);

    CREATE TABLE IF NOT EXISTS replaceable_events (
      kind INTEGER NOT NULL,
      pubkey TEXT NOT NULL,
      d_tag TEXT,
      current_id TEXT NOT NULL,
      PRIMARY KEY (kind, pubkey, d_tag),
      FOREIGN KEY (current_id) REFERENCES events(id)
    );

    CREATE TABLE IF NOT EXISTS profiles (
      pubkey TEXT PRIMARY KEY,
      metadata JSON,
      updated_at INTEGER NOT NULL
    );

    CREATE TABLE IF NOT EXISTS follows (
      follower TEXT NOT NULL,
      followed TEXT NOT NULL,
      created_at INTEGER NOT NULL,
      metadata JSON,
      PRIMARY KEY (follower, followed)
    );

    CREATE TABLE IF NOT EXISTS publications (
      id TEXT PRIMARY KEY,
      title TEXT NOT NULL,
      author TEXT NOT NULL,
      created_at INTEGER NOT NULL,
      metadata JSON,
      FOREIGN KEY (id) REFERENCES events(id)
    );

    CREATE TABLE IF NOT EXISTS publication_sections (
      publication_id TEXT NOT NULL,
      section_id TEXT NOT NULL,
      ordering INTEGER NOT NULL,
      PRIMARY KEY (publication_id, section_id),
      FOREIGN KEY (publication_id) REFERENCES publications(id),
      FOREIGN KEY (section_id) REFERENCES events(id)
    );

    -- Vector storage preparation
    CREATE TABLE IF NOT EXISTS vector_metadata (
      event_id TEXT PRIMARY KEY,
      vector_type TEXT NOT NULL,
      dimensions INTEGER NOT NULL,
      metadata JSON,
      FOREIGN KEY (event_id) REFERENCES events(id)
    );

    CREATE TABLE IF NOT EXISTS vector_data (
      event_id TEXT NOT NULL,
      vector BLOB NOT NULL,
      FOREIGN KEY (event_id) REFERENCES vector_metadata(event_id)
    );"))

;;; Event Storage and Retrieval

(cl-defun nostr-db-store-event (event &key (update-replaceable t))
  "Store EVENT in database, optionally UPDATE-REPLACEABLE events."
  (nostr-db-ensure-connection)
  (let* ((event-data (if (stringp event)
                         (json-read-from-string event)
                       event))
         (event-id (alist-get 'id event-data))
         (pubkey (alist-get 'pubkey event-data))
         (kind (alist-get 'kind event-data))
         (created-at (alist-get 'created_at event-data))
         (tags (json-encode (alist-get 'tags event-data)))
         (content (alist-get 'content event-data))
         (sig (alist-get 'sig event-data)))

    (sqlite-execute
     nostr-db--connection
     "INSERT OR REPLACE INTO events
      (id, pubkey, created_at, kind, content, sig, tags)
      VALUES (?, ?, ?, ?, ?, ?, ?)"
     event-id pubkey created-at kind content sig tags)

    ;; Handle replaceable events
    (when (and update-replaceable
               (nostr-db--is-replaceable-kind-p kind))
      (nostr-db--update-replaceable event-data))

    ;; Update cache
    (puthash event-id event-data nostr-db--event-cache)

    ;; Trim cache if needed
    (when (> (hash-table-count nostr-db--event-cache)
             nostr-db-cache-size)
      (nostr-db--trim-cache))

    event-id))

(defun nostr-db--is-replaceable-kind-p (kind)
  "Check if KIND is a replaceable event type."
  (or (= kind 0)
      (= kind 3)
      (and (>= kind 10000)
           (< kind 20000))))

(defun nostr-db--update-replaceable (event)
  "Update replaceable event tracking for EVENT."
  (let* ((kind (alist-get 'kind event))
         (pubkey (alist-get 'pubkey event))
         (d-tag (nostr-db--extract-d-tag event))
         (event-id (alist-get 'id event)))
    (sqlite-execute
     nostr-db--connection
     "INSERT OR REPLACE INTO replaceable_events
      (kind, pubkey, d_tag, current_id)
      VALUES (?, ?, ?, ?)"
     kind pubkey d-tag event-id)))

(defun nostr-db--extract-d-tag (event)
  "Extract d tag value from EVENT if present."
  (let ((tags (alist-get 'tags event)))
    (cl-loop for tag in tags
             when (and (vectorp tag)
                       (string= (aref tag 0) "d"))
             return (aref tag 1))))

(defun nostr-db--trim-cache ()
  "Remove oldest entries from cache to maintain size limit."
  (let* ((entries (hash-table-keys nostr-db--event-cache))
         (excess (- (length entries) nostr-db-cache-size)))
    (cl-loop repeat excess
             for key in entries
             do (remhash key nostr-db--event-cache))))

;;; Query Interface

(cl-defun nostr-db-query (&key ids authors kinds tags since until limit)
  "Query events using Nostr filter parameters.
Returns events matching specified criteria."
  (nostr-db-ensure-connection)
  (let ((conditions '())
        (params '()))

    (when ids
      (push "id IN (?)" conditions)
      (push (string-join ids ",") params))

    (when authors
      (push "pubkey IN (?)" conditions)
      (push (string-join authors ",") params))

    (when kinds
      (push "kind IN (?)" conditions)
      (push (string-join (mapcar #'number-to-string kinds) ",") params))

    (when since
      (push "created_at >= ?" conditions)
      (push since params))

    (when until
      (push "created_at <= ?" conditions)
      (push until params))

    ;; Handle tag filtering using jq
    (when tags
      (push "json_valid(tags)" conditions)
      (cl-loop for (key . values) in tags
               do (push (format "json_array_length(json_extract(tags, '$.%s')) > 0" key)
                        conditions)))

    (let ((where-clause
           (if conditions
               (concat " WHERE " (string-join conditions " AND "))
             ""))
          (limit-clause
           (if limit
               (format " LIMIT %d" limit)
             "")))

      (sqlite-select
       nostr-db--connection
       (concat "SELECT * FROM events"
               where-clause
               " ORDER BY created_at DESC"
               limit-clause)
       params))))

;;; Publication Management (NIP-62)

(defun nostr-db-store-publication (index-event section-events)
  "Store a complete publication with INDEX-EVENT and SECTION-EVENTS."
  (nostr-db-ensure-connection)
  (let* ((index-id (alist-get 'id index-event))
         (author (alist-get 'pubkey index-event))
         (created-at (alist-get 'created_at index-event))
         (title (nostr-db--extract-title-tag index-event)))

    ;; Store all events
    (nostr-db-store-event index-event)
    (mapc #'nostr-db-store-event section-events)

    ;; Create publication record
    (sqlite-execute
     nostr-db--connection
     "INSERT OR REPLACE INTO publications
      (id, title, author, created_at, metadata)
      VALUES (?, ?, ?, ?, ?)"
     index-id title author created-at
     (json-encode (alist-get 'tags index-event)))

    ;; Link sections
    (cl-loop for section in section-events
             for idx from 0
             do (sqlite-execute
                 nostr-db--connection
                 "INSERT OR REPLACE INTO publication_sections
                  (publication_id, section_id, ordering)
                  VALUES (?, ?, ?)"
                 index-id
                 (alist-get 'id section)
                 idx))

    index-id))

(defun nostr-db--extract-title-tag (event)
  "Extract title from EVENT tags."
  (let ((tags (alist-get 'tags event)))
    (cl-loop for tag across tags
             when (and (= (length tag) 2)
                       (string= (aref tag 0) "title"))
             return (aref tag 1))))

(provide 'nostr-db)
;;; nostr-db.el ends here
