;; nostr-core.el --- Core Nostr protocol implementation -*- lexical-binding: t; -*-

;;; Commentary:
;; Implements core Nostr protocol functionality, focusing on event validation
;; and publication features compliant with NIP-01 and NIP-62.

(require 'nak-wrapper)
(require 'jq-wrapper)

;;; Code:

;; Core Data Structures

(cl-defstruct nostr-event
  "Core Nostr event structure."
  id                 ; Event ID (hex string)
  pubkey            ; Public key (hex string)
  created_at        ; Unix timestamp
  kind              ; Event kind (integer)
  tags              ; List of tag vectors
  content           ; Content string
  sig)              ; Signature (hex string)

(cl-defstruct nostr-filter
  "Event filter structure."
  ids               ; List of event IDs
  authors           ; List of pubkeys
  kinds             ; List of event kinds
  tags              ; Hashtable of tag filters
  since             ; Unix timestamp
  until             ; Unix timestamp
  limit)            ; Maximum events to return

;; Event Kind Ranges
(defconst nostr-kind-ranges
  '((regular     . ((1 . 2) (4 . 45) (1000 . 9999)))
    (replaceable . ((0 . 0) (3 . 3) (10000 . 19999)))
    (ephemeral   . ((20000 . 29999)))
    (addressable . ((30000 . 39999))))
  "NIP-01 event kind classifications.")

;; Event Validation

(defun nostr-validate-event (event)
  "Validate NOSTR-EVENT according to NIP-01 requirements."
  (let ((errors '()))
    (dolist (field '(id pubkey created_at kind tags content sig))
      (unless (slot-value event field)
        (push (format "Missing required field: %s" field) errors)))
    ;; Return validation result
    (list :valid (null errors) :errors errors)))

;; Publication Framework (NIP-62)

(defun nostr-publication-create (title content &optional metadata)
  "Create Nostr publication with TITLE and CONTENT."
  (let* ((sections (nostr-split-sections content))
         (index (nostr-create-index title sections metadata))
         (section-events (mapcar #'nostr-create-section sections)))
    (list :index index :sections section-events)))

(defun nostr-create-index (title sections metadata)
  "Create kind 30040 index event for publication."
  (let* ((doc-id (nostr-generate-id))
         (tags `(("d" ,doc-id)
                 ("title" ,title)
                 ("auto-update" "yes")
                 ,@(when metadata
                     (nostr-metadata-to-tags metadata))))
         (event (make-nostr-event
                 :kind 30040
                 :tags tags
                 :content "")))
    event))

(defun nostr-create-section (section)
  "Create kind 30041 event for publication section."
  (let* ((section-id (nostr-generate-id))
         (tags `(("d" ,section-id)
                 ("title" ,(plist-get section :title))))
         (event (make-nostr-event
                 :kind 30041
                 :tags tags
                 :content (plist-get section :content))))
    event))

;; Utility Functions

(defun nostr-generate-id ()
  "Generate a unique identifier for events."
  (secure-hash 'sha256 (concat (current-time-string) (random))))

(defun nostr-metadata-to-tags (metadata)
  "Convert METADATA plist to Nostr tags."
  (cl-loop for (key value) on metadata by #'cddr
           when value
           collect (list (substring (symbol-name key) 1) value)))

(defun nostr-split-sections (content)
  "Split CONTENT into sections based on AsciiDoc headers."
  ;; TODO: Implement AsciiDoc parsing
  (list (list :title "Section 1" :content content)))

(defun nostr-event-to-json (event)
  "Convert EVENT struct to JSON format for NAK."
  (json-encode
   `((id . ,(nostr-event-id event))
     (pubkey . ,(nostr-event-pubkey event))
     (created_at . ,(nostr-event-created_at event))
     (kind . ,(nostr-event-kind event))
     (tags . ,(nostr-event-tags event))
     (content . ,(nostr-event-content event))
     (sig . ,(nostr-event-sig event)))))

(defun nostr-json-to-event (json)
  "Convert JSON from NAK to EVENT struct."
  (let ((data (json-read-from-string json)))
    (make-nostr-event
     :id (alist-get 'id data)
     :pubkey (alist-get 'pubkey data)
     :created_at (alist-get 'created_at data)
     :kind (alist-get 'kind data)
     :tags (alist-get 'tags data)
     :content (alist-get 'content data)
     :sig (alist-get 'sig data))))
(defun nostr-core-publish-event (event relays callback error-callback)
  "Publish nostr EVENT to RELAYS. Call CALLBACK on success, ERROR-CALLBACK on failure."
  (let ((args (list
               "--sec" (nostr-login-get-key)
               "-k" (number-to-string (nostr-event-kind event))
               "-c" (nostr-event-content event))))

    ;; Add tags
    (when (nostr-event-tags event)
      (dolist (tag (nostr-event-tags event))
        (setq args (append args (list "-t" tag)))))

    ;; Add relays at end of args list
    (setq args (append args relays))

    (nak-execute "event"
                 :args args
                 :callback (lambda (output)
                             (when (string-match "\"id\":\"\\([^\"]+\\)\"" output)
                               (let ((event-id (match-string 1 output)))
                                 (nak-execute "encode"
                                              :args (list "nevent" event-id)
                                              :callback callback
                                              :error-callback error-callback))))
                 :error-callback error-callback)))
(cl-defun nostr-create-post (&key content (kind 1) tags relays callback error-callback)
  "Create and publish a Nostr post.
Returns the event's bech32 encoded identifier (nevent or naddr)."
  (when-let ((key (nostr-post-ensure-key)))
    (let ((event (make-nostr-event
                  :kind kind
                  :content content
                  :tags tags)))
      (nostr-core-publish-event event relays callback error-callback))))
(defun nostr-get-npub (npub-id callback error-callback)
  "Retrieve a Nostr publication by its NPUB-ID."
  (nak-execute "encode"
               :args (list "npub" npub-id)
               :callback callback
               :error-callback error-callback))

(defun nostr-get-user-npub ()
  "Print npub of user."
  (interactive)
  (nostr-get-npub (nostr-login-get-pubkey)
                  (lambda (output)
                    (message output))
                  (lambda (output)
                    (message output)))
  )

(provide 'nostr-core)
;;; nostr-core.el ends here
