;; nak-wrapper.el --- Improved wrapper for nak nostr tool -*- lexical-binding: t; -*-

(require 'json)
(require 'cl-lib)
(require 'jq-wrapper)

(defgroup nak nil
  "Settings for nak command wrapper."
  :group 'tools)

(defcustom nak-executable "nak"
  "Path to the nak executable."
  :type 'string
  :group 'nak)

(defcustom nak-default-relays '("wss://relay.damus.io" "wss://nos.lol")
  "List of default relays to use."
  :type '(repeat string)
  :group 'nak)

(defvar nak-debug nil
  "When non-nil, print debug information.")

(defun nak--debug (format-string &rest args)
  "Print debug message when nak-debug is enabled."
  (when nak-debug
    (apply #'message (concat "nak-debug: " format-string) args)))

(cl-defun nak--make-process (&key name command callback error-callback)
  "Create a nak process with given parameters."
  (let ((process-buffer (generate-new-buffer (format " *nak-%s*" name))))
    (make-process
     :name name
     :buffer process-buffer
     :command command
     :sentinel
     (lambda (process event)
       (let ((status (process-exit-status process))
             (output (with-current-buffer (process-buffer process)
                       (buffer-string))))
         (cond
          ((zerop status)
           (when callback
             (funcall callback output)))
          (t
           (when error-callback
             (funcall error-callback
                      (format "Process failed with status %d: %s"
                              status output))))))))))

(cl-defun nak-execute (subcommand &key args relays callback error-callback jq stream)
  "Execute a nak SUBCOMMAND with the given parameters.
RELAYS can be a string (single relay) or a list of relay URLs."
  (let* ((relay-list (cond
                      ((stringp relays) (list relays))
                      ((listp relays) relays)
                      (t nil)))
         ;; Command args are nak + subcommand + options + relays at the end
         (command-args (append (list nak-executable subcommand)
                               args  ; args are now in correct order
                               relay-list))
         (process-name (format "nak-%s" subcommand)))

    (nak--debug "Executing: %s" (mapconcat #'identity command-args " "))

    (nak--make-process
     :name process-name
     :command command-args
     :callback (if jq
                   (lambda (output)
                     (condition-case err
                         (let ((clean-output (nak--clean-relay-output output)))
                           (if stream
                               ;; For streamed output, process each line
                               (dolist (line (split-string clean-output "\n" t))
                                 (jq-parse line jq
                                           :callback callback
                                           :raw nil))
                             ;; For regular output, process as single chunk
                             (jq-parse clean-output jq
                                       :callback callback
                                       :raw nil)))
                       (error
                        (when error-callback
                          (funcall error-callback
                                   (format "JQ processing failed: %s" err))))))
                 callback)
     :error-callback error-callback)))

(defun nak--clean-relay-output (output)
  "Clean relay connection messages from OUTPUT.
Returns cleaned JSON output ready for processing."
  (let* ((clean-output (replace-regexp-in-string "connecting to.*\n" "" output))
         (clean-output (replace-regexp-in-string "ok\\.\n" "" clean-output))
         (clean-output (string-trim clean-output)))
    ;; If we have multiple JSON objects, wrap them in an array
    (if (string-match-p "\n" clean-output)
        (format "[%s]"
                (mapconcat #'identity
                           (split-string clean-output "\n" t)
                           ","))
      clean-output)))

;; High-level API functions

(cl-defun nak-req (&key kinds authors limit tags since until relays callback error-callback jq stream)
  "Execute a nak req command with the given filters.
If STREAM is non-nil, process each event as it arrives."
  (let ((args nil))
    ;; Build arguments list in the correct order
    (when stream
      (setq args (append args (list "--stream"))))
    (when kinds
      (dolist (kind kinds)
        (setq args (append args (list "-k" (number-to-string kind))))))
    (when authors
      (dolist (author authors)
        (setq args (append args (list "-a" author)))))
    (when limit
      (setq args (append args (list "--limit" (number-to-string limit)))))
    (when tags
      (dolist (tag tags)
        (setq args (append args (list "-t" tag)))))
    (when since
      (setq args (append args (list "--since" (number-to-string since)))))
    (when until
      (setq args (append args (list "--until" (number-to-string until)))))

    (nak-execute "req"
                 :args args  ; Don't reverse here, will be reversed in nak-execute
                 :relays (or relays nak-default-relays)
                 :callback callback
                 :error-callback error-callback
                 :jq jq
                 :stream stream)))

(cl-defun nak-event (&key content kind tags relays callback error-callback)
  "Create and optionally publish a nostr event."
  (let ((args (list)))
    (when content
      (push content args)
      (push "-c" args))
    (when kind
      (push (number-to-string kind) args)
      (push "-k" args))
    (when tags
      (dolist (tag tags)
        (push tag args)
        (push "-t" args)))

    (nak-execute "event"
                 :args (nreverse args)
                 :relays (or relays nak-default-relays)
                 :callback callback
                 :error-callback error-callback)))

(cl-defun nak-fetch (target &key relays callback error-callback jq)
  "Fetch nostr content identified by TARGET."
  (nak-execute "fetch"
               :args (list target)
               :relays (or relays nak-default-relays)
               :callback callback
               :error-callback error-callback
               :jq jq))

;; Utility functions

(defun nak-get-user-metadata (pubkey &optional callback)
  "Fetch metadata for PUBKEY."
  (nak-req :kinds '(0)
           :authors (list pubkey)
           :limit 1
           :jq ".content | fromjson"
           :callback callback))

(defun nak-get-user-notes (pubkey limit &optional callback)
  "Fetch recent notes for PUBKEY."
  (nak-req :kinds '(1)
           :authors (list pubkey)
           :limit limit
           :callback callback))

(defun nak-get-thread (event-id &optional callback)
  "Fetch thread context for EVENT-ID."
  (nak-req :tags (list (concat "e=" event-id))
           :callback callback))

;; USAGE:
;;    nak encode [command [command options]] [arguments...]

;; DESCRIPTION:
;;    example usage:
;;        nak encode npub <pubkey-hex>
;;        nak encode nprofile <pubkey-hex>
;;        nak encode nprofile --relay <relay-url> <pubkey-hex>
;;        nak encode nevent <event-id>
;;        nak encode nevent --author <pubkey-hex> --relay <relay-url> --relay <other-relay> <event-id>
;;        nak encode nsec <privkey-hex>
(defun nak-encode (subcommand &rest args)
  "Encode a nak SUBCOMMAND with the given ARGS."
  (nak-execute "encode"
               :args (cons subcommand args)))
;; USAGE:
;;    nak decode [command [command options]] <npub | nprofile | nip05 | nevent | naddr | nsec>

;; DESCRIPTION:
;;    example usage:
;;        nak decode npub1uescmd5krhrmj9rcura833xpke5eqzvcz5nxjw74ufeewf2sscxq4g7chm
;;        nak decode nevent1qqs29yet5tp0qq5xu5qgkeehkzqh5qu46739axzezcxpj4tjlkx9j7gpr4mhxue69uhkummnw3ez6ur4vgh8wetvd3hhyer9wghxuet5sh59ud
;;        nak decode nprofile1qqsrhuxx8l9ex335q7he0f09aej04zpazpl0ne2cgukyawd24mayt8gpz4mhxue69uhk2er9dchxummnw3ezumrpdejqz8thwden5te0dehhxarj94c82c3wwajkcmr0wfjx2u3wdejhgqgcwaehxw309aex2mrp0yhxummnw3exzarf9e3k7mgnp0sh5
;;        nak decode nsec1jrmyhtjhgd9yqalps8hf9mayvd58852gtz66m7tqpacjedkp6kxq4dyxsr

;; OPTIONS:
;;    --id, -e      return just the event id, if applicable (default: false)
;;    --pubkey, -p  return just the pubkey, if applicable (default: false)
(defun nak-decode (subcommand &rest args)
  "Decode a nak SUBCOMMAND with the given ARGS."
  (nak-execute "decode"
               :args (cons subcommand args)))



;; Example usage:
;; (nak-req :kinds '(1)
;;          :limit 5
;;          :relays '("wss://relay.damus.io" "wss://nos.lol")
;;          :jq "."
;;          :callback (lambda (result) (message "Got result: %S" result)))

(provide 'nak-wrapper)
;; Query multiple relays
;; (nak-req :kinds '(1)
;;          :limit 5
;;          :relays '("wss://relay.damus.io" "wss://nos.lol")
;;          :jq "."
;;          :callback (lambda (result)
;;                     (message "Got result: %S" result)))

;; ;; Stream events from multiple relays
;; (nak-req :kinds '(1)
;;          :relays '("wss://relay.damus.io" "wss://nos.lol")
;;          :stream t
;;          :jq "."
;;          :callback (lambda (event)
;;                     (message "Got event: %S" event)))
;; Add this to your init.el or evaluate it before using IELM
