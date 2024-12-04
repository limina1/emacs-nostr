;; nostr-profile.el --- Nostr profile creation and management -*- lexical-binding: t; -*-

(require 'nak-wrapper)
(require 'json)
(require 'seq)

(defgroup nostr-profile nil
  "Settings for Nostr profile management."
  :group 'tools)

(defvar nostr-profile-debug t
  "When non-nil, print debug information about profile operations.")

(defcustom nostr-profile-dir (expand-file-name "nostr"
                                               (or (getenv "XDG_CONFIG_HOME")
                                                   (expand-file-name "~/.config")))
  "Directory for storing Nostr profile keys."
  :type 'string
  :group 'nostr-profile)

(defcustom nostr-profile-relays
  '(("wss://relay.damus.io" . t)
    ("wss://nos.lol" . t)
    ("wss://nostr.mom" . t)
    ("wss://relay.nostr.band" . nil)
    ("wss://nostr.oxtr.dev" . nil))
  "Alist of (relay . enabled) pairs for profile operations.
Only relays marked as enabled (t) will be used by default."
  :type '(alist :key-type string :value-type boolean)
  :group 'nostr-profile)

;; Debug helper
(defun nostr-profile--debug (format-string &rest args)
  "Print debug message when debug is enabled."
  (when nostr-profile-debug
    (apply #'message (concat "nostr-profile-debug: " format-string) args)))

;; Relay management functions
(defun nostr-profile-enabled-relays ()
  "Get list of currently enabled relay URLs."
  (mapcar #'car (seq-filter #'cdr nostr-profile-relays)))

(defun nostr-profile-toggle-relay (relay)
  "Toggle enabled status for RELAY."
  (interactive
   (list (completing-read "Relay: "
                          (mapcar #'car nostr-profile-relays)
                          nil t)))
  (let ((cell (assoc relay nostr-profile-relays)))
    (when cell
      (setcdr cell (not (cdr cell)))
      (message "Relay %s is now %s"
               relay
               (if (cdr cell) "enabled" "disabled")))))

(defun nostr-profile-list-relays ()
  "Display current relay configuration."
  (interactive)
  (with-current-buffer (get-buffer-create "*Nostr Relays*")
    (erase-buffer)
    (insert "Nostr Relay Configuration\n")
    (insert "=======================\n\n")
    (dolist (relay nostr-profile-relays)
      (insert (format "[%c] %s\n"
                      (if (cdr relay) ?✓ ?✗)
                      (car relay))))
    (insert "\nUse M-x nostr-profile-toggle-relay to enable/disable relays\n")
    (display-buffer (current-buffer))))

;; Helper functions
(defun nostr-profile--format-filename (npub name)
  "Create profile filename from NPUB and NAME."
  (let* ((npub-prefix (substring npub 4 14)) ; Get first 10 chars after 'npub'
         (safe-name (replace-regexp-in-string "[^a-zA-Z0-9_-]" "-" name))
         (safe-name (downcase (replace-regexp-in-string "-+" "-" safe-name))))
    (format "%s_%s.ncryptsec" npub-prefix safe-name)))

(defun nostr-profile--ensure-dir ()
  "Ensure the profile directory exists."
  (unless (file-exists-p nostr-profile-dir)
    (make-directory nostr-profile-dir t)))

(defun nostr-profile--publish-event (hex-key content kind output-buf callback)
  "Publish event with HEX-KEY, CONTENT, and KIND to enabled relays.
Updates OUTPUT-BUF with progress and calls CALLBACK on success."
  (let ((event-args (list
                     "--sec" hex-key
                     "--nevent"        ; Request nevent code
                     "--auth"          ; Enable auth
                     "-k" (number-to-string kind)
                     "-c" content))
        (relays (nostr-profile-enabled-relays)))

    ;; Add relays to args
    (setq event-args (append event-args relays))

    (when (buffer-live-p output-buf)
      (with-current-buffer output-buf
        (insert "\nAttempting to publish with args:\n")
        (insert (format "%S\n\n" event-args))))

    (nak-execute
     "event"
     :args event-args
     :callback (lambda (result)
                 (when (buffer-live-p output-buf)
                   (with-current-buffer output-buf
                     (insert (format "\nEvent result: %s\n" result)))
                   (funcall callback result)))
     :error-callback
     (lambda (err)
       (when (buffer-live-p output-buf)
         (with-current-buffer output-buf
           (insert (format "\nError during publish: %s\n" err))))))))

;; Profile creation
(defun nostr-profile--handle-profile-creation (hex-key pub-key clean-npub output-buf)
  "Handle the interactive profile creation process after key generation."
  (let* ((name (read-string "Display name: "))
         (about (read-string "About: "))
         (picture (read-string "Picture URL (optional): "))
         (profile-data (make-hash-table :test 'equal))
         (encryption-password (read-passwd "Choose password for key encryption: " t)))

    ;; Build profile
    (puthash "name" name profile-data)
    (puthash "about" about profile-data)
    (when (not (string-empty-p picture))
      (puthash "picture" picture profile-data))

    (let ((profile-json (json-encode profile-data)))
      (nostr-profile--debug "Profile data: %s" profile-json)

      ;; Encrypt the key
      (nak-execute
       "key"
       :args (list "encrypt" hex-key encryption-password)
       :callback
       (lambda (encrypted-key)
         (let* ((clean-encrypted (string-trim encrypted-key))
                (filename (nostr-profile--format-filename clean-npub name))
                (keyfile (expand-file-name filename nostr-profile-dir)))

           ;; Save encrypted key
           (nostr-profile--ensure-dir)
           (with-temp-file keyfile
             (insert clean-encrypted))

           (nostr-profile--debug "Saved encrypted key to %s" keyfile)

           ;; Publish profile
           (nostr-profile--publish-event
            hex-key
            profile-json
            0
            output-buf
            (lambda (event-output)
              (nostr-profile--handle-publish-result
               event-output pub-key output-buf)))))))))

(defun nostr-profile--handle-publish-result (event-output pub-key output-buf)
  "Handle the result of profile publication attempt."
  (nostr-profile--debug "Event output: %s" event-output)

  (let ((success nil))
    ;; Check for successful publish
    (with-current-buffer output-buf
      (cond
       ((string-match "publishing to \\(.*?\\)\\.\\.\\. success" event-output)
        (setq success t)
        (insert "\nSuccessfully published profile!\n"))
       ((string-match "publishing to \\(.*?\\)\\.\\.\\. failed: \\(.*?\\)\\(\n\\|$\\)"
                      event-output)
        (let ((error-msg (match-string 2 event-output)))
          (insert (format "\nFailed to publish: %s\n" error-msg))))
       (t
        (insert "\nUnknown response from relay\n"))))

    ;; If successful, create nprofile
    (when success
      (when (string-match "\"id\":\"\\([^\"]+\\)\"" event-output)
        (let* ((event-id (match-string 1 event-output))
               (nprofile-args
                (append
                 (list "nprofile" pub-key)
                 (mapcar (lambda (relay)
                           (concat "--relay=" relay))
                         (nostr-profile-enabled-relays)))))

          (nostr-profile--debug "Event ID: %s" event-id)

          (nak-execute
           "encode"
           :args nprofile-args
           :callback
           (lambda (nprofile)
             (with-current-buffer output-buf
               (insert (format "\nProfile identifier: %s\n\n"
                               (string-trim nprofile)))
               (insert "IMPORTANT: Backup your encrypted key and password!\n")
               (goto-char (point-max))))))))))

(defun nostr-create-new-profile ()
  "Interactively create a new Nostr profile with encrypted key storage."
  (interactive)
  (let ((buf (get-buffer-create "*Nostr Profile Creation*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Creating new Nostr profile...\n\n")
      (goto-char (point-max)))
    (display-buffer buf)

    ;; Generate new key
    (nostr-profile--debug "Generating new key...")
    (nak-execute
     "key"
     :args (list "generate")
     :callback
     (lambda (private-key)
       (let ((hex-key (string-trim private-key)))
         (nostr-profile--debug "Generated key: %s" hex-key)
         (with-current-buffer buf
           (insert (format "Private Key (hex): %s\n" hex-key))
           (goto-char (point-max)))

         ;; Get public key info
         (nak-execute
          "key"
          :args (list "public" hex-key)
          :callback
          (lambda (public-key)
            (let ((pub-key (string-trim public-key)))
              (nostr-profile--debug "Public key: %s" pub-key)

              ;; Get npub
              (nak-execute
               "encode"
               :args (list "npub" pub-key)
               :callback
               (lambda (npub)
                 (let ((clean-npub (string-trim npub)))
                   (with-current-buffer buf
                     (insert (format "Public Key (hex): %s\n" pub-key))
                     (insert (format "Public Key (npub): %s\n\n" clean-npub))
                     (goto-char (point-max)))

                   ;; Continue with profile creation
                   (nostr-profile--handle-profile-creation
                    hex-key pub-key clean-npub buf))))))))))))

(defun nostr-profile-enter-nsec-key ()
  "Prompt the user to enter an nsec private key."
  (interactive)
  (let ((nsec-key (read-string "Enter nsec private key: ")))
    (nostr-profile--handle-nsec-to-hex nsec-key)))
(defun nostr-profile--handle-nsec-to-hex (nsec-key)
  "Convert NSEC-KEY to a hex private key."
  (nak-execute "encode"
               :args (list "nsec" nsec-key)
               :callback
               (lambda (hex-key)
                 (nostr-profile--handle-hex-to-ncryptsec (string-trim hex-key)))))
(defun nostr-profile--handle-hex-to-ncryptsec (hex-key)
  "Encrypt HEX-KEY to ncryptsec format."
  (let ((encryption-password (read-passwd "Choose password for key encryption: " t)))
    (nak-execute "key"
                 :args (list "encrypt" hex-key encryption-password)
                 :callback
                 (lambda (ncryptsec-key)
                   (nostr-profile--handle-profile-creation
                    (string-trim ncryptsec-key)
                    hex-key
                    (nostr-profile--get-npub-from-hex hex-key)
                    (get-buffer-create "*Nostr Profile Creation*"))))))
(provide 'nostr-profile)
;;; nostr-profile.el ends here
