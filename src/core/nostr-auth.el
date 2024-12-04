;; nostr-auth.el --- Nostr authentication management -*- lexical-binding: t; -*-

(require 'nak-wrapper)
(require 'jq-wrapper)

(defgroup nostr-auth nil
  "Settings for Nostr authentication."
  :group 'tools)

(defcustom nostr-auth-profile-dir
  (expand-file-name "nostr/profiles"
                    (or (getenv "XDG_CONFIG_HOME")
                        (expand-file-name "~/.config")))
  "Directory storing encrypted Nostr keys."
  :type 'string
  :group 'nostr-auth)

(defvar nostr-auth-debug t
  "When non-nil, print debug information about authentication.")

(defcustom nostr-auth-cache-timeout 3600
  "Number of seconds to cache the decrypted key in memory."
  :type 'integer
  :group 'nostr-auth)

;; Internal variables
(defvar nostr-auth--current-key nil
  "Currently active decrypted key.")

(defvar nostr-auth--pubkey nil
  "Current public key derived from private key.")

(defvar nostr-auth--current-profile nil
  "Currently loaded profile name.")

(defun nostr-auth--debug (format-string &rest args)
  "Print debug message when debug is enabled."
  (when nostr-auth-debug
    (apply #'message (concat "nostr-auth-debug: " format-string) args)))

(defun nostr-auth--format-keyfile (npub name)
  "Create profile filename from NPUB and NAME."
  (let* ((npub-prefix (substring npub 4 14)) ; Get first 10 chars after 'npub'
         (safe-name (replace-regexp-in-string "[^a-zA-Z0-9_-]" "-" name))
         (safe-name (downcase (replace-regexp-in-string "-+" "-" safe-name))))
    (expand-file-name (format "%s_%s.ncryptsec" npub-prefix safe-name)
                      nostr-auth-profile-dir)))

(defun nostr-auth-setup (hex-key name password)
  "Set up initial encrypted key from HEX-KEY using PASSWORD."
  (interactive
   (list
    (read-string "Enter hex private key: ")
    (read-string "Enter profile name: ")
    (read-passwd "Choose encryption password: " t)))

  (nostr-auth--debug "Setting up new encrypted key")

  ;; First verify the key is valid by deriving public key
  (nak-execute "key"
               :args (list "public" hex-key)
               :callback
               (lambda (pubkey)
                 (let ((clean-pubkey (string-trim pubkey)))
                   ;; Get npub for filename
                   (nak-execute "encode"
                                :args (list "npub" clean-pubkey)
                                :callback
                                (lambda (npub)
                                  (let ((clean-npub (string-trim npub)))
                                    ;; Now encrypt the key
                                    (nak-execute "key"
                                                 :args (list "encrypt" hex-key password)
                                                 :callback
                                                 (lambda (encrypted)
                                                   (let ((clean-encrypted (string-trim encrypted))
                                                         (keyfile (nostr-auth--format-keyfile clean-npub name)))
                                                     ;; Save to file
                                                     (make-directory nostr-auth-profile-dir t)
                                                     (with-temp-file keyfile
                                                       (insert clean-encrypted))
                                                     (setq nostr-auth--current-profile name)
                                                     (message "Key encrypted and saved successfully!")
                                                     (nostr-auth--debug "Saved encrypted key for pubkey %s" clean-pubkey)))))))))))

;; Rest of nostr-auth.el functions remain the same
(provide 'nostr-auth)
