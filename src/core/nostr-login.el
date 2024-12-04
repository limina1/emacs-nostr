;; nostr-login.el --- Nostr login and session management -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides unified login functionality and session management for Nostr
;; client, integrating profile management and authentication.

;;; Code:

(let ((current-dir (cond
                    ;; When loading from file
                    (load-file-name (file-name-directory load-file-name))
                    ;; When evaluating buffer
                    (buffer-file-name (file-name-directory buffer-file-name))
                    ;; Fallback for direct evaluation
                    (t (expand-file-name "~/.config/doom/modules/tools/nostr-emacs/src/core/")))))
  ;; Add util directory to load-path
  (add-to-list 'load-path
               (expand-file-name "../util" current-dir))
  ;; Alternative: direct path resolution
  (add-to-list 'load-path
               (expand-file-name "~/.config/doom/modules/tools/nostr-emacs/src/util")))
(add-to-list 'load-path
             (expand-file-name "~/.config/doom/modules/tools/nostr-emacs/src/core"))

(require 'nak-wrapper)
(require 'jq-wrapper)
(require 'nostr-core)
(require 'nostr-auth)
(require 'nostr-profile)

(defgroup nostr-login nil
  "Settings for Nostr login management."
  :group 'nostr)

(defcustom nostr-login-session-timeout 3600
  "Number of seconds before requiring re-authentication."
  :type 'integer
  :group 'nostr-login)

;; Session state management
(defvar nostr-login--session-data nil
  "Current session data including login state and metadata.")

(cl-defstruct nostr-session
  "Structure holding current session information."
  key              ; Current private key (hex)
  pubkey           ; Public key (hex)
  profile          ; Profile data
  created-at       ; Session creation timestamp
  last-active)     ; Last activity timestamp

;; Core login functions


;; Update nostr-login-with-key to handle nsec

(defun nostr-login-with-key (key-input password)
  "Log in using KEY-INPUT (hex, nsec, or ncryptsec) with PASSWORD."
  (interactive
   (list
    (read-string "Enter key (hex/nsec/ncryptsec) or path: ")
    (read-passwd "Enter key password: ")))

  (cond
   ;; Handle direct ncryptsec input
   ((string-prefix-p "ncryptsec" key-input)
    (nak-execute "key"
                 :args (list "decrypt" key-input password)
                 :callback #'nostr-login--handle-decrypted-key
                 :error-callback (lambda (err)
                                   (message "Error decrypting ncryptsec: %s" err))))

   ;; Handle key file path
   ((file-exists-p key-input)
    (let ((encrypted-key (with-temp-buffer
                           (insert-file-contents key-input)
                           (string-trim (buffer-string)))))
      (nak-execute "key"
                   :args (list "decrypt" encrypted-key password)
                   :callback #'nostr-login--handle-decrypted-key
                   :error-callback (lambda (err)
                                     (message "Error decrypting key file: %s" err)))))

   ;; Handle nsec input - convert to hex first
   ((string-prefix-p "nsec" key-input)
    (nak-execute "decode"
                 :args (list key-input)
                 :callback (lambda (hex-key)
                             (let ((clean-hex (string-trim hex-key)))
                               ;; Now encrypt with password
                               (nak-execute "key"
                                            :args (list "encrypt" clean-hex password)
                                            :callback (lambda (ncryptsec)
                                                        ;; Finally decrypt with password
                                                        (nak-execute "key"
                                                                     :args (list "decrypt" ncryptsec password)
                                                                     :callback #'nostr-login--handle-decrypted-key
                                                                     :error-callback (lambda (err)
                                                                                       (message "Error in final decryption: %s" err)))))))
                 :error-callback (lambda (err)
                                   (message "Error decoding nsec: %s" err))))

   ;; Assume hex input
   (t (let ((clean-hex (string-trim key-input)))
        (nak-execute "key"
                     :args (list "encrypt" clean-hex password)
                     :callback (lambda (ncryptsec)
                                 (nak-execute "key"
                                              :args (list "decrypt" ncryptsec password)
                                              :callback #'nostr-login--handle-decrypted-key
                                              :error-callback (lambda (err)
                                                                (message "Error decrypting hex: %s" err))))
                     :error-callback (lambda (err)
                                       (message "Error encrypting hex: %s" err)))))))
(defun nostr-login--handle-decrypted-key (hex-key)
  "Handle decrypted HEX-KEY and initialize session."
  (when hex-key
    (let ((clean-key (string-trim hex-key)))
      ;; Get public key
      (nak-execute "key"
                   :args (list "public" clean-key)
                   :callback
                   (lambda (pubkey)
                     (let ((clean-pubkey (string-trim pubkey)))
                       ;; Initialize session
                       (setq nostr-login--session-data
                             (make-nostr-session
                              :key clean-key
                              :pubkey clean-pubkey
                              :created-at (float-time)
                              :last-active (float-time)))
                       ;; Fetch profile data
                       (nostr-login--fetch-profile clean-pubkey)))))))
(defun nostr-login--fetch-profile (pubkey)
  "Fetch profile data for PUBKEY from enabled relays."
  (let ((relays (nostr-profile-enabled-relays)))
    (nak-execute
     "req"
     :args
     (append
      (list "-k" "0"      ; kind 0 = metadata
            "-a" pubkey
            "-l" "1")     ; limit 1
      relays)
     :callback #'nostr-login--handle-profile-data)))

(defun nostr-login--handle-profile-data (profile-json)
  "Process PROFILE-JSON and update session data."
  (condition-case err
      (let* ((data (json-read-from-string profile-json))
             (content (alist-get 'content data))
             (profile (json-read-from-string content)))
        (setf (nostr-session-profile nostr-login--session-data) profile)
        (message "Logged in as %s" (alist-get 'name profile)))
    (error
     (message "Error processing profile: %s" err))))

(defun nostr-login-status ()
  "Display current login status."
  (interactive)
  (if nostr-login--session-data
      (with-current-buffer (get-buffer-create "*Nostr Login Status*")
        (let ((inhibit-read-only t)
              (session nostr-login--session-data))
          (erase-buffer)
          (insert "Nostr Login Status\n")
          (insert "=================\n\n")
          (insert (format "Public Key: %s\n" (nostr-session-pubkey session)))
          (when-let ((profile (nostr-session-profile session)))
            (insert (format "Name: %s\n" (alist-get 'name profile)))
            (insert (format "About: %s\n" (alist-get 'about profile))))
          (insert (format "\nSession created: %s"
                          (format-time-string "%Y-%m-%d %H:%M:%S"
                                              (nostr-session-created-at session))))
          (display-buffer (current-buffer))))
    (message "Not logged in")))

(defun nostr-login-active-p ()
  "Check if there is an active login session."
  (when nostr-login--session-data
    (let ((now (float-time))
          (last-active (nostr-session-last-active nostr-login--session-data)))
      (when (< (- now last-active) nostr-login-session-timeout)
        t))))

(defun nostr-login-touch ()
  "Update last activity timestamp."
  (when nostr-login--session-data
    (setf (nostr-session-last-active nostr-login--session-data)
          (float-time))))

(defun nostr-login-get-key ()
  "Get current private key, ensuring active session."
  (when (nostr-login-active-p)
    (nostr-session-key nostr-login--session-data)))

(defun nostr-login-get-pubkey ()
  "Get current public key if session is active."
  (when (nostr-login-active-p)
    (nostr-session-pubkey nostr-login--session-data)))

(defun nostr-logout ()
  "Clear current login session."
  (interactive)
  (setq nostr-login--session-data nil)
  (message "Logged out"))

;; Integration helpers

(defun nostr-login-ensure ()
  "Ensure active login session, prompting if needed."
  (unless (nostr-login-active-p)
    (call-interactively #'nostr-login-with-key))
  (nostr-login-active-p))

(provide 'nostr-login)
;;; nostr-login.el ends here
