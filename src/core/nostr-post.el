;; nostr-post.el --- Functions for creating Nostr posts with secure authentication -*- lexical-binding: t; -*-

(require 'nak-wrapper)
(require 'jq-wrapper)
(require 'nostr-login)

(defgroup nostr-post nil
  "Settings for Nostr post creation."
  :group 'tools)

(defvar nostr-post-debug t
  "When non-nil, print debug information about post creation.")

(defcustom nostr-post-default-relays '("wss://relay.damus.io" "wss://nos.lol")
  "Default relays to publish posts to."
  :type '(repeat string)
  :group 'nostr-post)

(defun nostr-post--debug (format-string &rest args)
  "Print debug message using FORMAT-STRING and ARGS when debug is enabled."
  (when nostr-post-debug
    (apply #'message (concat "nostr-post-debug: " format-string) args)))

(cl-defun nostr-create-post
    (&key content
          (kind 1)
          tags
          relays
          (callback nil)
          (error-callback nil))
  "Create a Nostr post with given parameters."
  (if-let ((key (nostr-login-get-key)))
      (progn
        (nostr-post--debug "Creating post with content: %s" content)
        (nostr-post--debug "Using relays: %S" (or relays nostr-post-default-relays))

        (let ((args (list "--sec" key)))
          (when kind
            (setq args (append args (list "-k" (number-to-string kind)))))

          (when content
            (setq args (append args (list "-c" content))))

          (when tags
            (dolist (tag tags)
              (setq args (append args (list "-t" tag)))))

          ;; Add relays at the end
          (setq args (append args (or relays nostr-post-default-relays)))

          (nostr-post--debug "Constructed arguments: %S" args)

          (nak-execute "event"
                       :args args
                       :callback
                       (lambda (output)
                         (nostr-post--debug "Raw event output: %s" output)
                         (condition-case err
                             (when (string-match "\"id\":\"\\([^\"]+\\)\"" output)
                               (let ((event-id (match-string 1 output)))
                                 (nostr-post--debug "Extracted event ID: %s" event-id)
                                 (nak-execute "encode"
                                              :args (list "nevent" event-id)
                                              :callback
                                              (lambda (encoded)
                                                (let ((clean-encoded (string-trim encoded)))
                                                  (nostr-post--debug "Encoded result: %s" clean-encoded)
                                                  (when callback
                                                    (funcall callback clean-encoded))))
                                              :error-callback error-callback)))
                           (error
                            (let ((err-msg (format "Failed to process event: %s" err)))
                              (nostr-post--debug "Error: %s" err-msg)
                              (when error-callback
                                (funcall error-callback err-msg))))))
                       :error-callback
                       (lambda (err)
                         (nostr-post--debug "Event creation error: %s" err)
                         (when error-callback
                           (funcall error-callback err))))))
    (message "No active login session. Please login first.")))

(defun nostr-create-post-interactive ()
  "Interactively create a Nostr post."
  (interactive)
  (unless (nostr-login-active-p)
    (call-interactively #'nostr-login-with-key))

  (when (nostr-login-active-p)
    (let ((buf (get-buffer-create "*Nostr Post Status*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert "Creating Nostr Post...\n\n"))
      (display-buffer buf)

      (let* ((content (read-string "Post content: "))
             (tags-input (read-string "Tags (comma-separated, e.g. t=nostr,p=pubkey): " nil nil ""))
             (tags (when (not (string-empty-p tags-input))
                     (split-string tags-input "," t "\\s-+")))
             (relays (when (y-or-n-p "Use custom relays? ")
                       (split-string
                        (read-string "Relay URLs (comma-separated): "
                                     (mapconcat #'identity nostr-post-default-relays ","))
                        "," t "\\s-+"))))

        ;; Update status buffer
        (with-current-buffer buf
          (insert (format "Content: %s\n" content))
          (when tags
            (insert (format "Tags: %S\n" tags)))
          (insert (format "Relays: %S\n\n" (or relays nostr-post-default-relays)))
          (insert "Sending to relays...\n"))

        ;; Create the post
        (nostr-create-post
         :content content
         :tags tags
         :relays relays
         :callback
         (lambda (encoded-id)
           (with-current-buffer buf
             (insert (format "\nSuccess! Post created with ID: %s\n" encoded-id))
             (insert "\nYou can view your post using this ID on any Nostr client.")))
         :error-callback
         (lambda (err)
           (with-current-buffer buf
             (insert (format "\nError creating post: %s\n" err)))))))))

(provide 'nostr-post)
;;; nostr-post.el ends here
