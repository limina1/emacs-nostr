;; nostr-kb.el --- Publish AsciiDoc sections as Nostr events -*- lexical-binding: t; -*-

(require 'nostr-login)
(require 'nostr-core)
(require 'nak-wrapper)

(defvar nostr-kb-relays '("wss://wheat.happytavern.co" "wss://theforest.nostr1.com" "wss://nostr.happytavern.co")
  "Default relays for knowledge base publication.")

(defun nostr-kb--normalize-title (title)
  "Normalize section TITLE for use in d-tag."
  (downcase
   (replace-regexp-in-string
    "[^a-zA-Z0-9]+" "-"
    (string-trim title))))

(defun nostr-kb--extract-section ()
  "Extract current section title and content from buffer."
  (save-excursion
    (let* ((start (progn
                    (beginning-of-line)
                    (while (and (not (looking-at "^==\\s-+\\(.+\\)$"))
                                (not (bobp)))
                      (forward-line -1))
                    (point)))
           (title (when (looking-at "^==\\s-+\\(.+\\)$")
                    (match-string-no-properties 1)))
           (content-start (line-beginning-position 2))
           (content-end (save-excursion
                          (forward-line 1)
                          (while (and (not (looking-at "^==\\s-+"))
                                      (not (eobp)))
                            (forward-line 1))
                          (point)))
           (content (buffer-substring-no-properties content-start content-end)))
      (when title
        (list :title title
              :content (concat "== " title "\n" content))))))

(defun nostr-kb-publish-section ()
  "Publish current AsciiDoc section as a kind 30041 event."
  (interactive)
  (if-let* ((key (nostr-login-get-key))
            (section (nostr-kb--extract-section))
            (title (plist-get section :title))
            (content (plist-get section :content))
            (dtag (nostr-kb--normalize-title title)))
      (let ((args (list
                   "--sec" key
                   "-k" "30041"
                   "-c" content
                   "-t" (concat "title=" title)
                   "-t" (concat "d=" dtag))))
        (nak-execute "event"
                     :args (append args (mapcar (lambda (url) url) nostr-kb-relays))
                     :callback (lambda (output)
                                 (when (string-match "\"id\":\"\\([^\"]+\\)\"" output)
                                   (let ((event-id (match-string 1 output)))
                                     ;; Encode as nevent with relay hints
                                     (nak-execute "encode"
                                                  :args (append
                                                         (list "nevent" event-id)
                                                         (mapcar (lambda (relay)
                                                                   (concat "--relay=" relay))
                                                                 nostr-kb-relays))
                                                  :callback (lambda (nevent)
                                                              (message "Published as: %s" (string-trim nevent)))
                                                  :error-callback (lambda (err)
                                                                    (message "Error encoding event: %s" err)))))
                                 (message "Raw output: %s" output))
                     :error-callback (lambda (err)
                                       (message "Failed to publish section: %s" err))))
    (message "Could not extract section or not logged in")))

(provide 'nostr-kb)
