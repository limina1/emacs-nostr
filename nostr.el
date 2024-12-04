;; In your packages.el
(package! nostr)

;; In your config.el
(use-package nostr
  :init
  (nostr-initialize)
  :config
  (setq nostr-profile-dir "~/.nostr/profiles"))
