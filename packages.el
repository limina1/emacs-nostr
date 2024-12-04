;; packages.el --- Package installation for nostr-emacs -*- lexical-binding: t; -*-

(package! nostr
  :recipe (:host github
           :repo "yourusername/nostr-emacs"
           :files ("*.el"
                   "src/content/*.el"
                   "src/core/*.el"
                   "src/identity/*.el"
                   "src/network/*.el"
                   "src/ui/*.el"
                   "src/util/*.el")))

(unpin! nostr)
