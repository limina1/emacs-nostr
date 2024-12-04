;;; config.el --- Configuration options for nostr-emacs -*- lexical-binding: t; -*-

(defgroup nostr-emacs nil
  "Nostr client for Emacs."
  :group 'tools)

(defcustom nostr-jq-executable "jq"
  "Path to the jq executable."
  :type 'string
  :group 'nostr-emacs)

(defcustom nostr-nak-executable "nak"
  "Path to the nak executable."
  :type 'string
  :group 'nostr-emacs)

(defcustom nostr-default-test-relays
  '("wss://relay.damus.io" "wss://nos.lol" "wss://relay.nostr.band" "wss://nos.lol")
  "Default test relays."
  :type '(repeat string)
  :group 'nostr-emacs
  )
