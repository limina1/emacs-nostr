;;; tools/nostr/nostr-emacs.el -*- lexical-binding: t; -*-
;; Copyright (C) 2024


;; Author: limina1
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: nostr, comm
;; URL: https://github.com/limina1/nostr-emacs

;; Commentary:
;; A nostr client for Emacs, focused on event navigation and publishing
;; Requires jq and nak command-line tools to be installed.


;;; Code:

(require 'json)

;; Load configuration
(require 'nostr-config)

;; Load core functionality
(require 'nostr-core)
(require 'nostr-protocol)

(provide 'nostr-emacs)
;;; nostr-emacs.el ends here
