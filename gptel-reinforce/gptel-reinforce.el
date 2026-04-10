;;; gptel-reinforce.el --- Reinforcement layer for text artifacts -*- lexical-binding: t; -*-

;; Author: Justin Silverman <justinsilverman@psu.edu>
;; Maintainer: Justin Silverman <justinsilverman@psu.edu>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (gptel "0"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; `gptel-reinforce' stores local feedback about items and generated outputs,
;; summarizes that feedback, and helps update text artifacts such as prompts,
;; templates, rules, and code snippets over time.

;;; Code:

(require 'gptel-reinforce-core)
(require 'gptel-reinforce-db)
(require 'gptel-reinforce-org)
(require 'gptel-reinforce-backend)
(require 'gptel-reinforce-ui)
(require 'gptel-reinforce-elfeed)

(provide 'gptel-reinforce)

;;; gptel-reinforce.el ends here
