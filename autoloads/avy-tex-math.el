;;; avy-tex-math.el --- functions to jump to latex math using avy and/or evil -*- lexical-binding: t -*-

(defvar latex-math-start-regexp
  "\\(\\\\begin{\\(equation\\*?\\|align\\*?\\|split\\*?\\)}\\)\\|\\(\\\\\\[\\|\\\\(\\|\\$\\$?[^\\$]+\\$\\)"
  "Regular expression string that matches beginning of latex math.")

;;;###autoload
(defun jds/latex-next-math-start ()
    "Move point to start of next latex math element."
  (interactive)
  (if (re-search-forward latex-math-start-regexp nil t)
      nil
    (message "No matching math in that direction.")))

;;;###autoload
(defun jds/latex-previous-math-start ()
  "Move point to start of next latex math element."
  (interactive)
  (if (re-search-backward latex-math-start-regexp nil t)
      nil
    (message "No matching math in that direction.")))

;;;###autoload
(defun jds/avy-latex-math ()
  "Use avy to jump to LaTeX math environments in the current buffer."
  (interactive)
  (let ((avy-all-windows nil))
    (avy-with latex-math-avy
      (avy-jump latex-math-start-regexp))))
