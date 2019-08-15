;; a simple major mode, vdf-mode

(defvar vdf-mode-syntax-table nil "Syntax table for `vdf-mode'.")


(setq vdf-mode-syntax-table
      (let ( (synTable (make-syntax-table)))
        (modify-syntax-entry ?\/ ". 12b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)
        (modify-syntax-entry ?{ "(}" synTable)
        (modify-syntax-entry ?} "){" synTable)
        synTable))

(setq vdf-highlights
      '(("#include\\|#base" . font-lock-preprocessor-face)
        ("\"\\([^\"]+?\\)\"" . (1 font-lock-constant-face))
        ))

(define-derived-mode vdf-mode fundamental-mode "vdf"
  "major mode for editing Valve VDF files"
  (setq font-lock-defaults '(vdf-highlights))
  (set-syntax-table vdf-mode-syntax-table)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (smie-setup smie-sample-grammar #'smie-sample-rules)
)
