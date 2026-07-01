;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; UI ;;;
(setq display-line-numbers-type 'relative)   ; set line number style
(setq confirm-kill-emacs nil)                ; disable quit prompt
(setq indent-tabs-mode nil)                  ; use spaces for Tab indentation
(setq-default markdown-hide-markup t)        ; hide markdown markup syntax

;;; THEME ;;;
;; (setq doom-theme 'doom-one)      ; set doom theme
(setq doom-theme 'catppuccin)

;;; FONT ;;;
;; Set font family
(setq doom-font (font-spec :family "Monaspace Neon Frozen" :size 15)) ; editor font

;; Automatically switch to newly created splits
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;;; VIM RELATED ;;;
(setq evil-want-fine-undo 'fine   ; vim like undo
      evil-cross-lines t)         ; vim whichwrap

;; Scrolloff
(setq scroll-step 1)
(setq scroll-margin 8)

;;; EVIL SNIPE ;;;
(setq evil-snipe-scope 'visible
      evil-snipe-repeat-scope 'whole-visible
      evil-snipe-spillover-scope 'whole-buffer)

;;; FLYCHECK ;;;
;; Check syntax on idle
(after! flycheck
  (setq flycheck-check-syntax-automatically '(idle-change))
  (setq flycheck-idle-change-delay 0.5))

;; (setq +vc-gutter-default-style nil)                    ; Disable default fringe styling
;; (setq-default flycheck-indication-mode 'left-fringe)   ; Move flycheck to left margin

;;; LSP ;;;
;; Disable invasive lsp-mode features
(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil
        ;; If an LSP server isn't present when I start a prog-mode buffer, you
        ;; don't need to tell me. I know. On some machines I don't care to have
        ;; a whole development environment for some ecosystems.
        lsp-enable-suggest-server-download nil))
(after! lsp-ui
  (setq lsp-ui-sideline-enable nil          ; no more useful than flycheck
        lsp-ui-doc-enable nil               ; redundant with K
        lsp-eldoc-enable-hover nil          ; disable doc below modeline on hover
        lsp-signature-auto-activate nil))   ; disable function signature help popup

;; Formatting
(setq-hook! 'js-mode-hook +format-with-lsp nil)

;;; DIRVISH ;;;
;; Function to close dirvish after file is opened
(defun my/dirvish-side-open-and-quit ()
  "Open the file at point and close the dirvish-side window."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (dirvish-side) ;; This toggles the sidebar closed
    (find-file file)))

;; Map the above function to 'Enter' key to close dirvish on file open
(map! :after dirvish
      :map dirvish-mode-map
      ;; :n "RET" #'my/dirvish-side-open-and-quit
      :n "<right>" #'my/dirvish-side-open-and-quit)
      ;; :n [return] #'my/dirvish-side-open-and-quit)

;;; MODELINE ;;;
;; Simple doom modeline flycheck format
(after! doom-modeline
  (setq doom-modeline-check-simple-format t))

(setq lsp-modeline-code-actions-enable nil)   ; disable code actions in doom modeline
(setq doom-modeline-indent-info nil           ; show indent level
      doom-modeline-lsp t)                    ; show lsp status

;;; WHITESPACE MODE ;;;
(global-whitespace-mode +1)                ; enable globally
(setq whitespace-style '(face trailing))   ; set style

;;; INDENT BARS ;;;
;; Highlight current context with different colour
(after! indent-bars
   (setq indent-bars-pattern "."
         indent-bars-width-frac 0.1
         indent-bars-pad-frac 0.25
         indent-bars-color-by-depth nil
         indent-bars-display-on-blank-lines 'least
         indent-bars-no-descend-lists t))
         ;; indent-bars-highlight-current-depth '(:face default :blend 0.35)))

;;; TERMINAL ;;;
(setq shell-file-name "/bin/fish"
      vterm-max-scrollback 5000)
(setq eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

;; Disable eshell completion
(defun my-eshell-remove-pcomplete ()
  (remove-hook 'completion-at-point-functions #'pcomplete-completions-at-point t))

;; Add the function to the eshell-mode-hook to run when Eshell starts
(add-hook 'eshell-mode-hook #'my-eshell-remove-pcomplete)

;; Kdl file support
(use-package! kdl-mode
  :mode "\\.kdl\\'")

;;; LOAD USER DEFINED KEYBINDINGS ;;;
(load! "keybindings")
