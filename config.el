;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; UI ;;;
(setq display-line-numbers-type 'relative)   ; set line number style
(setq confirm-kill-emacs nil)                ; disable quit prompt

;; Custom splash image
(setq fancy-splash-image (file-name-concat doom-user-dir "splash.png"))

;;; THEME ;;;
(setq doom-theme 'doom-one)      ; set doom theme

;;; FONT ;;;
;; Set font family
(setq doom-font (font-spec :family "Adwaita Mono" :size 15)) ; editor font

;; Custom styles for all themes
;; (custom-set-faces!
  ;; '(font-lock-keyword-face :slant italic :weight medium)
  ;; '(font-lock-comment-face :slant italic :weight normal))
;; '(italic :slant italic :weight medium))
;; '(tree-sitter-hl-face:property :slant italic :weight medium)
;; '(line-number-current-line :slant italic :weight medium))

;;; WINDOW ;;;
(add-to-list 'default-frame-alist '(fullscreen . maximized))   ; open emacs maximized

;; Automatically switch to newly created splits
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Transparency
;; (doom/set-frame-opacity 97)   ; add transparency
;; (set-frame-parameter nil 'alpha-background 97) ; for current frame
;; (add-to-list 'default-frame-alist '(alpha-background . 97)) ; for all new frames henceforth

;; Disable window decoration if using graphical session
;; (if (display-graphic-p)
;;   (setq default-frame-alist '((undecorated . t))) ; for current frame
;;   (add-to-list 'default-frame-alist '(undecorated . t))) ; for all new frames henceforth
;; (add-to-list 'default-frame-alist '(drag-internal-border . 1))    ; enable drag and resize for internal borders
;; (add-to-list 'default-frame-alist '(internal-border-width . 1))   ; set internal border size

;;; VIM RELATED ;;;
(setq evil-want-fine-undo 'fine   ; vim like undo
      evil-cross-lines t)         ; vim whichwrap

;; Scrolloff
;; (setq scroll-step 1)
;; (setq scroll-margin 8)

;; Function to add space from both sides inside braces
(defun my/c-mode-insert-space (arg)
  (interactive "*P")
  (let ((prev (char-before))
        (next (char-after)))
    (self-insert-command (prefix-numeric-value arg))
    (if (and prev next
             (string-match-p "[[({]" (string prev))
             (string-match-p "[])}]" (string next)))
        (save-excursion (self-insert-command 1)))))

(defun my/c-mode-delete-space (arg &optional killp)
  (interactive "*p\nP")
  (let ((prev (char-before))
        (next (char-after))
        (pprev (char-before (- (point) 1))))
    (if (and prev next pprev
             (char-equal prev ?\s) (char-equal next ?\s)
             (string-match "[[({]" (string pprev)))
        (delete-char 1))
    (backward-delete-char-untabify arg killp)))

;; Add space between brackets when 'SpaceBar' is pressed
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key " " 'my/c-mode-insert-space)
            (local-set-key "\177" 'my/c-mode-delete-space)))

;;; EVIL SNIPE ;;;
(setq evil-snipe-scope 'visible
      evil-snipe-repeat-scope 'whole-visible
      evil-snipe-spillover-scope 'whole-buffer)

;;; FLYCHECK ;;;
;; Check syntax on idle
(after! flycheck
  (setq flycheck-check-syntax-automatically '(idle-change)))

(setq +vc-gutter-default-style nil)                    ; Disable default fringe styling
(setq-default flycheck-indication-mode 'left-fringe)   ; Move flycheck to left margin

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

;;; NEOTREE ;;;
(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))

;; Function to autoclose neotree on file open
(defun neo-open-file-hide (full-path &optional arg)
  "Open a file node and hides tree."
  (neo-global--select-mru-window arg)
  (find-file full-path)
  (neotree-hide))

(defun neotree-enter-hide (&optional arg)
  "Enters file and hides neotree directly"
  (interactive "P")
  (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))

;; Close neotree when 'Enter' is pressed to open a file
(add-hook
 'neotree-mode-hook
 (lambda ()
   (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter-hide)))

;;; MODELINE ;;;
;; (setq doom-modeline-major-mode-icon t)            ; show major mode icon in doom modeline(filetype icon)
(setq lsp-modeline-code-actions-enable nil       ; disable code actions in doom modeline
	  doom-modeline-check-simple-format t)
;; (setq doom-modeline-modal-icon nil)               ; disable mode icon and show mode text
;; (setq doom-modeline-indent-info t)                ; show indent level

;;; WHITESPACE MODE ;;;
(global-whitespace-mode +1)                ; enable globally
(setq whitespace-style '(face trailing))   ; set style

;;; INDENT BARS ;;;
;; (setq highlight-indent-guides-auto-enabled nil)   ; fix color issues in catppuccin
;; (setq highlight-indent-guides-responsive 'top)   ; display different color for current context
;; (setq highlight-indent-guides-method 'column)

;;; TERMINAL ;;;
(setq shell-file-name "/bin/fish"
      vterm-max-scrollback 5000)
(setq eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

;;; LOAD USER DEFINED KEYBINDINGS ;;;
(load! "keybindings")
