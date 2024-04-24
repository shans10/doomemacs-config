;;; UI ;;;
(setq display-line-numbers-type 'relative)   ; set line number style
(setq confirm-kill-emacs nil)                ; disable quit prompt

;;; THEME ;;;
;; (setq doom-theme 'adwaita-dark)        ; set external theme
;; (setq doom-theme 'doom-nord-aurora)      ; set doom theme

;; Catppuccin settings
;; (setq catppuccin-flavor 'mocha
;;       catppuccin-highlight-matches t
;;       catppuccin-italic-comments t)

;; Custom styles for catppuccin theme
;; (custom-theme-set-faces! 'catppuccin
;;   '(lsp-face-highlight-textual :background "#45475a" :foreground "#cdd6f4" :weight bold)
;;   '(lsp-face-highlight-read  :inherit 'lsp-face-highlight-textual)
;;   '(lsp-face-highlight-write :inherit 'lsp-face-highlight-textual))

;; Custom styles for adwaita-dark theme
;; (custom-theme-set-faces! 'adwaita-dark
;;   '(font-lock-keyword-face :foreground "#ffa348")
;;   '(show-paren-match :foreground "#ffa348" :weight ultra-bold)
;;   '(region :background "#21364A")
;;   '(line-number-current-line :slant italic :weight bold)
;;   '(doom-modeline-evil-insert-state :foreground "#FFA348" :weight normal))

;;; FONT ;;;
;; Set font family
(setq doom-font (font-spec :family "JetBrainsMono NF" :size 15)) ; editor font

;; Custom styles for all themes
(custom-set-faces!
  ;; '(font-lock-keyword-face :slant italic :weight medium)
  '(font-lock-comment-face :slant italic :weight normal))
;; '(italic :slant italic :weight medium))
;; '(tree-sitter-hl-face:property :slant italic :weight medium)
;; '(line-number-current-line :slant italic :weight medium))

;;; WINDOW ;;;
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))   ; open emacs maximized

;; Automatically switch to newly created splits
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Transparency
;; (doom/set-frame-opacity 97)   ; add transparency
;; (set-frame-parameter nil 'alpha-background 97) ; for current frame
;; (add-to-list 'default-frame-alist '(alpha-background . 97)) ; for all new frames henceforth

;; Disable window decoation if using graphical session
;; (if (display-graphic-p)
;;   (setq default-frame-alist '((undecorated . t))) ; for current frame
;;   (add-to-list 'default-frame-alist '(undecorated . t))) ; for all new frames henceforth
;; (add-to-list 'default-frame-alist '(drag-internal-border . 1))    ; enable drag and resize for internal borders
;; (add-to-list 'default-frame-alist '(internal-border-width . 1))   ; set internal border size

;;; VIM RELATED ;;;
(setq evil-want-fine-undo 'fine   ; vim like undo
      evil-cross-lines t)         ; vim whichwrap

;; Scrolloff
(setq scroll-step 1)
(setq scroll-margin 8)

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
(setq lsp-ui-doc-enable nil                   ; disable doc hover information unless key pressed
      lsp-ui-sideline-show-code-actions nil   ; disable code action hints in sideline
      lsp-eldoc-enable-hover nil              ; disable doc below modeline on hover
      lsp-signature-auto-activate nil)        ; disable function signature help popup

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
(setq doom-modeline-major-mode-icon t)            ; show major mode icon in doom modeline(filetype icon)
(setq lsp-modeline-code-actions-enable nil)       ; disable code actions in doom modeline
;; (setq doom-modeline-modal-icon nil)               ; disable mode icon and show mode text
;; (setq doom-modeline-indent-info t)                ; show indent level

;;; WHITESPACE MODE ;;;
(global-whitespace-mode +1)                ; enable globally
(setq whitespace-style '(face trailing))   ; set style

;;; HIGHLIGHT INDENT GUIDES ;;;
;; (setq highlight-indent-guides-auto-enabled nil)   ; fix color issues in catppuccin
(setq highlight-indent-guides-responsive 'top)   ; display different color for current context
(setq highlight-indent-guides-method 'column)

;;; TERMINAL ;;;
(setq shell-file-name "/bin/fish"
      vterm-max-scrollback 5000)
(setq eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

;;; TREESITTER ;;;
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode) ; enable tree-sitter highlights

;;; EXTERNAL TERMINAL ;;;
;; Open specified terminal in current working directory
(defun term-here ()
  (interactive)
  (start-process "" nil "alacritty"))

;;; TERMINAL CURSOR ;;;
;; Change cursor in terminal emacs based on mode
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate)) ; or (etcc-on)

;;; ZOOM WINDOW ;;;
(setq zoom-window-mode-line-color nil) ; disable modeline color

;;; INDENT GUIDE ;;;
;; (indent-guide-global-mode)
;; (setq indent-guide-recursive t)

;;; LOAD USER DEFINED KEYBINDINGS ;;;
(load! "keybindings")
