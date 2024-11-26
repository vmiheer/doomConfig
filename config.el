;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 's)
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Miheer Vaidya"
      user-mail-address "m.vaidya@utah.edu")

(setq mac-command-modifier 'control)

(require 's)
(require 'f)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(if (eq system-type 'darwin)
    (setq doom-font (font-spec :family "monaco" :size 12))
  (setq doom-font (font-spec :family "Victor Mono" :size 20 :weight 'Medium)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with he
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (if (not (directory-empty-p "~/box/org"))
    "~/box/org"
  "~/org"
  ))

(if (not (eq system-type 'darwin))
    (use-package! darkman
      :config (setq darkman-themes '(:light doom-acario-light :dark doom-acario-dark)))
  t)

;; (use-package! zig
;;   :mode ("\\.csl\\'" . zig-mode))

(use-package! evil-vars
  :config (add-to-list 'evil-emacs-state-modes 'org-agenda-mode))

(use-package! org-pomodoro
  :init (add-hook 'org-pomodoro-finished-hook
                  (lambda () (progn
                          (shell-command "powershell -C \"New-BurntToastNotification  -Text DonePomodoro\"")
                          (save-excursion
                            (org-clock-goto)
                            (save-buffer))
                          )))
        (add-hook 'org-pomodoro-started-hook
                  (lambda () (shell-command "powershell -C \"New-BurntToastNotification  -Text StartedPomodoro\""))))

(use-package! markdown-mode
  :config (setq fill-column 80)
  :init (add-hook 'gfm-mode-hook #'(lambda ()
                                    (progn
                                      (auto-fill-mode 1)
                                      (flyspell-mode)
                                      )
                                    ))
  )

(use-package! org
  :config (setq fill-column 80)
  :init (add-hook 'org-mode-hook #'(lambda ()
                                    (progn
                                      (auto-fill-mode 1)
                                      (flyspell-mode)
                                      )
                                    ))
  )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq enable-local-variables t)


(make-variable-buffer-local 'compile-command)

(map! :leader
      (:prefix ("t" . "toggle")
        :desc "Toggle git gutter" "z" #'git-gutter:toggle))

(map! :leader
      (:prefix ("t" . "toggle")
        :desc "Toggle mouse support in term mode" "m" #'xterm-mouse-mode))

(setq-hook! python-mode
  flycheck-checker 'python-ruff)
(setq-hook! python-mode
  apheleia-formatter 'ruff)

(use-package! git-link
  :config (setq git-link-use-commit t))

;; https://gist.github.com/vmiheer/ee9de0a971af3e22520b64442331700f
(windmove-default-keybindings)

(use-package! tramp
  :config
  (dolist (elem '("~/.local/bin" "~/.emacs.d/.local/etc/lsp/clangd/clangd_12.0.0/bin"))
    (add-to-list 'tramp-remote-path elem
                 )
    )
  )

(use-package! lsp
  :config
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                     :major-modes '(c++-mode)
                     :remote? t
                     :server-id 'clangd-remote)))

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;; ran in https://github.com/emacs-lsp/lsp-metals/issues/84#issuecomment-1293699837
(use-package! treemacs
  :config
  (require 'treemacs-extensions)
  )
(use-package! lsp
  :config
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                     :major-modes '(c++-mode)
                     :remote? t
                     :server-id 'clangd-remote)))

(use-package! evil-string-inflection
  :config
  (map! :leader :desc "Toggle case" "t t" #'string-inflection-cycle))

(require 'mlir-lsp-client)
(use-package! mlir-mode
  :mode ("\\.td\\'" . tablegen-mode)
  :config (progn
            (setq lsp-mlir-server-executable "/home/mvaidya/source/repos/MLIR_Workspace/llvm-project/build/bin/mlir-lsp-server")
            (add-hook! 'mlir-mode-hook '(lambda () (progn (lsp-mlir-setup) (lsp))))))

(use-package! pyvenv
  :config
  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

;;
;; (use-package org-ref-ivy
;;   :ensure nil
;;   :load-path (lambda () (expand-file-name "org-ref" scimax-dir))
;;   :init (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
;; 	      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
;; 	      org-ref-insert-label-function 'org-ref-insert-label-link
;; 	      org-ref-insert-ref-function 'org-ref-insert-ref-link
;; 	      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; accept completion from copilot and fallback to company
(use-package! copilot
  :config (setq copilot-node-executable "/uufs/chpc.utah.edu/common/home/u1290058/.local/bin/node")
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(setq mv/ssh-mount-prefix "file:////Users/miheerv/mnt/chpcScratch")

(defun get-excel-path()
  (interactive)
  (message
   (if (boundp 'mv/ssh-mount-prefix)
       (s-replace "/scratch/general/vast/u1290058" mv/ssh-mount-prefix (dired-get-filename))
       (s-replace "/scratch/general/vast/u1290058" "d:/" (dired-get-filename))
       )))
(global-set-key (kbd "M-<backspace>") 'doom/delete-backward-word)
(defun mv/print-as-hex()
  (interactive)
  (message
   (format "%x" (string-to-number (buffer-substring (region-beginning) (region-end))))))

(setq lsp-json-schemas `[(:fileMatch ["CMakePresets.json" "CMakeUserPresets.json"] :url  "https://cmake.org/cmake/help/latest/_downloads/3e2d73bff478d88a7de0de736ba5e361/schema.json")])

(set-file-template! "/CMakePresets\\.json$" :trigger "__CMakePresets.json" :mode 'json-mode)
(set-file-template! "/CMakeLists\\.txt$" :trigger "__CMakeLists.txt" :mode 'json-mode)

(require 'ivy)
(require 'taskrunner)
(require 's)

(defun mv/get-ninja-tasks (DIR)
  "Retrieve all ninja tasks from directory DIR.
Happily copied from https://github.com/emacs-taskrunner/emacs-taskrunner/blob/716323aff410b4d864d137c9ebe4bbb5b8587f5e/taskrunner-clang.el#L126
TODO: upstream this"
  (let ((default-directory DIR)
        (targets '()))
    (call-process "ninja" nil (taskrunner--make-task-buff-name "ninja") nil "-t" "targets")
    (with-temp-buffer
      (set-buffer (taskrunner--make-task-buff-name "ninja"))
      (goto-char (point-min))
      (dolist (elem (split-string (buffer-string) "\n"))
        (push (car (split-string elem ":")) targets))
      (kill-current-buffer)
      )
    targets))
(defun mv/counsel-ninja-target ()
  "Forward to `describe-function'."
  (interactive)
  (ivy-read "Run Ninja: "
                (mv/get-ninja-tasks (car ivy-taskrunner-build-dir-list))
            :keymap counsel-describe-map
            :preselect (ivy-thing-at-point)
            :history 'counsel-ninja-target-history
            :require-match t
            :action (lambda (x)
                      (compile (s-join
                       " " (append `("ninja" "-C" ,(car ivy-taskrunner-build-dir-list) ,x)))))
            :caller 'counsel-ninja-target))

(defun mv/org-pdf-hook()
  (interactive)
  (progn
    (org-babel-tangle)
    (org-export-to-file 'latex (s-prepend (f-no-ext (f-this-file)) ".tex"))))
