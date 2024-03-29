;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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
  :init (add-hook 'gfm-mode-hook '(lambda ()
                                    (progn
                                      (auto-fill-mode 1)
                                      (flyspell-mode)
                                      )
                                    ))
  )

(use-package! org
  :config (setq fill-column 80)
  :init (add-hook 'org-mode-hook '(lambda ()
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

(use-package! python-mode
  :hook #'auto-revert-mode
  )

(use-package! lsp-pyright
  :init (setq lsp-pyright-multi-root nil)
  )

(use-package! git-link
  :config (setq git-link-use-commit t))

;; https://gist.github.com/vmiheer/ee9de0a971af3e22520b64442331700f
(use-package! python-black
  :demand t
  :after python
  :config
  (add-hook! 'python-mode-hook #'python-black-on-save-mode)
  (setq python-black-command "PYTHONPATH=\"\" black")
  ;; Feel free to throw your own personal keybindings here
  (map! :leader :desc "Blacken Buffer" "m b b" #'python-black-buffer)
  (map! :leader :desc "Blacken Region" "m b r" #'python-black-region)
  (map! :leader :desc "Blacken Statement" "m b s" #'python-black-statement)
)

(use-package! justl
  :config
  (map! :n "RET" 'justl-exec-recipe)
  )

(use-package! yaml-mode
  :mode (".yaml$")
  :hook
  (yaml-mode . yaml-mode-outline-hook)

  :init
  (defun yaml-outline-level ()
    "Return the outline level based on the indentation, hardcoded at 2 spaces."
    (s-count-matches "[ ]\\{2\\}" (match-string 0)))

  (defun yaml-mode-outline-hook ()
    (outline-minor-mode)
    (setq outline-regexp
      (rx
       (seq
        bol
        (group (zero-or-more "  ")
               (or (group
                    (seq (or (seq "\"" (*? (not (in "\"" "\n"))) "\"")
                             (seq "'" (*? (not (in "'" "\n"))) "'")
                             (*? (not (in ":" "\n"))))
                         ":"
                         (?? (seq
                              (*? " ")
                              (or (seq "&" (one-or-more nonl))
                                  (seq ">-")
                                  (seq "|"))
                              eol))))
                   (group (seq
                           "- "
                           (+ (not (in ":" "\n")))
                           ":"
                           (+ nonl)
                           eol)))))))
    (setq outline-level 'yaml-outline-level))
  )

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

(use-package! evil-string-inflection
  :config
  (map! :leader :desc "Toggle case" "t t" #'string-inflection-cycle))

(require 'mlir-lsp-client)
(use-package! mlir-mode
  :mode ("\\.td\\'" . tablegen-mode)
  :config (progn
            (setq lsp-mlir-server-executable "/home/mvaidya/source/repos/MLIR_Workspace/llvm-project/build/bin/mlir-lsp-server")
            (add-hook! 'mlir-mode-hook '(lambda () (progn (lsp-mlir-setup) (lsp))))))

(use-package! lsp-python-ms
  :init (setq lsp-python-ms-nupkg-channel "daily")
  )

(use-package! ivy-bibtex
  :init
  (setq bibtex-completion-bibliography '("~/Dropbox/emacs/bibliography/references.bib"
					 "~/Dropbox/emacs/bibliography/dei.bib"
					 "~/Dropbox/emacs/bibliography/master.bib"
					 "~/Dropbox/emacs/bibliography/archive.bib")
	bibtex-completion-library-path '("~/Dropbox/emacs/bibliography/bibtex-pdfs/")
	bibtex-completion-notes-path "~/Dropbox/emacs/bibliography/notes/"
	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

	bibtex-completion-additional-search-fields '(keywords)
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "open" nil 0 nil fpath))))

(use-package! org-ref
  :init
  (require 'bibtex)
  (setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5)
  (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
  (define-key org-mode-map (kbd "s-[") 'org-ref-insert-link-hydra/body)
  (require 'org-ref-ivy)
  (require 'org-ref-arxiv)
  (require 'org-ref-scopus)
  (require 'org-ref-wos))

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
(let ((hostname
    (s-trim
     (shell-command-to-string "hostname -f"))))
(setq global-mode-string
      (cond ((consp global-mode-string)
             (add-to-list 'global-mode-string hostname 'APPEND))
            ((not global-mode-string)
             (list hostname))
            ((stringp global-mode-string)
             (list global-mode-string hostname)))))
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

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
