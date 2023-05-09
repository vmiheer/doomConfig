;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Miheer Vaidya"
      user-mail-address "m.vaidya@utah.edu")

(setq mac-command-modifier 'control)

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
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (if (not (directory-empty-p "~/box/org"))
    "~/box/org"
  "~/org"
  ))

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
            (lambda ()
              (progn
                (shell-command "powershell -C \"New-BurntToastNotification  -Text StartedPomodoro\"")
                (save-excursion
                  (org-clock-goto)
                  (save-buffer))
                )
              )))

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
          (setq org-todo-keywords
                '((sequence
                   "TODO(t)"  ; A task that needs doing & is ready to do
                   "PROJ(p@)"  ; A project, which usually contains other tasks
                   "LOOP(r)"  ; A recurring task
                   "STRT(s)"  ; A task that is in progress
                   "WAIT(w@/!)"  ; Something external is holding up this task
                   "HOLD(h@/!)"  ; This task is paused/on hold because of me
                   "IDEA(i)"  ; An unconfirmed and unapproved task or notion
                   "|"
                   "DONE(d@)"  ; Task successfully completed
                   "KILL(k@)") ; Task was cancelled, aborted or is no longer applicable
                  (sequence
                   "[ ](T)"   ; A task that needs doing
                   "[-](S)"   ; Task is in progress
                   "[?](W)"   ; Task is being held up or paused
                   "|"
                   "[X](D)")  ; Task was completed
                  (sequence
                   "|"
                   "OKAY(o)"
                   "YES(y)"
                   "NO(n)"))
                org-todo-keyword-faces
                '(("[-]"  . +org-todo-active)
                  ("STRT" . +org-todo-active)
                  ("[?]"  . +org-todo-onhold)
                  ("WAIT" . +org-todo-onhold)
                  ("HOLD" . +org-todo-onhold)
                  ("PROJ" . +org-todo-project)
                  ("NO"   . +org-todo-cancel)
                  ("KILL" . +org-todo-cancel)))
  :init (add-hook 'org-mode-hook '(lambda ()
                                    (progn
                                      (auto-fill-mode 1)
                                      (flyspell-mode)
                                      (auto-revert-mode 1)
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

(windmove-default-keybindings)

(use-package! plantuml-mode
  :config
  (setq plantuml-default-exec-mode 'jar))

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
