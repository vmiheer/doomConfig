;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Miheer Vaidya"
      user-mail-address "m.vaidya@utah.edu")

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

(setq doom-font (font-spec :family "Jetbrains Mono Medium" :size 14))


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
