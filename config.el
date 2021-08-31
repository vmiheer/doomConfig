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
;; available. You can either set `doom-theme' or manually load a theme with he
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(use-package! org-mode-crate
             :init (global-set-key (kbd "<f12>") 'org-agenda)
             :config (require 'org-mode-crate))

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
        :desc "Toggle mouse support in term mode" "m" #'xterm-mouse-mode))

(use-package! python-mode
  :hook #'auto-revert-mode
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
;; TODO: make it generic using https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html
(use-package! lsp-julia
    :config
    (setq lsp-julia-package-dir nil)
    (setq lsp-julia-flags `("-J/home/mvaidya/.julia/languageserver.so"))
      (setq lsp-julia-default-environment "~/.julia/environments/v1.6"))
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
