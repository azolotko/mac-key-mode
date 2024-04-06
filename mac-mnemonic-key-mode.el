;;; mac-mnemonic-key-mode.el --- provide mac-style key bindings where most shortcuts have two keystrokes: the first one defines a group and the second one defines an action in this group.

;; Copyright (C) 2004-2010  Seiji Zenitani
;; Copyright (C) 2023-2024  Alex Zolotko

;; Author: Seiji Zenitani <zenitani@mac.com>
;; $Id$
;; Keywords: tools, mac, mnemonic
;; Created: 2004-12-27
;; URL(en): https://github.com/azolotko/mac-mnemonic-key-mode

;; Contributors: Tetsuro Kurita, Nozomu Ando, Dave Peck

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides mac-mnemonic-key-mode, a minor mode that provides
;; mac-like key bindings and relevant elisp functions.
;;
;; To use this package, add these lines to your .emacs file:
;;
;;     (require 'mac-mnemonic-key-mode)
;;     (mac-mnemonic-key-mode 1)
;;
;; Note that mac-mnemonic-key-mode works better with redo+.el (but I don't use
;; it...)  In order to set additional key bindings, modify
;; mac-mnemonic-key-mode-map in your .emacs file:
;;
;;     (require 'mac-mnemonic-key-mode)
;;     (define-key mac-mnemonic-key-mode-map [(super l)] 'goto-line)
;;
;; mac-mnemonic-key-mode takes advantage of additional functions, provided by
;; the mac-functions.patch <http://homepage.mac.com/zenitani/comp-e.html>.
;; (e.g. mac-spotlight-search, mac-spotlight-search etc.)


;;; Code:


(defgroup mac-mnemonic-key-mode nil
  "Mac-style key-binding mode where most shortcuts have two keystrokes."
  :group 'mac
  :version "22.3")
(defconst mac-mnemonic-key-mode-lighter (char-to-string 8984) ; the command mark
  "A lighter string which is displayed in the modeline
when `mac-mnemonic-key-mode' is on.")

(defcustom mac-mnemonic-key-mode-hook nil
  "The hook to run when mac-mnemonic-key-mode is toggled."

  :type 'hook
  :group 'mac-mnemonic-key-mode)

(defcustom mac-mnemonic-key-advanced-setting t
  "If non-nil, `mac-mnemonic-key-mode' activates addional settings:
1) menu items are added to the File menu and the Edit menu, and
2) the SPC key invokes Quick Look information in dired-mode."
  :group 'mac-mnemonic-key-mode
  :type 'boolean)

(defvar mac-mnemonic-key-backup-command-modifier nil
  "Internal variable.  Do not use this.")


;; process objects
(defvar mac-mnemonic-key-speech-process nil
  "The process object for text-to-speech subprocess.")
(defvar mac-mnemonic-key-ql-process nil
  "The process object for Quick Look subprocess.")


(defvar mac-mnemonic-key-mode-map
  (let ((map (make-sparse-keymap))
        (block-map (make-sparse-keymap))
        (debug-map (make-sparse-keymap))
        (editor-map (make-sparse-keymap))
        (inspect-map (make-sparse-keymap))
        (jump-map (make-sparse-keymap))
        (kommit-map (make-sparse-keymap))
        (line-map (make-sparse-keymap))
        (navigate-map (make-sparse-keymap))
        (project-map (make-sparse-keymap))
        (refactor-map (make-sparse-keymap))
        (test-map (make-sparse-keymap))
        (utilities-map (make-sparse-keymap))
        (window-map (make-sparse-keymap)))
    (define-key map [(super o)] (lambda()(interactive)(let(last-nonmenu-event)(menu-find-file-existing))))
    (define-key map [(super s)] 'save-buffer)
    (define-key map [(super shift s)] 'mac-mnemonic-key-save-as)
    (define-key map [(super q)] 'save-buffers-kill-emacs)
    (define-key map [(super z)] 'undo)
    (define-key map [(super shift z)] 'undo-fu-only-redo)
    (define-key map [(super x)] 'clipboard-kill-region)
    (define-key map [(super c)] 'clipboard-kill-ring-save)
    (define-key map [(super v)] 'clipboard-yank)
    (define-key map [(super a)] 'mark-whole-buffer)
    (define-key map [(super f)] 'isearch-forward)
    (define-key map [(super meta f)] 'occur)
    (define-key map [(super g)] 'isearch-repeat-forward)
    (define-key map [(super shift g)] 'isearch-repeat-backward)
    (define-key map [(super m)] 'iconify-frame)
    (define-key map [(super \`)] 'other-frame)
    (define-key map [(super shift n)] 'make-frame-command)
    (define-key map [(super shift w)] 'delete-frame)
    (define-key map [(super \?)] 'info)
    (define-key map [(super /)] 'info)
    (define-key map [(super .)] 'keyboard-quit)
    (define-key map [(super up)] 'beginning-of-buffer)
    (define-key map [(super down)] 'end-of-buffer)
    (define-key map [(super left)] 'beginning-of-line)
    (define-key map [(super right)] 'end-of-line)
    (define-key map [A-mouse-1] 'browse-url-at-mouse)
    (define-key map [C-down-mouse-1] 'mac-mnemonic-key-context-menu)
    (define-key map [mouse-3] 'mac-mnemonic-key-context-menu)
;;    (define-key map [C-mouse-1] 'mac-mnemonic-key-context-menu)
    (define-key map [A-S-mouse-1] 'mouse-buffer-menu)
    (define-key map [S-down-mouse-1] 'mac-mnemonic-key-shift-mouse-select)

    (define-key map [(super b)] block-map)
    (define-key map [(super d)] debug-map)

    (define-key map [(super e)] editor-map)
    (define-key editor-map (kbd "x") 'kill-buffer-and-window)
    (define-key editor-map [(super x)] 'kill-buffer-and-window)

    (define-key map [(super i)] inspect-map)

    (define-key map [(super j)] jump-map)
    (define-key jump-map (kbd "d") 'xref-find-definitions)
    (define-key jump-map [(super d)] 'xref-find-definitions)
    (define-key jump-map (kbd "r") 'ff-find-related-file)
    (define-key jump-map [(super r)] 'ff-find-related-file)

    (define-key map [(super k)] kommit-map)

    (define-key map (kbd "S-<escape>") 'delete-window)
    (define-key map (kbd "s-[") 'better-jumper-jump-backward)
    (define-key map (kbd "s-]") 'better-jumper-jump-forward)
    (define-key map [(super h)] 'iconify-frame)

    (define-key map [(super l)] line-map)
    (define-key line-map (kbd "b") 'bookmark-set)
    (define-key line-map [(super b)] 'bookmark-set)
    (define-key line-map (kbd "c") 'comment-line)
    (define-key line-map [(super c)] 'comment-line)
    (define-key line-map (kbd "d") 'duplicate-line)
    (define-key line-map [(super d)] 'duplicate-line)
    (define-key line-map (kbd "g") 'goto-line)
    (define-key line-map [(super g)] 'goto-line)
    (define-key line-map (kbd "i") 'lisp-indent-line)
    (define-key line-map [(super i)] 'lisp-indent-line)
    (define-key line-map (kbd "j") 'join-line)
    (define-key line-map [(super j)] 'join-line)
    (define-key line-map (kbd "n") 'doom/toggle-line-numbers)
    (define-key line-map [(super n)] 'doom/toggle-line-numbers)
    (define-key line-map (kbd "r") 'reverse-region)
    (define-key line-map [(super r)] 'reverse-region)
    (define-key line-map (kbd "s") 'sort-lines)
    (define-key line-map [(super s)] 'sort-lines)
    (define-key line-map (kbd "x") 'kill-whole-line)
    (define-key line-map [(super x)] 'kill-whole-line)

    (define-key map [(super n)] navigate-map)
    (define-key navigate-map (kbd "f") '+vertico/project-search)
    (define-key navigate-map [(super f)] '+vertico/project-search)
    (define-key navigate-map (kbd "o") 'projectile-find-file)
    (define-key navigate-map [(super o)] 'projectile-find-file)
    (define-key navigate-map (kbd "r") 'consult-recent-file) ; 'projectile-recentf)
    (define-key navigate-map [(super r)] 'consult-recent-file) ; 'projectile-recentf)
    (define-key navigate-map (kbd "u") 'xref-find-references)
    (define-key navigate-map [(super u)] 'xref-find-references)

    (define-key map [(super p)] project-map)
    (define-key project-map (kbd "o") (lambda()(interactive)(call-interactively 'dired 'projectile-add-known-project)))
    (define-key project-map [(super o)] (lambda()(interactive)(call-interactively 'dired 'projectile-add-known-project)))
    (define-key project-map (kbd "p") 'projectile-switch-project)
    (define-key project-map [(super p)] 'projectile-switch-project)

    (define-key map [(super r)] refactor-map)
    (define-key map [(super t)] test-map)

    (define-key map [(super u)] utilities-map)
    (define-key utilities-map (kbd "a") 'execute-extended-command)
    (define-key utilities-map [(super a)] 'execute-extended-command)

    (define-key map [(super w)] window-map)
    (define-key window-map (kbd "p") '+treemacs/toggle)
    (define-key window-map [(super p)] '+treemacs/toggle)
    (define-key window-map (kbd "t") 'mac-mnemonic-key-term-split-horizontal)
    (define-key window-map [(super t)] 'mac-mnemonic-key-term-split-horizontal)
    (define-key window-map (kbd "v") 'magit)
    (define-key window-map [(super v)] 'magit)

    map)
  "Keymap for `mac-mnemonic-key-mode'.")

;; mode-line menu
(define-key-after mode-line-mode-menu [mac-mnemonic-key-mode]
  `(menu-item ,(purecopy
                (concat "Mac Mnemonic Key (" mac-mnemonic-key-mode-lighter ")"))
              mac-mnemonic-key-mode :button (:toggle . mac-mnemonic-key-mode))
  'highlight-changes-mode)

;;;###autoload
(define-minor-mode mac-mnemonic-key-mode
  "Toggle Mac Mnemonic Key mode.
With arg, turn Mac Mnemonic Key mode on if arg is positive.
When Mac Mnemonic Key mode is enabled, mac-style key bindings are provided."
  :global t
  :group 'mac-mnemonic-key-mode
  :lighter (" " mac-mnemonic-key-mode-lighter)
  :keymap 'mac-mnemonic-key-mode-map
  (if mac-mnemonic-key-mode
      (progn
        (setq mac-command-modifier 'super
              mac-option-modifier 'meta)

        (if (boundp 'mac-mnemonic-key-mode-internal)
            (setq mac-mnemonic-key-mode-internal t))

        ;; turn on advanced settings
        (when mac-mnemonic-key-advanced-setting

          ;; menu items
          (define-key-after menu-bar-file-menu [mac-mnemonic-key-file-separator]
            '("--" . nil) 'recover-session)
          (define-key-after menu-bar-file-menu [mac-mnemonic-key-show-in-finder]
            '(menu-item "Show In Finder" mac-mnemonic-key-show-in-finder
              :help "Display current file/directory in a Finder window"
              :enable (or (and (boundp 'buffer-file-name) buffer-file-name)
                          (and (boundp 'dired-directory) dired-directory)))
            'mac-mnemonic-key-file-separator)
          (define-key-after menu-bar-file-menu [mac-mnemonic-key-open-terminal]
            '(menu-item "Open Terminal" mac-mnemonic-key-open-terminal
              :help "Launch Terminal.app and go to the relevant directory")
            'mac-mnemonic-key-show-in-finder)
          (define-key-after menu-bar-edit-menu [redo]
            '(menu-item "Redo" redo
            :help "Redo the most recent undo"
            :enable (not (or (eq buffer-undo-list t)
                             (eq last-buffer-undo-list nil)
                             ;; ** one more thing here **
                             (eq buffer-undo-list pending-undo-list)
                             (eq (cdr buffer-undo-list) pending-undo-list)
                             )))
            'undo)
          (define-key-after menu-bar-edit-menu [mac-mnemonic-key-edit-separator]
            '("--" . nil) 'redo)

          ;; assign mac-mnemonic-key-quick-look to the SPC key
          (if (boundp 'dired-mode-map)
              (define-key dired-mode-map " " 'mac-mnemonic-key-quick-look)
            (add-hook 'dired-mode-hook
                      (lambda () (interactive)
                        (define-key dired-mode-map " " 'mac-mnemonic-key-quick-look)))
            )

          ))
    (progn

      (if (boundp 'mac-mnemonic-key-mode-internal)
          (setq mac-mnemonic-key-mode-internal nil))

      ;; turn off advanced settings
      (when mac-mnemonic-key-advanced-setting

        ;; menu items
        (global-unset-key [menu-bar file mac-mnemonic-key-file-separator])
        (global-unset-key [menu-bar file mac-mnemonic-key-show-in-finder])
        (global-unset-key [menu-bar file mac-mnemonic-key-open-terminal])
        (global-unset-key [menu-bar edit redo])
        (global-unset-key [menu-bar edit mac-mnemonic-key-edit-separator])

        ;; restore SPC to dired-next-line (a bad way to deal with it)
        (if (boundp 'dired-mode-map)
            (define-key dired-mode-map " " 'dired-next-line))
        (remove-hook 'dired-mode-hook
                     (lambda () (interactive)
                       (define-key dired-mode-map " " 'mac-mnemonic-key-quick-look)))

        ))
    ))


;; close window (command + W)
(defun mac-mnemonic-key-close-window ()
  "Close the Quick Look window or kill the current buffer."
  (interactive)
  (let ((mybuffer (and mac-mnemonic-key-ql-process
                       (process-buffer mac-mnemonic-key-ql-process))))
    (if (buffer-live-p mybuffer)
        (kill-buffer mybuffer)
      (kill-this-buffer))
    ))

;; save as.. dialog (shift + command + S)
(defun mac-mnemonic-key-save-as (filename &optional wildcards)
  "Write current buffer to another file using standard file open dialog."
  (interactive
   (let (last-nonmenu-event)
     (find-file-read-args "Write file: " nil)))
   (write-file filename))


;; utf8 code by Ando-san
(defun mac-mnemonic-key-applescript-utf8data (str)
  (let ((len (length str))
        (len1 31) ;XXX: 254/2/4. utf-8 is 4byte per code point at most.
        (reslist '(")"))
        pos epos)
    (setq pos len)
    (while (> pos 0)
      (setq epos pos)
      (setq pos (max (- pos len1) 0))
      (setq reslist (cons " & (\307data utf8"
                          (cons (mapconcat (lambda (ch) (format "%02X" ch))
                                           (encode-coding-string
                                            (substring str pos epos)
                                            'utf-8) "")
                                (cons "\310 as Unicode text)"
                                      reslist)))))
    (apply 'concat "(\"\"" reslist)))


;; Show In Finder (command + I)

(defun mac-mnemonic-key-show-in-finder (&optional path)
  "Display current file/directory in a Finder window"
  (interactive)
  (let ((item (or path
                  (and (boundp 'buffer-file-name) buffer-file-name)
                  (and (eq major-mode 'dired-mode) default-directory)) ))

    (cond
     ((not (stringp item)))
     ((file-remote-p item)
      (error "This item is located on a remote system."))
     (t
      (setq item (expand-file-name item))
      (condition-case err
          (progn
            (do-applescript
             (concat
              "tell application \"Finder\" to select ("
              (mac-mnemonic-key-applescript-utf8data item)
              " as POSIX file)"))
            (if (fboundp 'mac-process-activate)
                (mac-process-activate "com.apple.finder")
              (do-applescript "tell application \"Finder\" to activate"))
            )
        (error err)))

     )))


;; Text-to-Speech functions

(defun mac-mnemonic-key-speak-buffer ()
  "Speak buffer contents."
  (interactive)
  (mac-mnemonic-key-speak-region (point-min)(point-max)))

(defun mac-mnemonic-key-speak-region (beg end)
  "Speak the region contents."
  (interactive "r")
  (mac-mnemonic-key-stop-speaking)
  (let ((buffer-file-coding-system 'utf-8-unix)
        (tmp-file (make-temp-file "emacs-speech-" nil ".txt")))
    (write-region beg end tmp-file nil)
    (message "Invoking text-to-speech...")
    (setq mac-mnemonic-key-speech-process
          (start-process "text-to-speech" "*Text-to-Speech Output*"
                         "/usr/bin/say" "-f" tmp-file))
    ))

(defun mac-mnemonic-key-stop-speaking ()
  "Terminate the text-to-speech subprocess, if it is running."
  (interactive)
  (let ((mybuffer (and mac-mnemonic-key-speech-process
                       (process-buffer mac-mnemonic-key-speech-process))))
    (when (buffer-live-p mybuffer)
      (kill-buffer mybuffer)
      (beep))
    ))


;; Quick Look
;; inspired by http://journal.mycom.co.jp/column/osx/263/index.html

(defun mac-mnemonic-key-quick-look ()
  "Display the Quick Look information for the current line's file.
You might use dired-mode-hook to use this function in dired mode,
like this:

    \(add-hook 'dired-mode-hook
       (lambda() (local-set-key \" \" 'mac-mnemonic-key-quick-look)))
"
  (interactive)

  (let ((mybuffer (and mac-mnemonic-key-ql-process
                       (process-buffer mac-mnemonic-key-ql-process)))
        (item default-directory))
    (cond
     ((buffer-live-p mybuffer)
      (kill-buffer mybuffer))
;;       (eq (process-status mac-mnemonic-key-ql-process) 'run)
;;       (kill-process mac-mnemonic-key-ql-process))
     ((file-remote-p item)
      (error "This item is located on a remote system."))
     (t
      (setq item (expand-file-name item))
      (condition-case err
          (setq item (dired-get-file-for-visit))
        (error err))
      (condition-case err
          (setq mac-mnemonic-key-ql-process
                (start-process "quicklook" "*QuickLook Output*"
                               "/usr/bin/qlmanage" "-p"
                               (shell-quote-argument item)))
        (error err)))
     )))

;; shift+click
;; Contributed by Dave Peck

(defun mac-mnemonic-key-shift-mouse-select (event)
  "Set the mark and then move point to the position clicked on with
the mouse.  This should be bound to a mouse click event type."
  (interactive "e")
  (mouse-minibuffer-check event)
  (if mark-active (exchange-point-and-mark))
  (set-mark-command nil)
  ;; Use event-end in case called from mouse-drag-region.
  ;; If EVENT is a click, event-end and event-start give same value.
  (posn-set-point (event-end event)))


;; Contextual menu

(defun mac-mnemonic-key-context-menu (event)
  "Pop up a contextual menu."
  (interactive "e")

  (let ((editable (not buffer-read-only))
        (pt (save-excursion (mouse-set-point last-nonmenu-event)))
        beg end
        )

    ;; getting word boundaries
    (if (and mark-active
             (<= (region-beginning) pt) (<= pt (region-end)) )
        (setq beg (region-beginning)
              end (region-end))
      (save-excursion
        (goto-char pt)
        (setq end (progn (forward-word) (point)))
        (setq beg (progn (backward-word) (point)))
        ))

    ;; popup menu
    (popup-menu
     '(nil
       ["Search in Spotlight"
        (mac-spotlight-search (buffer-substring-no-properties beg end))
        :active (fboundp 'mac-spotlight-search)
        :help "Do a Spotlight search of word at cursor"]
       ["Search in Google"
        (browse-url
         (concat "http://www.google.com/search?q="
                 (url-hexify-string (buffer-substring-no-properties beg end))))
        :help "Ask a WWW browser to do a Google search"]
     ["--" nil]
     ["Look Up in Dictionary"
      (browse-url
       (concat "dict:///"
               (url-hexify-string (buffer-substring-no-properties beg end))))
      :active t
      :help "Look up word at cursor in Dictionary.app"]
     ["--" nil]
     ["Cut"   (clipboard-kill-region beg end) :active (and editable mark-active)
      :help "Delete text in region and copy it to the clipboard"]
     ["Copy"  (clipboard-kill-ring-save beg end) :active mark-active
      :help "Copy text in region to the clipboard"]
     ["Paste" (clipboard-yank) :active editable
      :help "Paste text from clipboard"]
     ["--" nil]
     ("Spelling"
      ["Spelling..."
       (progn (goto-char end)(ispell-word)) :active editable
       :help "Spell-check word at cursor"]
      ["Check Spelling" (ispell-buffer) :active editable
       :help "Check spelling of the current buffer"]
      ["Check Spelling as You Type"
       (flyspell-mode)
       :style toggle :selected flyspell-mode :active editable
       :help "Check spelling while you edit the text"]
     )
     ("Font"
      ["Show Fonts" (ignore) :active nil]
      ["Bold"       (ignore) :active nil]
      ["Italic"     (ignore) :active nil]
      ["Underline"  (ignore) :active nil]
      ["Outline"    (ignore) :active nil]
      ["Styles..."  (ignore) :active nil]
      ["--" nil]
      ["Show Colors" (ignore) :active nil]
     )
     ("Speech"
      ["Start Speaking"
       (if (and mark-active
                (<= (region-beginning) pt) (<= pt (region-end)) )
           (mac-mnemonic-key-speak-region beg end)
         (mac-mnemonic-key-speak-buffer) )
       :help "Speak text through the sound output"]
      ["Stop Speaking" (mac-mnemonic-key-stop-speaking)
       :active (and mac-mnemonic-key-speech-process
                    (eq (process-status mac-mnemonic-key-speech-process) 'run))
       :help "Stop speaking"]
     )
     ["--" nil]
     ["Buffers" mouse-buffer-menu
       :help "Pop up a menu of buffers for selection with the mouse"]
     ))))


(defun mac-mnemonic-key-term-split-horizontal ()
  "Split the window horizontally and open `term` in the new window."
  (interactive)
  (split-window-below)
  (other-window 1)
  (term "/usr/bin/zsh"))

(provide 'mac-mnemonic-key-mode)

;;; mac-mnemonic-key-mode.el ends here.
