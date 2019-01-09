;;; defer-vc.el --- description                       -*- lexical-binding: t; -*-

;; Copyright 2019 Google LLC

;; Author: Matt Armstrong <marmstrong@google.com>
;; Keywords: vc tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package makes Emacs VC operations fast(er).
;;
;; The Defer-Vc package aims to make Emacs faster by deferring
;; non-essential work related to Emacs' built in "VC" mode, with a
;; minimum negative impact.  In particular, all VC commands should
;; still work normally when invoked explicitly, even in buffers that
;; Emacs has not yet determined the VC state for.
;;
;; Usage is simple: require the package, configure the list of
;; problematic backends (there are no defaults), and enable
;; `defer-vc-mode' (a global mode).  For example:
;;
;;   (require 'defer-vc)
;;   (add-to-list 'defer-vc-backends 'Hg)
;;   (defer-vc-mode)
;;
;; ...or a similar use-package incantation:
;;
;;   (use-package defer-vc
;;     :config
;;     (add-to-list 'defer-vc-backends 'Hg)
;;     (defer-vc-mode))
;;
;; The `defer-vc' customization group used for a similar effect.
;;
;; Regardless of the configuration approach, when `defer-vc-mode' is
;; on, opening files no longer enables VC mode for the set of
;; configured backends.  Instead, the per-file status in those
;; backends are determined on a deferred, as needed, basis.  The VC
;; status will no longer be displayed in the mode line (at least,
;; until the first VC command is issued), but basic operations like
;; `find-file' will be fast.  Emacs will still be slow when VC
;; functionality is actually used, but at that point the performance
;; issue will make more sense to the user.

;;; Background

;; Emacs' built in VC implementation slows Emacs down.  This is most
;; evident when the VC backend is slow.  The code refreshes
;; information displayed in the mode line synchronously whenever a
;; file is loaded.  The rest of the VC subsystem is designed with this
;; assumption; there is no support for deferring this work until the
;; user explicitly invokes a command actually requires the status.
;;
;; Evidence that users find this annoying is easy to find.
;;
;; A thread in r/emacs on Reddit claimed that disabling VC mode was
;; "The biggest performance improvement to Emacs I've made in years":
;; http://redd.it/4c0mi3, and it amounts to:
;;
;;   (remove-hook 'find-file-hooks 'vc-find-file-hook)
;;
;; This hack, however totally disables VC mode for the affected files.
;; For example, later invoking "M-x vc-diff" results in an error.
;;
;; VC has a few supported ways of disabling itself, which are
;; preferable to the above.  Customizing the `vc-ignore-dir-regexp'
;; variable can be used to disable it on a directory basis, and
;; customizing the `vc-handled-backends' variable can be used to
;; disable some or all backends entirely.  Neither approach is
;; satisfactory if the user still wants to use VC commands!
;;
;; There are other threads.  "Git slows Emacs to Death" at
;; https://stackoverflow.com/questions/6724471/git-slows-down-emacs-to-death-how-to-fix-this
;; and its companion piece "Mercurial slows Emacs to Death" at
;; https://stackoverflow.com/questions/17323841/mercurial-slows-emacs-to-death-when-opening-saving-files.
;; In each case, I find it telling that the user(s) did not find it
;; easy to diagnose and mitigate the performance issues for
;; themselves, which suggests that Emacs could do better here.  There
;; may be some hope to fix this problem in Emacs itself.  See
;; https://lists.gnu.org/archive/html/emacs-devel/2016-02/msg00440.html.
;;
;; Until then, this little hack can ease the pain.

;;; Code:

(defgroup defer-vc nil
  "Minor mode for making Emacs VC less intrusive for slow backends."
  :prefix "defer-vc-"
  :group 'vc)

(defcustom defer-vc-backends nil
  "A list of VC backends to defer.

When Defer-Vc mode is on, these backends are temporarily removed
while doing some operations such as opening files.  See
function `defer-vc-mode' for more information."
  :group 'defer-vc
  :options vc-handled-backends
  :type '(repeat symbol))

(defvar-local defer-vc-deferred nil
  "When t, a call to `vc-refresh-state' has been deferred.

A buffer with a deferred VC state will have its true VC state
determined using an \"only as needed\" heuristic.")

(defvar defer-vc-deferring nil
  "Non-nil when the `defer-vc-backends` are disabled.")

(defun defer-vc-remove-backends (original)
  "Return a copy of ORIGINAL with `defer-vc-backends' removed."
  (let (filtered)
    (dolist (element original filtered)
      (unless (member element defer-vc-backends)
        (setq filtered (cons element filtered))))))

(defun defer-vc--remove-backends-around (orig-fun &rest args)
  "Call ORIG-FUN with ARGS, with a trimmed set of active VC backends.

Maybe.  XXX: explain.

Temporarily binds `vc-handled-backends' to a copy of itself with
`defer-vc-backends' removed, disabling those backends for the
call.  If VC fails to find a backend for the buffer, set
`defer-vc-deferred' on the buffer, which arranges for it to be
called again later.

This function is intended to be used as \"around\" advice for
common Emacs functions such as `auto-revert-handler' and
`after-find-file'."
  (let* ((defer-vc-deferring
           ;; Avoid deferring in various scenarios that complicate
           ;; things significantly.  See `vc-deduce-fileset' for the
           ;; set of conditions where a VC action doesn't necessarily
           ;; correspond to the current buffer's file.
           (not (or (not buffer-file-name)
                    (derived-mode-p 'dired-mode)
                    (derived-mode-p 'log-view-mode)
                    (derived-mode-p 'vc-dir-mode))))
         (vc-handled-backends
          (if defer-vc-deferring
              (defer-vc-remove-backends vc-handled-backends)
            vc-handled-backends)))
    (apply orig-fun args)))

(defun defer-vc--refresh-state-after (&rest _args)
  ""
  (when (and defer-vc-deferring
             (not (local-variable-p 'defer-vc-deferred)))
    ;; NOTE: alternatively we could do this, at the cost of breaking
    ;; through an API abstraction.
    ;;
    ;; (setq defer-vc-deferred
    ;;       (not (if buffer-file-name
    ;;                (eq 'none (vc-file-getprop
    ;;                           (expand-file-name buffer-file-name)
    ;;                           'vc-backend)))))
    (setq defer-vc-deferred (not (vc-backend buffer-file-name)))))

(defun defer-vc--refresh-deferred-state (buffers)
  "Call `vc-refresh-state' on BUFFERS.

For all BUFFERS with the buffer local variable
`defer-vc-deferred' set will have fresh VC state after this
call."
  (dolist (buffer buffers)
    (when (buffer-local-value 'defer-vc-deferred buffer)
      (with-current-buffer buffer
        (with-temp-message
            (format "Refreshing VC state for buffer %s..." buffer)
          (setq defer-vc-deferred nil)
          (vc-refresh-state))))))

(defun defer-vc--deduce-fileset-around (orig-fun &rest args)
  "Call ORIG-FUN with ARGS after performing deferred actions.

If `defer-vc-deferred' is set on any buffers, first call
`vc-refresh-state' on all of them.

This function is intended to be used as \"around\" advice for VC
functions that otherwise expect `vc-refresh-state' to have
already been called."
  ;; Refresh deferred state in all buffers for the conditions where
  ;; `vc-deduce-fileset' may produce a set of files, since we want it
  ;; to have correct info about all open buffers.  See similar logic
  ;; in `defer-vc--remove-backends-around'.  Otherwise, refresh only
  ;; the current buffer.
  (defer-vc--refresh-deferred-state
    (if (or (derived-mode-p 'dired-mode)
            (derived-mode-p 'log-view-mode)
            (derived-mode-p 'vc-dir-mode))
        (buffer-list)
      (list (current-buffer))))
  (apply orig-fun args))

(defun defer-vc-advice-add ()
  "Add all advice needed by Defer-Vc mode."
  (advice-add 'auto-revert-handler :around 'defer-vc--remove-backends-around)
  (advice-add 'after-find-file :around 'defer-vc--remove-backends-around)
  (advice-add 'vc-deduce-fileset :around 'defer-vc--deduce-fileset-around)
  (advice-add 'vc-refresh-state :after 'defer-vc--refresh-state-after))

(defun defer-vc-advice-remove ()
  "Remove all advice added by Defer-Vc mode."
  (advice-remove 'auto-revert-handler 'defer-vc--remove-backends-around)
  (advice-remove 'auto-revert-handler 'defer-vc--remove-backends-around)
  (advice-remove 'vc-deduce-fileset 'defer-vc--deduce-fileset-around)
  (advice-remove 'vc-refresh-state 'defer-vc--refresh-state-after))

(defun defer-vc-turn-on ()
  "Turn Defer-Vc mode on."
  (defer-vc-advice-add))

(defun defer-vc-turn-off ()
  "Turn Defer-Vc mode off."
  (defer-vc-advice-remove)
  (dolist (buffer (buffer-list))
    (if (local-variable-p 'defer-vc-deferred buffer)
        (with-current-buffer buffer
          (kill-local-variable 'defer-vc-deferred))))
  (defer-vc--refresh-all-deferred-state))

(define-minor-mode defer-vc-mode
  "Toggle Defer-Vc mode.

Interactively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Defer-Vc mode is enabled, the backends in
'defer-vc-backends' are temporarily removed from
'vc-handled-backends' when reopening files, which speeds things
up at the cost of not displaying the VC state in the mode
line. Subsequent VC commands, such as \\[vc-diff], do work as usual
-- the VC state is determined when needed.  It also possible to
refresh the state explicitly by executing \\[vc-refresh-state]."
  :global t                           ; This mode is not buffer local.
  :lighter " DeferVc"
  (if defer-vc-mode
      (defer-vc-turn-on)
    (defer-vc-turn-off)))

(defun defer-vc-unload-function ()
  "Unload Defer-Vc.

This turns the mode off."
  (defer-vc-mode nil))

(if nil
    (progn
      (global-auto-revert-mode 0)
      (dolist (func
               '(
                 ;;auto-revert-handler
                 after-find-file
                 vc-deduce-fileset
                 vc-refresh-state
                 vc-backend
                 vc-file-setprop
                 vc-file-getprop
                 vc-file-clearprops
                 defer-vc--deduce-fileset-around
                 defer-vc--refresh-deferred-state
                 defer-vc--refresh-state-after
                 defer-vc--remove-backends-around))
        (trace-function-background func))))

(provide 'defer-vc)

;;; defer-vc.el ends here
