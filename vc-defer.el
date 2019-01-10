;;; vc-defer.el --- description                       -*- lexical-binding: t; -*-

;; Copyright 2019 Google LLC

;; SPDX-License-Identifier: GPL-3.0-or-later

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
;; The Vc-Defer package aims to make Emacs faster by deferring
;; non-essential work related to Emacs' built in "VC" mode, with a
;; minimum negative impact.  In particular, all VC commands should
;; still work normally when invoked explicitly, even in buffers that
;; Emacs has not yet determined the VC state for.
;;
;; Usage is simple: require the package, configure the list of
;; problematic backends (there are no defaults), and enable
;; `vc-defer-mode' (a global mode).  For example:
;;
;;   (require 'vc-defer)
;;   (add-to-list 'vc-defer-backends 'Hg)
;;   (vc-defer-mode)
;;
;; ...or a similar use-package incantation:
;;
;;   (use-package vc-defer
;;     :config
;;     (add-to-list 'vc-defer-backends 'Hg)
;;     (vc-defer-mode))
;;
;; The `vc-defer' customization group used for a similar effect.
;;
;; Regardless of the configuration approach, when `vc-defer-mode' is
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

(defgroup vc-defer nil
  "Minor mode for making Emacs VC less intrusive for slow backends."
  :prefix "vc-defer-"
  :group 'vc)

(defcustom vc-defer-backends nil
  "A list of VC backends to defer.

When Vc-Defer mode is on, these backends are temporarily removed
while doing some operations such as opening files.  See
function `vc-defer-mode' for more information."
  :group 'vc-defer
  :options vc-handled-backends
  :type '(repeat symbol))

(defvar-local vc-defer-deferred nil
  "When t, a call to `vc-refresh-state' has been deferred.

A buffer with a deferred VC state will have its true VC state
determined using an \"only as needed\" heuristic.")

(defvar vc-defer-deferring nil
  "Non-nil when the `vc-defer-backends` are disabled.")

(defun vc-defer-remove-backends (original)
  "Return a copy of ORIGINAL with `vc-defer-backends' removed."
  (let (filtered)
    (dolist (element original filtered)
      (unless (member element vc-defer-backends)
        (setq filtered (cons element filtered))))))

(defun vc-defer--remove-backends-around (orig-fun &rest args)
  "Call ORIG-FUN with ARGS, with a trimmed set of active VC backends.

Maybe.  The set of VC backends is trimmed unless the current
buffer's variable `buffer-file-name' is nil, or is in a mode that VC
treats specially.

Otherwise, temporarily binds `vc-handled-backends' to a copy of
itself with `vc-defer-backends' removed, disabling those backends
for the call.  If VC fails to find a backend for the buffer, set
`vc-defer-deferred' on the buffer, which arranges for it to be
called again later.

This function is intended to be used as \"around\" advice for
common Emacs functions such as `auto-revert-handler' and
`after-find-file'."
  (let* ((vc-defer-deferring
           ;; Avoid deferring in various scenarios that complicate
           ;; things significantly.  See `vc-deduce-fileset' for the
           ;; set of conditions where a VC action doesn't necessarily
           ;; correspond to the current buffer's file.
           (not (or (not buffer-file-name)
                    (derived-mode-p 'dired-mode)
                    (derived-mode-p 'log-view-mode)
                    (derived-mode-p 'vc-dir-mode))))
         (vc-handled-backends
          (if vc-defer-deferring
              (vc-defer-remove-backends vc-handled-backends)
            vc-handled-backends)))
    (apply orig-fun args)))

(defun vc-defer--refresh-state-after (&rest _args)
  "Set `vc-defer-deferred' if warranted.

When variable `vc-mode' is on this is run as advice after
`vc-refresh-state'.

It sets the buffer local variable `vc-defer-deferred' if it
appears that the our temporary reduction of `vc-handled-backends'
may have caused the file to be attributed to a different, or no,
backend."
  (when (and vc-defer-deferring
             (not (local-variable-p 'vc-defer-deferred)))
    ;; NOTE: alternatively we could do this, at the cost of breaking
    ;; through an API abstraction.
    ;;
    ;; (setq vc-defer-deferred
    ;;       (not (if buffer-file-name
    ;;                (eq 'none (vc-file-getprop
    ;;                           (expand-file-name buffer-file-name)
    ;;                           'vc-backend)))))
    ;;
    ;; FIXME: note that this is subtly wrong.  The trimmed set of
    ;; `vc-handled-backends' may allow one VC backend to be selected
    ;; when another would have been.  The correct logic would be: set
    ;; `vc-deferred' if there is no backend set *or* if the backend
    ;; that was set appears after one of the `vc-defer-backends' in
    ;; the user's `vc-handled-backends'.  --Matt
    (setq vc-defer-deferred (not (vc-backend buffer-file-name)))))

(defun vc-defer--refresh-deferred-state (buffers)
  "Call `vc-refresh-state' on BUFFERS.

For all BUFFERS with the buffer local variable
`vc-defer-deferred' set will have fresh VC state after this
call."
  (dolist (buffer buffers)
    (when (buffer-local-value 'vc-defer-deferred buffer)
      (with-current-buffer buffer
        (with-temp-message
            (format "Refreshing VC state for buffer %s..." buffer)
          (setq vc-defer-deferred nil)
          (vc-refresh-state))))))

(defun vc-defer--deduce-fileset-around (orig-fun &rest args)
  "Call ORIG-FUN with ARGS after performing deferred actions.

If `vc-defer-deferred' is set on any buffers, first call
`vc-refresh-state' on all of them.

This function is intended to be used as \"around\" advice for VC
functions that otherwise expect `vc-refresh-state' to have
already been called."
  ;; Refresh deferred state in all buffers for the conditions where
  ;; `vc-deduce-fileset' may produce a set of files, since we want it
  ;; to have correct info about all open buffers.  See similar logic
  ;; in `vc-defer--remove-backends-around'.  Otherwise, refresh only
  ;; the current buffer.
  (vc-defer--refresh-deferred-state
    (if (or (derived-mode-p 'dired-mode)
            (derived-mode-p 'log-view-mode)
            (derived-mode-p 'vc-dir-mode))
        (buffer-list)
      (list (current-buffer))))
  (apply orig-fun args))

(defun vc-defer--advice-add ()
  "Add all advice needed by Vc-Defer mode."
  (advice-add 'auto-revert-handler :around 'vc-defer--remove-backends-around)
  (advice-add 'after-find-file :around 'vc-defer--remove-backends-around)
  (advice-add 'vc-deduce-fileset :around 'vc-defer--deduce-fileset-around)
  (advice-add 'vc-refresh-state :after 'vc-defer--refresh-state-after))

(defun vc-defer--advice-remove ()
  "Remove all advice added by Vc-Defer mode."
  (advice-remove 'auto-revert-handler 'vc-defer--remove-backends-around)
  (advice-remove 'auto-revert-handler 'vc-defer--remove-backends-around)
  (advice-remove 'vc-deduce-fileset 'vc-defer--deduce-fileset-around)
  (advice-remove 'vc-refresh-state 'vc-defer--refresh-state-after))

(defun vc-dever--turn-on ()
  "Turn Vc-Defer mode on."
  (vc-defer--advice-add))

(defun vc-dever--turn-off ()
  "Turn Vc-Defer mode off."
  (vc-defer--advice-remove)
  (dolist (buffer (buffer-list))
    (if (local-variable-p 'vc-defer-deferred buffer)
        (with-current-buffer buffer
          (kill-local-variable 'vc-defer-deferred))))
  (vc-defer--refresh-all-deferred-state))

(define-minor-mode vc-defer-mode
  "Toggle Vc-Defer mode.

Interactively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Vc-Defer mode is enabled, the backends in
'vc-defer-backends' are temporarily removed from
'vc-handled-backends' when reopening files, which speeds things
up at the cost of not displaying the VC state in the mode
line. Subsequent VC commands, such as \\[vc-diff], do work as usual
-- the VC state is determined when needed.  It also possible to
refresh the state explicitly by executing \\[vc-refresh-state]."
  :global t                           ; This mode is not buffer local.
  :lighter " VcDefer"
  (if vc-defer-mode
      (vc-dever--turn-on)
    (vc-dever--turn-off)))

(defun vc-defer-unload-function ()
  "Unload Vc-Defer.

This turns the mode off."
  (vc-defer-mode nil))

(defun vc-defer--trace ()
  "Trace the calls to key functions related to `vc-defer' mode.

The aim is to facilitate debugging.  This function is not
interactive; it is intended for experts only.  Use
\\[eval-expression] or an equivalent mechanism to invoke it.

First, \\[global-auto-revert-mode] is turned off.  Second, a set
of interesting commands are traced with
\\[trace-function-background].  See the results in the buffer
`trace-buffer'.

Use \\[untrace-all] to turn tracing off.  If you want to turn
global auto reverts back on, use \\[global-auto-revert-mode]."
  (interactive)
  (global-auto-revert-mode 0)
  (dolist (func
           '(after-find-file
             ;; auto-revert-handler is quite noisy, so it is normally
             ;; disabled.
             ;; auto-revert-handler
             vc-backend
             vc-deduce-fileset
             vc-defer--deduce-fileset-around
             vc-defer--refresh-deferred-state
             vc-defer--refresh-deferred-state
             vc-defer--refresh-state-after
             vc-defer--remove-backends-around
             vc-file-clearprops
             vc-file-getprop
             vc-file-setprop
             vc-refresh-state))
    (trace-function-background func)))

(provide 'vc-defer)

;;; vc-defer.el ends here
