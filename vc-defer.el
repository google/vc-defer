;;; vc-defer.el --- Defer non-essential vc.el work -*- lexical-binding: t; -*-

;; Copyright 2019 Google LLC

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Matt Armstrong <marmstrong@google.com>
;; Maintainer: Tom Fitzhenry <tomfitzhenry@google.com>
;; Keywords: vc tools
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.1
;; URL: https://github.com/google/vc-defer

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

(defvar vc-defer--backends-are-trimmed nil
  "Non-nil when `vc-defer-backends` has been trimmed by this package.

This variable is set dynamically as a means of communication
between different pieces of advice.")

(defun vc-defer--remove-backends (original)
  "Return a copy of ORIGINAL after effecting `vc-defer-backends'.

This truncates ORIGIONAL after the first backend from
`vc-defer-backends'.  This way, no file will be assigned a
backend that might have been handled by one of the deferred ones."
  (let (filtered)
    (catch 'early
      (dolist (element original filtered)
        (if (member element vc-defer-backends)
            (throw 'early filtered)
          (setq filtered (cons element filtered)))))))

(defun vc-defer--defer-around (orig-fun &rest args)
  "Call ORIG-FUN with ARGS with possibly fewer VC backends.

The set of VC backends is trimmed according to
`vc-defer-backends' unless the current buffer local variable
`buffer-file-name' is nil, or is in a mode that VC treats
specially.

Otherwise, temporarily binds `vc-handled-backends' to a copy with
all backends that occur after any element of `vc-defer-backends'
removed, disabling those backends for the call.

See `vc-defer--refresh-state-after' for related logic.

This function is intended to be used as \"around\" advice for
common Emacs functions such as `auto-revert-handler' and
`after-find-file'."
  (let* ((vc-defer--backends-are-trimmed
          ;; Avoid deferring in various scenarios that complicate
          ;; things significantly.  See `vc-deduce-fileset' for the
          ;; set of conditions where a VC action doesn't necessarily
          ;; correspond to the current buffer's file.
          (not (or (not buffer-file-name)
                   (derived-mode-p 'dired-mode)
                   (derived-mode-p 'log-view-mode)
                   (derived-mode-p 'vc-dir-mode))))
         (vc-handled-backends
          (if vc-defer--backends-are-trimmed
              (vc-defer--remove-backends vc-handled-backends)
            vc-handled-backends)))
    (apply orig-fun args)))

(defun vc-defer--setprop (file value)
  (vc-file-setprop file 'vc-defer--x-deferred value))

(defun vc-defer--getprop (file)
  (vc-file-getprop file 'vc-defer--x-deferred))

(defun vc-defer--refresh-state-after (&rest _args)
  "Deal with after advice for `vc-refresh-state'.

When `vc-defer-mode' is on this is run as advice after
`vc-refresh-state'.

This sets the per-file VC property `vc-defer--x-deferred' to t if
this buffer's file has no VC backend and this is due, possibly,
to the trimmed set of VC backends set by this package."
  (if (and vc-defer--backends-are-trimmed
           (stringp buffer-file-name)
           (not vc-mode))
      (vc-defer--setprop buffer-file-name t))
  nil)

(defun vc-defer--refresh-deferred-state (buffers)
  "Call `vc-refresh-state' on BUFFERS.

Refresh the VC state of all BUFFERS whose file has the per-file
VC property `vc-defer--x-deferred' set (possibly clearing that
property, and setting the actual VC backend."
  (dolist (buffer buffers)
    (let ((file (buffer-file-name buffer)))
      (when (and file (vc-defer--getprop file))
        (with-current-buffer buffer
          (with-temp-message
              (format "Refreshing VC state for buffer %s..." buffer)
            (vc-defer--setprop file nil)
            (vc-refresh-state)))))))

(defun vc-defer--deduce-fileset-around (orig-fun &rest args)
  "Call ORIG-FUN with ARGS after performing deferred actions.

This function is installed as \"around\" advice for
`vc-deduce-fileset`, which see for more information.

First calls `vc-refresh-state' either on the current buffer or on
all deferred buffers.  In the latter case, this may well be
overkill, but the logic in `vc-deduce-fileset` is complex, which
makes behavior that is closer to optimal difficult."
  ;; Refresh deferred state in all buffers for the conditions where
  ;; `vc-deduce-fileset' may produce a set of files, since we want it
  ;; to have correct info about all open buffers.  See similar logic
  ;; in `vc-defer--defer-around'.  Otherwise, refresh only
  ;; the current buffer.
  ;;
  ;; Rather than refreshing state for all buffers, a possible
  ;; improvement might be to advise `vc-dir-deduce-fileset`,
  ;; `vc-dired-deduce-fileset` and handle those as special cases.
  ;; This would be close to re-implementing the logic outside the
  ;; package.  Preferable would be integration of vc-defer features
  ;; into vc itself.
  (vc-defer--refresh-deferred-state
   (if (or (derived-mode-p 'dired-mode)
           (derived-mode-p 'log-view-mode)
           (derived-mode-p 'vc-dir-mode))
       (buffer-list)
     (list (current-buffer))))
  (apply orig-fun args))

(defun vc-defer--advice-add ()
  "Add all advice needed by Vc-Defer mode."
  (advice-add 'auto-revert-handler :around 'vc-defer--defer-around)
  (advice-add 'after-find-file :around 'vc-defer--defer-around)
  (advice-add 'vc-deduce-fileset :around 'vc-defer--deduce-fileset-around)
  (advice-add 'vc-refresh-state :after 'vc-defer--refresh-state-after))

(defun vc-defer--advice-remove ()
  "Remove all advice added by Vc-Defer mode."
  (advice-remove 'auto-revert-handler 'vc-defer--defer-around)
  (advice-remove 'auto-revert-handler 'vc-defer--defer-around)
  (advice-remove 'vc-deduce-fileset 'vc-defer--deduce-fileset-around)
  (advice-remove 'vc-refresh-state 'vc-defer--refresh-state-after))

(defun vc-defer--turn-on ()
  "Turn Vc-Defer mode on."
  (vc-defer--advice-add))

(defun vc-defer--turn-off ()
  "Turn Vc-Defer mode off."
  (vc-defer--advice-remove)
  (vc-defer--refresh-deferred-state (buffer-list)))

;;;###autoload
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
      (vc-defer--turn-on)
    (vc-defer--turn-off)))

(defun vc-defer-unload-function ()
  "Unload Vc-Defer.

Called by \\[unload-feature].  This turns the mode off, which has
the effect of uninstalling all advice and hooks."
  (vc-defer-mode -1))

(defun vc-defer--trace ()
  "Trace the calls to key functions related to `vc-defer' mode.

The aim is to facilitate debugging.  This function is not
interactive; it is intended for experts only.  Use
\\[eval-expression] or an equivalent mechanism to invoke it.

First, \\[global-auto-revert-mode] is turned off, which prevents
periodic invocation of VC functions.  Second, a set of
interesting commands are traced with
\\[trace-function-background].  See the results in the buffer
`trace-buffer'.

Use \\[untrace-all] to turn tracing off.  If you want to turn
global auto reverts back on, use \\[global-auto-revert-mode]."
  (interactive)
  (let ((was-enabled vc-defer-mode))
    (vc-defer-mode -1)
    ;; Turn off global-auto-revert-mode, auto-revert-mode, and
    ;; auto-revert-tail-mode everywhere.  These run on a timer, and so
    ;; are very intrusive in the trace output.
    (global-auto-revert-mode 0)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (auto-revert-mode -1)
        (auto-revert-tail-mode -1)))
    (dolist (func
             '(after-find-file
               ;; auto-revert-handler is quite noisy, so do not trace it
               ;; by default.
               ;;
               ;; auto-revert-handler
               vc-backend
               vc-deduce-fileset
               vc-file-clearprops
               vc-file-getprop
               vc-file-setprop
               vc-refresh-state))
      (trace-function-background func))
    (mapatoms #'(lambda (symbol)
                  (if (and (functionp symbol)
                           (string-prefix-p "vc-defer-" (symbol-name symbol)))
                      (trace-function-background symbol))))
    (when was-enabled
      (vc-defer-mode 1))))

(provide 'vc-defer)

;;; vc-defer.el ends here
