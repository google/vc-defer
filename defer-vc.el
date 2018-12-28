;;; defer-vc.el --- description                       -*- lexical-binding: t; -*-

;; Copyright 2018 Google LLC

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

(defcustom defer-vc-backends '(Hg)
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

(defun defer-vc-remove-backends (original)
  "Return a copy of ORIGINAL with `defer-vc-backends' removed."
  (let (filtered)
    (dolist (element original filtered)
      (unless (member element defer-vc-backends)
        (setq filtered (cons element filtered))))))

(defun defer-vc-remove-backends-around (orig-fun &rest args)
  "Call ORIG-FUN with ARGS, with a trimmed set of actieve VC backends.

Temporarily binds `vc-handled-backends' to a copy of itself with
`defer-vc-backends' removed, disabling those backends for the
call.  If VC fails to find a backend for the buffer, set
`defer-vc-deferred' on the buffer, which arranges for it to be
called again later.

This function is intended to be used as \"around\" advice for
common Emacs functions such as `auto-revert-handler' and
`after-find-file'."
  (let ((vc-handled-backends
         (defer-vc-remove-backends vc-handled-backends)))
    (apply orig-fun args)
    (if (not (vc-backend buffer-file-name))
        (setq defer-vc-deferred t))))

(defun defer-vc-refresh-state ()
  "Call `vc-refresh-state' on all deferred buffers.

All buffers with the buffer local variable `defer-vc-deferred'
set will have fresh VC state after this call."
  (with-temp-message "Refreshing VC state for deferred buffers..."
    (dolist (buffer (buffer-list))
      (when (buffer-local-value 'defer-vc-deferred buffer)
        (with-current-buffer buffer
          (setq defer-vc-deferred nil)
          (vc-refresh-state))))))

(defun defer-vc-refresh-state-around (orig-fun &rest args)
  "Call ORIG-FUN with ARGS after performing deferred actions.

If `defer-vc-deferred' is set on any buffers, first call
`vc-refresh-state' on all of them.

This function is intended to be used as \"around\" advice for VC
functions that otherwise expect `vc-refresh-state' to have
already been called."
  (defer-vc-refresh-state)
  (apply orig-fun args))

(defconst defer-vc-remove-backend-around-funcs
  '(auto-revert-handler after-find-file))
(defconst defer-vc-refresh-state-around-funcs
  '(vc-deduce-fileset))

(defun defer-vc-advice-add-many (functions around-function)
  "Add advice to a list of functions.
AROUND-FUNCTION describes the advice to add.  Each function in
FUNCTIONS will have `:around' advice added to it."
  (dolist (f functions)
    (advice-add f :around around-function)))

(defun defer-vc-advice-remove-many (functions around-function)
  "Remove advice from a list of functions.
AROUND-FUNCTION describes the advice to remove.  Each function in
FUNCTIONS will have the advice removed."
  (dolist (f functions)
    (advice-remove f around-function)))

(defun defer-vc-advice-add ()
  "Add all advice needed by Defer-Vc mode."
  (defer-vc-advice-add-many
    defer-vc-remove-backend-around-funcs 'defer-vc-remove-backends-around)
  (defer-vc-advice-add-many
    defer-vc-refresh-state-around-funcs 'defer-vc-refresh-state-around))

(defun defer-vc-advice-remove ()
  "Remove all advice added by Defer-Vc mode."
  (defer-vc-advice-remove-many
    defer-vc-remove-backend-around-funcs 'defer-vc-remove-backends-around)
  (defer-vc-advice-remove-many
    defer-vc-refresh-state-around-funcs 'defer-vc-refresh-state-around))

(defun defer-vc-turn-on ()
  "Turn Defer-Vc mode on."
  (defer-vc-advice-add))

(defun defer-vc-turn-off ()
  "Turn Defer-Vc mode off."
  (defer-vc-advice-remove)
  (defer-vc-refresh-state))

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

(provide 'defer-vc)

;;; defer-vc.el ends here
