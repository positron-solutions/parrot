;;; parrot-rotate.el --- Process sentinel support for parrot.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Positron Solutions

;; Author: Psionik K <73710933+psionic-k@users.noreply.github.com>
;; URL: https://github.com/positron-solutions/parrot
;; Version: 2.0.0
;; Package-Requires: ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This sub-package adds the progress functionality, which can watch arbitrary
;; processes using the process-sentinel.

;;; Code:

(require 'magit)

(defcustom parrot-party-on-magit-push t
  "If non-nil a parrot will party during magit push operations."
  :group 'parrot
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (featurep 'parrot) parrot-mode)
           (parrot--maybe-advise-magit-push))))

(defcustom parrot-party-on-org-todo-states '("DONE")
  "If non-nil, these org todo states will trigger party.
This will happen whenever the `org-after-todo-state-change' hook
is called.  See `org-todo-keywords'."
  :group 'parrot
  :type '(repeat string)
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (featurep 'parrot) parrot-mode)
           (parrot--maybe-add-todo-hook))))

(declare-function parrot-start-animation "parrot.el")
(defun parrot-progress ()
  "Start a persistent parrot animation.
Use `parrot-progress-finished' to stop."
  (parrot-start-animation t t))

(declare-function parrot-stop-animation "parrot.el")
(defun parrot-progress-finished (&rest _args)
  "Stop persistent progress animation."
  (parrot-stop-animation))

(defun parrot--party-while-process (process)
  "Party until the provided process completes.

PROCESS the git process Replace sentinel for PROCESS, usually
`magit-process-sentinel' and modify it to call our own sentinel
just until this PROCESS is finished."
  (if-let ((sentinel (process-sentinel process)))
      (set-process-sentinel process
                            (lambda (&rest args)
                              (apply sentinel args)
                              (apply #'parrot-progress-finished args)))
    (set-process-sentinel process #'parrot-progress-finished))
  (parrot-progress))

(defun parrot--sentinel (process _event)
  "When process ends, stop the parrot.
PROCESS is the running process indicated by this parrot.
EVENT a bit of detail about current state"
(when (memq (process-status process) '(exit signal))
  (parrot-progress-finished)))

(defun parrot--magit-push-filter (fun &rest args)
  "If the git command is a push, add a process ending listener.
FUN is usually `magit-run-git-async'
ARGS is args for `magit-run-git-async'"
  (if-let* ((process (apply fun args))
            (command (car args)))
      (progn (when (and (stringp command) (string= "push" command))
               (parrot--party-while-process process))
             process)))

(defun parrot--maybe-advise-magit-push ()
  "Update advice for magit push.
See `parrot-party-on-magit-push'."
  (if (and parrot-mode
           parrot-party-on-magit-push)
      (advice-add 'magit-run-git-async :around #'parrot--magit-push-filter)
    (advice-remove 'magit-run-git-async #'parrot--magit-push-filter)))

(defun parrot--todo-party ()
  "Start the animation depending on the last set todo state.
Use `party-on-org-todo-states' to control partying or not."
  (when (member org-state parrot-party-on-org-todo-states)
    (parrot-start-animation)))

(defun parrot--maybe-add-todo-hook ()
  "Update hook for org mode todos.
See `parrot-party-on-org-todo-states'."
  (if (and parrot-party-on-org-todo-states parrot-mode)
      (add-hook 'org-after-todo-state-change-hook
                #'parrot--todo-party)
    (remove-hook 'org-after-todo-state-change-hook
                 #'parrot--todo-party)))

(provide 'parrot-progress)
;;; parrot-progress.el ends here
