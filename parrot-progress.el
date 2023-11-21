;;; parrot-progress.el --- Process sentinel support for parrot.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Positron Solutions

;; Author: Psionic K <contact@positron.solutions>
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

(declare-function parrot-start-animation "parrot")
(defun parrot--progress ()
  "Start a persistent parrot animation.
Use `parrot-progress-finished' to stop."
  (parrot-start-animation t t))

(declare-function parrot-stop-animation "parrot")
(defun parrot--progress-finished (&rest _args)
  "Stop persistent progress animation."
  (parrot-stop-animation))

(defun parrot-party-while-process (process)
  "Party until the provided process completes.

PROCESS the git process Replace sentinel for PROCESS, usually
`magit-process-sentinel' and modify it to call our own sentinel
just until this PROCESS is finished."
  (if-let ((sentinel (process-sentinel process)))
      (set-process-sentinel process
                            (lambda (&rest args)
                              (apply sentinel args)
                              (apply #'parrot--progress-finished args)))
    (set-process-sentinel process #'parrot--progress-finished))
  (parrot--progress))

(defun parrot--sentinel (process _event)
  "When process ends, stop the parrot.
PROCESS is the running process indicated by this parrot.
EVENT a bit of detail about current state"
(if (memq (process-status process) '(exit stop exit failed signal closed nil))
    (parrot--progress-finished)
  (unless (bound-and-true-p parrot--animation-timer) (parrot--progress))))

(provide 'parrot-progress)
;;; parrot-progress.el ends here
