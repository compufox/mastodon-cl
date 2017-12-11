;;;; mastodon.lisp
(in-package :cl-user)

(defpackage :mastodon
  (:nicknames :masto)
  (:use :cl)
  (:export

      ; status creation
      :post-status
      :delete-last-post))

(in-package :mastodon)

;;; "mastodon" goes here. Hacks and glory await!

(defvar *last-toot-id* nil
  "the id of the last status posted")

(defun delete-last-post ()
  (delete-status *last-toot-id*))
