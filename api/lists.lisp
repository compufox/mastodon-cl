(in-package :mastodon.api)

(defclass mastodon-list ()
  ((id :initarg :id
       :reader list-id)
   (title :initarg :title
	  :reader list-title)
   (members :initarg :members
	    :accessor list-members)))

(defun make-mastodon-list (raw-mastodon-list)
  (make-instance 'mastodon-list
		 :id (cdr (assoc :id raw-mastodon-list))
		 :title (cdr (assoc :title raw-mastodon-list))
		 :members (get-list-members (cdr (assoc :id raw-mastodon-list)))))



(defun get-list-members (id)
  (decode-json-from-string
   (masto--perform-request `(:get ,(concatenate 'string
					       "lists/"
					       id
					       "/accounts")))))
