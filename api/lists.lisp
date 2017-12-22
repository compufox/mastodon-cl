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

(defmethod print-object ((obj mastodon-list) out)
  (format out "~&Mastodon List: ~a~%Members: ~d" (list-title obj)
	  (length (list-members obj))))


(defmethod list-remove-account ((masto mastodon-list) (acct account))
  (remove-account-from-list (list-id masto) (account-id acct))
  (remove-if #'(lambda (l) (eq (account-id l) (account-id acct))) (list-members masto)))

(defmethod list-add-account ((masto mastodon-list) (acct account))
  (add-account-to-list (list-id masto) (account-id acct))
  (setf (list-members masto) (append (list-members masto)
				     masto)))

(defmethod list-delete ((masto mastodon-list))
  (delete-list (list-id masto))
  (list-id masto))

(defun create-list (title)
  (make-mastodon-list
   (decode-json-from-string
    (masto--perform-request `(:post "lists" :content
				   (("title" . ,title)))))))

(defun delete-list (id)
  (masto--perform-request `(:delete ,(concatenate 'string "lists/" id))))

(defun update-list-title (id title)
  (masto--perform-request `(:put ,(concatenate 'string "lists/" id)
				:content (("title" . ,title)))))

(defun get-list (id)
  (make-mastodon-list
   (decode-json-from-string
    (masto--perform-request `(:get ,(concatenate 'string
						"lists/" id))))))

(defun get-list-members (id &key (limit 0))
  (setq limit (write-to-string (min limit 50)))
  (let ((raw-accounts (decode-json-from-string
		       (masto--perform-request `(:get ,(concatenate 'string
								   "lists/"
								   id
								   "/accounts?limit=" limit))))))
    (labels ((make-accounts (accts)
	       (if (cdr accts)
		   (cons (make-account (car accts)) (make-accounts (rest accts)))
		   (cons (make-account (car accts)) nil))))
      (make-accounts raw-accounts))))

(defun add-account-to-list (list-id acct-id)
  (masto--perform-request `(:post ,(concatenate 'string
					       "lists/"
					       list-id
					       "/accounts")
				 :content (("account_ids" . ,acct-id)))))

(defun remove-account-from-list (list-id acct-id)
  (masto--perform-request `(:delete ,(concatenate 'string
						 "lists/"
						 list-id
						 "/accounts")
				   :content (("account_ids" . ,acct-id)))))
