(in-package :mastodon.api)

(defclass account ()
  ((id :initarg :id
       :accessor account-id)
   (username :initarg :username
	     :accessor account-username)
   (acct :initarg :acct
	   :accessor account-acct)
   (display-name :initarg :name
		 :accessor account-display-name)
   (locked :initarg :locked
	   :accessor account-locked?)
   (created-at :initarg :created-at
	       :accessor account-created-at)
   (follower-count :accessor account-follower-count)
   (following-count :accessor account-following-count)
   (statuses-count :accessor account-status-count)
   (bio :initarg :bio
	:accessor account-bio)
   (url :initarg :url
	:accessor account-url)
   (avatar :initarg :avatar
	   :accessor account-avatar-url)
   (header :initarg :header
	   :accessor account-header-url)
   (moved-to-account :initarg :moved
		     :accessor account-moved?)))

(defun make-account (raw-account)
  (if (not (null raw-account))
      (make-instance 'account
		     :id (cdr (assoc :id raw-account))
		     :username (cdr (assoc :username raw-account))
		     :acct (cdr (assoc :acct raw-account))
		     :name (cdr (assoc :display--name raw-account))
		     :locked (cdr (assoc :locked raw-account))
		     :created-at (cdr (assoc :created--at raw-account))
		     :bio (cdr (assoc :note raw-account))
		     :url (cdr (assoc :url raw-account))
		     :avatar (cdr (assoc :avatar raw-account))
		     :header (cdr (assoc :header raw-account))
		     :moved (cdr (assoc :moved--to--account raw-account)))
      nil))

(defmethod print-object ((obj account) out)
  (format out "~a" (account-acct obj)))


(defun get-account (id)
  (make-account
   (decode-json-from-string
    (masto--perform-request `(:get
			     ,(concatenate 'string
					   "accounts/" id))))))

(defun get-current-user ()
  (decode-json-from-string
   (masto--perform-request '(:get "accounts/verify_credentials"))))

(setf (fdefinition 'verify-credentials) #'get-current-user)

(defun update-user (&key display-name note avatar header)
  (let ((updated-user-data `(("display_name" . ,display-name)
			     ("note" . ,note)
			     ("avatar" . ,avatar)
			     ("header" . ,header))))
    (setq updated-user-data
	  (set-exclusive-or (loop for el in updated-user-data
			       when (null (cdr el)) collect el)
			    updated-user-data))
    (masto--perform-request `(:patch "accounts/update_credentials"
				  :content ,updated-user-data))))
					 
(defun get-account-followers (id &key max-id since-id (limit 40))
  (setq limit (write-to-string (min limit 80)))
  (decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "accounts/" id "/followers"
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun get-account-follows (id &key max-id since-id (limit 40))
  (setq limit (write-to-string (min limit 80)))
  (decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "accounts/" id "/following"
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun get-account-statuses (id &key exclude-replies pinned only-media max-id since-id (limit 20))
  (setq limit (write-to-string (min limit 40)))
  (let ((raw-statuses (decode-json-from-string
		       (masto--perform-request `(:get
						,(concatenate 'string
							      "accounts/" id "statuses"
							      "?limit=" (write-to-string limit)
							      (if exclude-replies "&exclude_replies=true")
							      (if pinned "&pinned=true")
							      (if only-media "&only_media=true")
							      (if max-id (concatenate 'string "&max_id=" max-id))
							      (if since-id (concatenate 'string "&since_id=" since-id))))))))
    (labels ((make-statuses (status)
	       (if (cdr status)
		   (cons (make-status (car status)) (make-statuses (rest status)))
		   (cons (car status) nil))))
      (make-statuses raw-statuses))))

(defun search-accounts (query &key (limit 40))
  (setq limit (write-to-string (mint limit 80)))
  (decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "accounts/search?q=" query
					  "&limit=" (write-to-string limit))))))
