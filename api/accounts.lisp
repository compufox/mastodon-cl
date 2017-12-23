(in-package :mastodon.api)

(defclass account ()
  ((id :initarg :id
       :reader account-id)
   (username :initarg :username
	     :reader account-username)
   (acct :initarg :acct
	   :reader account-acct)
   (display-name :initarg :name
		 :reader account-display-name)
   (locked :initarg :locked
	   :reader account-locked?)
   (created-at :initarg :created-at
	       :reader account-created-at)
   (follower-count :initarg :follower-count
		   :reader account-follower-count)
   (following-count :initarg :following-count
		    :reader account-following-count)
   (statuses-count :initarg :status-count
		   :reader account-status-count)
   (relationship :initarg :ship
		 :reader account-relationship)
   (bio :initarg :bio
	:reader account-bio)
   (url :initarg :url
	:reader account-url)
   (avatar :initarg :avatar
	   :reader account-avatar-url)
   (avatar-static :initarg :avatar-static
		  :reader account-avatar-static-url)
   (header :initarg :header
	   :reader account-header-url)
   (header-static :initarg :header-static
		  :reader account-header-static-url)
   (moved-to-account :initarg :moved
		     :reader account-moved?)))

(defclass relationship ()
  ((following :initarg :following
	      :reader following?)
   (followed-by :initarg :followed-by
		:reader follow-me?)
   (blocking :initarg :blocking
	     :reader blocked?)
   (muting :initarg :muting
	   :reader muted?)
   (muting-notifications :initarg :muting-notifs
			 :reader muting-notifications?)
   (follow-requested :initarg :follow-req
		     :reader follow-requested?)
   (domain-blocked :initarg :domain-blocking
		   :reader domain-blocked?)))

(defun make-relationship (raw-ship)
  (make-instance 'relationship
		 :following (cdr (assoc :following raw-ship))
		 :followed-by (cdr (assoc :followed--by raw-ship))
		 :blocking (cdr (assoc :blocking raw-ship))
		 :muting (cdr (assoc :muting raw-ship))
		 :muting-notifs (cdr (assoc :muting--notifications raw-ship))
		 :follow-req (cdr (assoc :requested raw-ship))
		 :domain-blocking (cdr (assoc :domain--blocking raw-ship))))

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
		     :moved (cdr (assoc :moved--to--account raw-account))
		     :ship (get-relationship (cdr (assoc :id raw-account)))
		     )
      nil))

(defmethod account-requested-follow ((acct account))
  (follow-requested? (account-relationship acct)))

(defmethod account-muted ((acct account))
  (muted? (account-relationship acct)))

(defmethod blocking-account-domain ((acct account))
  (domain-blocked? (account-relationship acct)))

(defmethod following-account? ((acct account))
  (following? (account-relationship acct)))

(defmethod print-object ((obj account) out)
  (format out "~a" (account-acct obj)))


(defun get-account (id)
  (make-account
   (decode-json-from-string
    (masto--perform-request `(:get
			     ,(concatenate 'string
					   "accounts/" id))))))

(defun get-current-user ()
  (make-account
   (decode-json-from-string
    (masto--perform-request '(:get "accounts/verify_credentials")))))

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

(defun get-relationship (ids)
  (unless (listp ids) (setq ids (list ids)))
  (let ((raw-ships (decode-json-from-string
		    (masto--perform-request `(:get ,(concatenate 'string
								"accounts/relationships?"
								(format nil "~{id[]=~a~^&~}" ids)))))))
    (labels ((make-ships (ships)
	       (if (cdr ships)
		   (cons (make-relationship (car ships)) (make-ships (rest ships)))
		   (cons (make-relationship (car ships)) nil))))
      (make-ships raw-ships))))
					 
(defun get-followers (id &key max-id since-id (limit 40))
  (setq limit (write-to-string (min limit 80)))
  (let ((raw-accounts (decode-json-from-string
		       (masto--perform-request `(:get
						,(concatenate 'string
							      "accounts/" id "/followers"
							      "?limit=" limit
							      (if max-id (concatenate 'string "&max_id=" max-id))
							      (if since-id (concatenate 'string "&since_id=" since-id))))))))
    (loop
       for acct in raw-accounts
       collect (make-account acct))))

(defun get-follows (id &key max-id since-id (limit 40))
  (setq limit (write-to-string (min limit 80)))
  (decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "accounts/" id "/following"
					  "?limit=" limit
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun get-statuses (id &key exclude-replies pinned only-media max-id since-id (limit 20))
  (setq limit (write-to-string (min limit 40)))
  (let ((raw-statuses (decode-json-from-string
		       (masto--perform-request `(:get
						,(concatenate 'string
							      "accounts/" id "statuses"
							      "?limit=" limit
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
  (setq limit (write-to-string (min limit 80)))
  (decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "accounts/search?q=" query
					  "&limit=" limit)))))
