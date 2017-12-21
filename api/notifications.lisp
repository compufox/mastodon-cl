(in-package :mastodon.api)

(defclass notification ()
  ((id :initarg :id
       :reader notification-id)
   (type :initarg :type
	 :reader notification-type)
   (created-at :initarg :created-at
	       :reader notification-created-at)
   (account :initarg :acct
	    :reader notification-sending-account)
   (status :initarg :status
	   :reader notification-status)))

(defun make-notification (raw-notification)
  (make-instance 'notification
		 :id (cdr (assoc :id raw-notification))
		 :type (cdr (assoc :type raw-notification))
		 :created-at (cdr (assoc :created--at raw-notification))
		 :acct (make-account (cdr (assoc :account raw-notification)))
		 :status (make-status (cdr (assoc :status raw-notification)))))


(defmethod notification-dismiss ((notif notification))
  (dismiss-notification (notification-id notif)))


(defun get-notifications (&key max-id since-id (limit 15) exclude-types)
  (setq limit (min limit 30))
  (let ((notifs (decode-json-from-string
		   (masto--perform-request `(:get
					    ,(concatenate 'string
							  "notifications"
							  "?limit=" (write-to-string limit)
							  (if max-id (concatenate 'string "&max_id=" max-id))
							  (if since-id (concatenate 'string "&since_id=" since-id))
							  (if exclude-types (loop for type in exclude-types
									       collect (format t "&exclude_type[]=~A" type)))))))))
  (loop
     for notif in notifs
     collect (make-notification notif))))

(defun get-notification (id)
   (decode-json-from-string
    (masto--perform-request `(:get
			     ,(concatenate 'string
					   "notifications/"
					   id)))))

(defun clear-notifications ()
  (masto--perform-request '(:post "notifications/clear")))

(defun dismiss-notification (id)
  (masto--perform-request `(:post "notifications/dismiss"
				 :content (("id" . ,id)))))
