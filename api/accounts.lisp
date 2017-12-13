(in-package :mastodon.api)

(defun get-account (id)
  (decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "accounts/" id)))))

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
  (setq limit (min limit 80))
  (decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "accounts/" id "/followers"
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun get-account-follows (id &key max-id since-id (limit 40))
  (setq limit (min limit 80))
  (decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "accounts/" id "/following"
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun get-account-statuses (id &key exclude-replies pinned only-media max-id since-id (limit 20))
  (setq limit (min limit 40))
  (decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "accounts/" id "statuses"
					  "?limit=" (write-to-string limit)
					  (if exclude-replies "&exclude_replies=true")
					  (if pinned "&pinned=true")
					  (if only-media "&only_media=true")
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun search-accounts (query &key (limit 40))
  (setq limit (mint limit 80))
  (decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "accounts/search?q=" query
					  "&limit=" (write-to-string limit))))))
