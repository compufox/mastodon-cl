(in-package #:mastodon)

(defun get-notifications (&key max-id since-id (limit 15) exclude-types)
  (setq limit (min limit 30))
  (cl-json:decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "notifications"
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id))
					  (if exclude-types (loop for type in exclude-types
							       collect (format t "&exclude_type[]=~A" type))))))))

(defun get-notification (id)
  (cl-json:decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "notifications/"
					  id)))))

(defun clear-notifications ()
  (masto--perform-request '(:post "notifications/clear")))

(defun dismiss-notification (id)
  (masto--perform-request `(:post "notifications/dismiss"
				 :content (("id" . ,id)))))
