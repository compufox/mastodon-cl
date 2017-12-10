(in-package #:mastodon)

(defun get-follow-requests (&key max-id since-id (limit 40))
  (setq limit (min limit 80))
  (cl-json:decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "follow_requests"
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun approve-follow-request (id)
  (masto--perform-request `(:post
			   ,(concatenate 'string
					 "follow_requests/"
					 id
					 "/authorize"))))

(defun reject-follow-request (id)
  (masto--perform-request `(:post
			   ,(concatenate 'string
					 "follow_requests/"
					 id
					 "/reject"))))
(defun follow-remote-user (user-uri)
  (masto--perform-request `(:post "follows"
				 :content (("uri" . ,user-uri)))))

(defun follow-user (id)
  (masto--perform-request `(:post
			   ,(concatenate 'string
					 "accounts/" id "/follow"))))

(defun unfollow-user (id)
  (masto--perform-request `(:post
			   ,(concatenate 'string
					 "accounts/" id "/unfollow"))))
