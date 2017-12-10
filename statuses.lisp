(in-package #:mastodon)

(defun fave-status (id)
  (masto--perform-request `(:post ,(concatenate 'string
					       "statuses/"
					       id
					       "/favourite"))))

(defun unfave-status (id)
  (masto--perform-request `(:post ,(concatenate 'string
					       "statuses/"
					       id
					       "/unfavourite"))))

(defun reblog-status (id)
  (masto--perform-request `(:post ,(concatenate 'string
					       "statuses/"
					       id
					       "/reblog"))))

(defun unreblog-status (id)
  (masto--perform-request `(:post ,(concatenate 'string
					       "statuses/"
					       id
					       "/unreblog"))))
(defun pin-status (id)
  (masto--perform-request `(:post ,(concatenate 'string
					       "statuses/"
					       id
					       "/pin"))))

(defun unpin-status (id)
  (masto--perform-request `(:post ,(concatenate 'string
					       "statuses/"
					       id
					       "/unpin"))))
    
(defun delete-status (id)
  (masto--perform-request `(:delete ,(concatenate 'string "statuses/" id))))

(defun get-reblog-count (id &key max-id since-id (limit 40))
  (cl-json:decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "statuses/" ,id "/reblogged_by"
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun get-favourite-count (id &key max-id since-id (limit 40))
  (cl-json:decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "statuses/" ,id "/favourited_by"
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun mute-conversation (id)
  (masto--perform-request `(:post
			   ,(concatenate 'string
					 "statuses/" ,id "/mute"))))

(defun unmute-conversation (id)
  (masto--perform-request `(:post
			   ,(concatenate 'string
					 "statuses/" ,id "/unmute"))))
