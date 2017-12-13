(in-package :mastodon.api)

(defvar *status-privacy-modes* '("public" "unlisted" "private" "direct")
  "the different privacy modes that a status can have")

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
  (decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "statuses/" id "/reblogged_by"
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun get-favourite-count (id &key max-id since-id (limit 40))
  (decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "statuses/" id "/favourited_by"
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun mute-conversation (id)
  (masto--perform-request `(:post
			   ,(concatenate 'string
					 "statuses/" id "/mute"))))

(defun unmute-conversation (id)
  (masto--perform-request `(:post
			   ,(concatenate 'string
					 "statuses/" id "/unmute"))))

(defun get-favourited-statuses (&key max-id since-id (limit 20))
  (setq limit (min limit 40))
  (decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "favourites"
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun post-status (status &key (visibility "public") sensitive spoiler reply-id media)
  (when (not (member visibility *status-privacy-modes* :test #'string=)) (error 'unrecognized-status-privacy))
  (masto--perform-request `(:post "statuses" :content
				 ,(concatenate 'list
					       `(("status" . ,status)
						 ("visibility" . ,visibility)
						 ("spoiler_text" . ,spoiler)
						 ("sensitive" . ,(when (not (null sensitive)) "true"))
						 ("in_reply_to_id" . ,reply-id))
					       (mass-upload-media media)))))
