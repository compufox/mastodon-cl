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
