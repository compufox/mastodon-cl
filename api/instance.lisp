(in-package :mastodon.api)

(defparameter *instance* "https://mastodon.social"
  "the instance that we should use, defaults to mastodon.social")

(defun get-instance ()
  "get information about the current instance"
  (decode-json-from-string
   (masto--perform-request '(:get "instance"))))

(defun set-instance (instance)
  (if (not (string= *instance* (replace-all instance "https://" "")))
      (progn
	(setq *instance* (if (not (find "https://" instance))
			     (concatenate 'string "https://" instance)
			     instance))
	(handler-case (get-instance)
	  (json:json-syntax-error () (error 'invalid-instance :reason "the instance url entered is not a valid mastodon instance"))))))
