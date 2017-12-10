(in-package #:mastodon)

(defun get-home-timeline (&key max-id since-id (limit 20))
  (setq limit (min limit 40))
  (cl-json:decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "timelines/home"
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun get-public-timeline (&key max-id since-id (limit 20))
  (setq limit (min limit 40))
  (cl-json:decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "timelines/public"
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun get-tag-timeline (tag &key local max-id since-id (limit 20))
  (cl-json:decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "timelines/tag/" (remove #\# tag)
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id))
					  (if local (concatenate 'string "&local=true")))))))

(defun get-local-timeline (&key max-id since-id (limit 20))
  (setq limit (min limit 40))
  (cl-json:decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "timelines/public?local=true"
					  "&limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))
