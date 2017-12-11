(in-package :mastodon.api)

(defun get-blocks (&key max-id since-id (limit 40))
  (setq limit (min limit 80))
  (decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "blocks"
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun get-domain-blocks (&key max-id since-id (limit 40))
  (setq limit (min limit 80))
  (decode-json-from-string
   (masto--perform-request `(:get
			    ,(concatenate 'string
					  "domain_blocks"
					  "?limit=" (write-to-string limit)
					  (if max-id (concatenate 'string "&max_id=" max-id))
					  (if since-id (concatenate 'string "&since_id=" since-id)))))))

(defun block-domain (domain)
  (masto--perform-request `(:post 
			   ,(concatenate 'string
					 "domain_blocks"
					 "?domain=" domain))))

(defun unblock-domain (domain)
  (masto--perform-request `(:delete
			   ,(concatenate 'string
					 "domain_blocks"
					 "?domain=" domain))))
