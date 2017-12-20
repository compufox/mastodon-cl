(in-package :mastodon.api)

(defparameter *client-id* nil
  "currently loaded app name")

(defparameter *client-key* nil
  "currently loaded client key")

(defparameter *client-secret* nil
  "currently loaded client secret")


(defun register-application (name &key instance (redirect-uri "urn:ietf:wg:oauth:2.0:oob")
				    (scopes (list "read")) website)
  (setq *client-key* nil)
  (setq *client-secret* nil)
  (setq *access-token* nil)
  
  (when instance (set-instance instance))
  
  (unless name (error 'api-error :reason "application name needs to be specified"))
  (let ((tokens (decode-json-from-string
		 (masto--perform-request `(:post "apps" :content
						(("client_name" . ,name)
						 ("scopes" . ,(format nil "~{~a~^ ~}" scopes))
						 ("redirect_uris" . ,redirect-uri)
						 ("website" . ,(or website ""))))))))
    (setq *client-id* (cdr (assoc :id tokens)))
    (setq *client-key* (cdr (assoc :client--id tokens)))
    (setq *client-secret* (cdr (assoc :client--secret tokens)))
    (write-client-tokens)))
    
