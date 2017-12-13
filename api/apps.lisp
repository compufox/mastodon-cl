(in-package :mastodon.api)

(defparameter *access-token* nil
  "the token that will allow us to access the masto instance")

(defparameter *client-key* nil
  "currently loaded client key")

(defparameter *client-secret* nil
  "currently loaded client secret")

(defun register-application (name &key instance (redirect-uri "urn:ietf:wg:oauth:2.0:oob")
				    (scopes (list "read")) website save-tokens)
  (setq *client-key* nil)
  (setq *client-secret* nil)
  (setq *access-token* nil)
  (when instance (set-instance instance))
  
  (unless name (error 'api-error :reason "application name needs to be specified"))
  (let ((tokens (decode-json-from-string
		 (masto--perform-request `(:post "apps" :content
						(("client_name" . ,name)
						 ("redirect_uris" . ,redirect-uri)
						 ("scopes" . ,scopes)
						 ,(when website `("website" . ,website))))))))
    (setq *client-key* (cdr (assoc :client--id tokens)))
    (setq *client-secret* (cdr (assoc :client--secret tokens)))
    (when save-tokens
      (write-config))))

(defun login (user-email password &key (write-out t) instance)
  "tries to log in to *INSTANCE* with the provided USER-EMAIL and PASSWORD
if WRITE-OUT is non-nil the tokens will be written out to a config file"
  (when instance
    (set-instance instance))
  
  (let ((token (decode-json-from-string
		(masto--perform-request `(:post "oauth/token" :content
					       (("client_id" . ,*client-key*)
						("client_secret" . ,*client-secret*)
						("grant_type" . "password")
						("username" . ,user-email)
						("password" . ,password)))))))
    (setq *access-token* (cdr (assoc :access--token token)))
    (when write-out
      (write-config :overwrite t :username (cdr (assoc :acct (verify-credentials)))))))
    
