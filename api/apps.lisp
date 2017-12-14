(in-package :mastodon.api)

(defparameter *access-token* nil
  "currently loaded access token")

(defparameter *client-id* nil
  "currently loaded app name")

(defparameter *client-key* nil
  "currently loaded client key")

(defparameter *client-secret* nil
  "currently loaded client secret")

(defparameter *config-dir* "./config/"
  "path to where the tokens are saved")

(defun set-config-dir (new-dir)
  (ensure-directories-exist new-dir)
  (setq *config-dir* new-dir))

(defun load-tokens-for-user (username &key config-dir instance)
  (when config-dir (set-config-dir config-dir))
  (when instance (set-instance instance))
  (let ((config (load-config (replace-all *instance* "https://" ""))))
    (block setting-tokens
      (dolist (tokens config)
	(setq *client-id* (cdr (assoc :id tokens)))
	(setq *client-key* (cdr (assoc :client-key tokens)))
	(setq *client-secret* (cdr (assoc :client-secret tokens)))
	(dolist (login (cdr (assoc :logins tokens)))
	  (when (string= (cdr (assoc :username login))
			 username)
	    (progn 
	      (setq *access-token* (cdr (assoc :access-token login)))
	      (return-from setting-tokens))))))))

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
						 ("redirect_uris" . ,redirect-uri)
						 ("scopes" . ,(format nil "~{~a~^ ~}" scopes))
						 ,(when website `("website" . ,website))))))))
    (setq *client-id* (cdr (assoc :id tokens)))
    (setq *client-key* (cdr (assoc :client--id tokens)))
    (setq *client-secret* (cdr (assoc :client--secret tokens)))
    (write-client-tokens)))

(defun login (user-email password &key (save-token t) instance)
  "tries to log in to *INSTANCE* with the provided USER-EMAIL and PASSWORD
if SAVE-TOKEN is non-nil the tokens will be written out to a config file"
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
    (when save-token
      (write-access-tokens :username (cdr (assoc :acct (verify-credentials)))))))
    
