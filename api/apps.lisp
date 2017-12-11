(in-package :mastodon.api)

(defun register-application (&key name (redirect-uri "urn:ietf:wg:oauth:2.0:oob")
			       (scopes "read") website)
  (unless name (error 'api-error :reason "application name needs to be specified"))
  (decode-json-from-string
   (masto--perform-request `(:post "apps" :content
				  (("client_name" . ,name)
				   ("redirect_uris" . ,redirect-uri)
				   ("scopes" . ,scopes)
				   ,(when website `("website" . ,website)))))))


