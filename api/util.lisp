(in-package :mastodon.api)

(defun masto--api-path (path)
  (if (not (search "oauth" path))
      (concatenate 'string *instance* "/api/v1/" path)
      (concatenate 'string *instance* "/" path)))

(defun masto--perform-request (request)
  (let* ((mode (pop request))
	 (path (pop request))
	 (args request)
	 (req-method (case mode
		       (:get #'get)
		       (:post #'post)
		       (:put #'put)
		       (:delete #'delete)
		       (:patch #'patch))))
    (apply req-method (append `(,(masto--api-path path)
				 :headers (("Authorization" . ,(concatenate 'string "Bearer " *access-token*))
					   ("User-Agent" . ,*user-agent*)))
			      args))))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaces with replacement"
  (with-output-to-string (out)
    (loop
       with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
			 :start2 old-pos
			 :test test)
       do (write-string string out
			:start old-pos
			:end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

(defun write-client-tokens ()
  (write-config :client t))

(defun write-access-tokens (&rest args)
  (apply 'write-config :access t args))

(defun load-config (conf-name)
  (with-open-file (conf (concatenate 'string *config-dir* conf-name)
			:direction :input
			:if-does-not-exist nil)
    (when (streamp conf)
      (loop for sexp = (read conf nil)
	 while sexp
	 collect sexp))))

(defun write-config (&key client access username)
  (ensure-directories-exist *config-dir*)
  (let* ((current (load-config (concatenate 'string (replace-all *instance* "https://" "")
					    ".conf")))
	 (changed current))
    (cond
      (client
       (setq changed (append changed `(((:id . ,*client-id*)
					(:client-key . ,*client-key*)
					(:client-secret . ,*client-secret*)
					(:logins . nil))))))
      (access
       (dolist (app changed)
	 (when (and (string= *client-id* (cdr (assoc :id app)))
		    access)
	   (block checking
	     (dolist (login (cdr (assoc :logins app)))
	       (when (string= (cdr (assoc :access-token login)) *access-token*)
		 (return-from checking)))
	     (setf (cdr (assoc :logins app)) (append (cdr (assoc :logins app))
						     `(((:access-token . ,*access-token*)
							(:username . ,username))))))))))
    (with-open-file (conf-file (concatenate 'string *config-dir*
					    (replace-all *instance* "https://" "") ".conf")
			       :direction :output
			       :if-exists :overwrite
			       :if-does-not-exist :create)
      (format conf-file "~{~S~%~}" changed))))
	

(defun merge-string-list (string-list &optional (format "~a"))
  (if (listp string-list)
      (with-output-to-string (s)
	(dolist (item string-list)
	      (format s format item)))))

(defun empty-stringp (string)
  (= (length (string-trim '(#\Space #\Newline #\Tab) string)) 0))

