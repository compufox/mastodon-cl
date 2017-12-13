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

(defun write-config (&key username overwrite)
  (with-open-file (config-file (concatenate 'string "./" (replace-all *instance* "https://" "") ".config")
			       :direction :output
			       :if-exists (if overwrite :overwrite :append)
			       :if-does-not-exist :create)
    (format config-file "~S~%" `((:client-key . ,*client-key*)
				 (:client-secret . ,*client-secret*)
				 ,(when *access-token* `(:access-token . ,*access-token*))
				 ,(when username `(:username . ,username))))))

(defun merge-string-list (string-list &optional (format "~a"))
  (if (listp string-list)
      (with-output-to-string (s)
	(dolist (item string-list)
	      (format s format item)))))

(defun empty-stringp (string)
  (= (length (string-trim '(#\Space #\Newline #\Tab) string)) 0))

