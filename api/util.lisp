(in-package :mastodon.api)

(defun masto--api-path (path)
  (concatenate 'string test-instance "/api/v1/" path))

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
				 :headers (("Authorization" . ,(concatenate 'string "Bearer " test-access-token))))
			      args))))

(defun merge-string-list (string-list &optional (format "~a"))
  (if (listp string-list)
      (with-output-to-string (s)
	(dolist (item string-list)
	      (format s format item)))))

(defun empty-stringp (string)
  (= (length (string-trim '(#\Space #\Newline #\Tab) string)) 0))
