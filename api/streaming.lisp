(in-package :cl-user)

(defpackage :mastodon.streaming
  (:use :cl)
  (:import-from :cl-json
		:decode-json-from-string)
  (:shadowing-import-from :dexador
			  :get)
  (:import-from :dexador.decoding-stream
		:stream-read-char)
  (:import-from :mastodon.api
		:masto--perform-request
		:merge-string-list
		:empty-stringp)
		
  (:export :stream-home
	   :stream-local
	   :stream-public
	   :stream-tag
	   :stream-notifications))

(in-package :mastodon.streaming)

(intern "UPDATE")
(intern "DELETE")
(intern "NOTIFICATION")

(defparameter *stream-socket* nil)

(defun stream--get-type (line)
  (let ((type (string-upcase (string-trim '(#\Space #\Newline) (subseq line 6)))))
    (unless (empty-stringp type)
      (intern type))))

(defun stream--parse (type data)
  (let ((parsed-data (decode-json-from-string (subseq data 6))))
    (cond
      ((eq type 'notification) ()) ; make a notification object here
      ((eq type 'update) (print (cdr (assoc :content parsed-data)))) ;make a status object here
      ((eq type 'delete) ()))))  ;remove status from list of saved ones

(defun stream--backend (api-path &rest wanted-types)
  (let ((socket (masto--perform-request `(:get ,api-path :keep-alive t :want-stream t :stream t))))
    (loop
       with wanted = nil
       for c = (stream-read-char socket)
       collect c into line
       when (char= c #\Newline) do (progn
				     (print (and (search "data: " line) wanted))
				     (if (and (search "data: " line) wanted)
					 (print "wanted!");(stream--parse (nth wanted wanted-types) line)
				       (setq wanted (position (stream--get-type (merge-string-list line)) wanted-types)))))))
				     

(defun stream-home ()
  (stream--backend "streaming/user" 'update 'delete))

(defun stream-local ()
  (stream--backend "streaming/public/local" 'update 'delete))

(defun stream-public ()
  (stream--backend "streaming/public" 'update 'delete))

(defun stream-tag (tag)
  (stream--backend (concatenate 'string "streaming/hashtag?tag=" tag) 'update 'delete))

(defun stream-notifications ()
  (stream--backend "streaming/user" 'notification))
    