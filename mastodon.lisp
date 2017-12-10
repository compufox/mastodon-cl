;;;; mastodon.lisp

(in-package #:mastodon)

;;; "mastodon" goes here. Hacks and glory await!

(defvar *instance* "https://mastodon.social"
  "the instance that we should use, defaults to mastodon.social")

(defvar test-instance "https://cybre.space"
  "the instance that the test token works with")

(defvar *access-token* nil
  "the token that will allow us to access the masto instance")

(defvar test-access-token "15269c53f7c528f22d0a062717d6367b52292555665e7d03f92d2cb2c9356593"
  "a test access token")

(defvar *last-toot-id* nil
  "the id of the last status posted")

(defun masto--api-path (path)
  (concatenate 'string test-instance "/api/v1/" path))

(defun masto--perform-request (request)
  (let* ((mode (pop request))
	 (path (pop request))
	 (args request)
	 (req-method (case mode
		       (:get #'dex:get)
		       (:post #'dex:post)
		       (:put #'dex:put)
		       (:delete #'dex:delete)
		       (:patch #'dex:patch))))
    (apply req-method (append `(,(masto--api-path path)
				 :headers (("Authorization" . ,(concatenate 'string "Bearer " test-access-token))))
			      args))))
    
(defun post-status (status &key (visibility "public") (sensitive nil) (spoiler nil) (reply-id nil))
  (let ((post (cl-json:decode-json-from-string
	       (masto--perform-request `(:post "statuses" :content
					      (("status" . ,status)
					       ("visibility" . ,visibility)
					       ("spoiler_text" . ,spoiler)
					       ("sensitive" . ,sensitive)
					       ("in_reply_to_id" . ,reply-id)))))))
    (setq *last-toot-id* (cdr (assoc :id post)))))

(defun delete-last-post ()
  (delete-status *last-toot-id*))
