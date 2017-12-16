;;;; mastodon.lisp
(in-package :cl-user)

(defpackage :mastodon
  (:nicknames :masto)
  (:use :mastodon.api :mastodon.streaming)
  (:export :post-status
	   :login
	   :register-application
	   :oauth-login
	   :console-oauth-login
	   :get-home-timeline
	   :get-notifications
	   :get-local-timeline
	   :get-public-timeline))

(in-package :mastodon)


