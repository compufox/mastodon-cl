;;;; mastodon.lisp
(in-package :cl-user)

(defpackage :mastodon
  (:nicknames :masto)
  (:use :mastodon.api :mastodon.streaming)
  (:export :post-status
	   :login
	   :register-application))

(in-package :mastodon)


