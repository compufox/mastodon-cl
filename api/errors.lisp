(in-package #:cl-user)

(defpackage :mastodon.error
  (:use :cl)
  (:export :api-error))

(in-package #:mastodon.error)

(define-condition api-error (error)
  ((reason :initarg :reason
	   :reader reason)))
