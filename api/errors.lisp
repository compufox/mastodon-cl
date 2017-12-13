(in-package #:cl-user)

(defpackage :mastodon.error
  (:use :cl))

(in-package #:mastodon.error)

(define-condition api-error (error)
  ((reason :initarg :reason
	   :reader reason)))

(define-condition unrecognized-status-privacy (api-error)
  ())

(define-condition invalid-instance (api-error)
  ())
