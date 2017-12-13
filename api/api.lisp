(in-package :cl-user)

(defpackage :mastodon.api
  (:use :cl)
  (:import-from :cl-json
		:decode-json-from-string)
  (:shadowing-import-from :dex
			  :get
			  :delete
			  :post)
  (:import-from :dex
		:patch
		:put)
  (:import-from :mastodon.error
		:api-error
		:unrecognized-status-privacy)
  (:export
   
      ; searching
      :mastodon-search

      ; reports
      :get-reports
      :report-user

      ; timelines
      :get-local-timeline
      :get-home-timeline
      :get-tag-timeline
      :get-public-timeline

      ; notifications
      :get-notifications
      :get-notification
      :clear-notifications
      :dismiss-notification

      ; statuses
      :fave-status
      :unfave-status
      :reblog-status
      :unreblog-status
      :pin-status
      :unpin-status
      :delete-status
      :post-status

      ;media
      :mass-upload-media))

(in-package :mastodon.api)

(defvar *version* "0.1.3")

(defvar *user-agent* (concatenate 'string "mastodon-cl/" *version*))

(defparameter *instance* "https://mastodon.social"
  "the instance that we should use, defaults to mastodon.social")

(defparameter test-instance "https://cybre.space"
  "the instance that the test token works with")

(defparameter *access-token* nil
  "the token that will allow us to access the masto instance")

(defparameter test-access-token "c433d3dd2ef458107ed26ce1c2fc02ddc30e6526fb1d71846ab018905da96ccf"
  "a test access token")
