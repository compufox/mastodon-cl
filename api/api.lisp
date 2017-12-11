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
      :post-status))

(in-package :mastodon.api)

(defparameter *instance* "https://mastodon.social"
  "the instance that we should use, defaults to mastodon.social")

(defparameter test-instance "https://cybre.space"
  "the instance that the test token works with")

(defparameter *access-token* nil
  "the token that will allow us to access the masto instance")

(defparameter test-access-token "5e3d8b15abf206c1f4f96d3e6aa2420b72ada415aa299d4a17e147a5927e8471"
  "a test access token")
