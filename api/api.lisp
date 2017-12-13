(in-package :cl-user)

(defpackage :mastodon.api
  (:use :cl)
  (:import-from :quri
		:url-encode)
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

      ;apps
      :register-application
   
      ; accounts
      :login
      :verify-credentials
      :get-current-user
      :update-user
      :get-account
      :get-account-followers
      :get-account-follows
      :get-account-statuses

      ;blocks
      :block-account
      :unblock-account
      :get-blocks
      :get-domain-blocks
      :block-domain
      :unblock-domain
      :mute-account
      :unmute-account
      :get-mutes
   
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
