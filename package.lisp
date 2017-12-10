;;;; package.lisp

(defpackage #:mastodon
  (:nicknames :masto)
  (:use #:dex #:cl-json)
  (:export

   ; status creation
   :post-status

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
   


   ))

