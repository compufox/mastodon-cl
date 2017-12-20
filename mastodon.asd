;;;; mastodon.asd

(asdf:defsystem #:mastodon
  :description "A library for interfacing with Mastodon instances"
  :version "0.1.3"
  :author "zactepps@gmail.com"
  :license "GPLv3"
  :serial t
  :depends-on (:dexador
	       :cl-json
	       :cl-ppcre)
  :components ((:module "api"
		:components
		((:file "errors")
		 (:file "api")
		 (:file "util")
		 (:file "apps")
		 (:file "media")
		 (:file "follows")
		 (:file "instance")
		 (:file "accounts")
		 (:file "statuses")
		 (:file "timelines")
		 (:file "notifications")
		 (:file "reports")
		 (:file "search")
		 (:file "streaming")
		 (:file "auth")))
	       (:file "mastodon")))


