;;;; mastodon.asd

(asdf:defsystem #:mastodon
  :description "A library for interfacing with Mastodon instances"
  :version "0.1.4"
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
		 (:file "accounts")
		 (:file "statuses")
		 (:file "notifications")		 
		 (:file "lists")
		 (:file "apps")
		 (:file "media")
		 (:file "follows")
		 (:file "instance")
		 (:file "timelines")
		 (:file "reports")
		 (:file "search")
		 (:file "streaming")
		 (:file "auth")))
	       (:file "mastodon")))


