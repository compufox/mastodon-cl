(in-package :mastodon.api)

(defun upload-media (path)
  (decode-json-from-string
   (masto--perform-request `(:post "media" :content
				  (("file" . ,path))))))

(defun mass-upload-media (paths)
  (when (> (length paths) 4) (error 'api-error :reason "only four files can be attached to a status"))
  (when (= (length paths) 0) (return-from mass-upload-media nil))
  (loop
     for path in paths
     for uploaded = (upload-media path)
     collect `("media_ids[]" . ,(cdr (assoc :id uploaded)))))
