# mastodon-cl

a mastodon API wrapper written in Common Lisp!

currently not on quicklisp, but supports being loaded by it, assuming your folders are properly set up

```lisp
(ql:quickload :mastodon)
(masto:register-application "My Awesome App" :instance "cybre.space" :scopes '("read" "write" "follow"))
(masto:login "me@example.com" "myFantasticPassword1!" :scopes '("read" "write" "follow"))
(masto:post-status "Hello from Lisp!")
```

## Setting a content warning:
```lisp
(masto:post-status "this post should be hidden" :cw "Hiding that post!")
```

## Adding pictures to a status (and setting the picture to NSFW):
```lisp
(masto:post-status "this post should have images attached" :media '("/path/to/file.png" "/path/to/another.jpg") :nsfw t)
```

## Setting different visibility options for a post:
```lisp
(masto:post-status "This post is unlisted :eyes:" :visibility "unlisted")
```

## Getting timelines:
```lisp
(masto:get-home-timeline)
(masto:get-local-timeline :limit 30)
(masto:get-notifications)
```

## Timeline streaming is also supported! (very much a WIP as of right now)
```lisp
(mastodon.streaming:stream-home)
(mastodon.streaming:stream-notifications)
```

[Todo](https://github.com/theZacAttacks/mastodon-cl/blob/master/TODO.md)

If you want full access to the api look in the package mastodon.api (gets loaded with the mastodon package). It exposes a lot more of the direct mastodon api to play with!


Feel free to submit PRs and issues!

If you need to reach me, I'm on Mastodon [@theZacAttacks@cybre.space](https://cybre.space/@theZacAttacks)
