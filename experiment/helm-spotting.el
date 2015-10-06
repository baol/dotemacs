;; helm-spotting.el
;;
;; based on (actually partly stolen from) helm-spotify.el package
;;
(require 'json)
(require 'request)
(require 'multi)

(defun alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (alist-get (cdr symbols) (assoc (car symbols) alist)) (cdr alist)))

(defmulti spotify-play-href (href)
  "Get the Spotify app to play the object with the given HREF." system-type)

(defmulti-method spotify-play-href 'darwin (href)
  (shell-command (format "osascript -e 'tell application %S to play track %S'" "Spotify" href)))

(defmulti-method spotify-play-href 'gnu/linux (href)
  (shell-command (format "dbus-send --session --type=method_call --dest=com.spotify.qt / org.freedesktop.MediaPlayer2.OpenUri \"string:%s\"" href)))

(defmulti-method spotify-play-href 'windows-nt (href)
  (shell-command (format "explorer %S" href)))

(defmulti-method-fallback spotify-play-href (href)
  (message "Sorry, helm-spotify does not support playing tracks on %S." system-type))

(defun spotify-play-track (track)
  "Get the Spotify app to play the TRACK."
  (spotify-play-href (alist-get '(uri) track)))

(defun spotify-play-album (track)
  "Get the Spotify app to play the album for this TRACK."
  (spotify-play-href (alist-get '(album uri) track)))

(defun helm-spotting-tracks (query-string)
  (interactive "sQuery: ")
  (spotify-search-async "track" query-string))

(defun helm-spotting (query-string)
  (interactive "sQuery: ")
  (spotify-search-async "album" query-string))

(defun spotify-format (js-data)
  (mapcar (lambda (track) (cons (concat
                                 " "
                                 (alist-get '(name) (aref (alist-get '(artists) track) 0))
                                 "  /  "
                                 (alist-get '(album name) track)
                                 "\n "
                                 (alist-get '(name) track)
                                 ) track))
          (alist-get '(items) (alist-get '(tracks) data))
          ))

(defun helm-spotify-actions-for-track (actions track)
  "Return a list of helm ACTIONS available for this TRACK."
  `((,(format "Play Track - %s" (alist-get '(name) track)) . spotify-play-track)
    (,(format "Play Album - %s" (alist-get '(name) track)) . spotify-play-album)
    ("Show Track Metadata" . pp)))

(defun spotify-search-async (endpoint search-string)
  "Search spotify for SEARCH-TERM, and sends the results to helm."
  (request
   "https://api.spotify.com/v1/search"
   :params `(("q" . ,search-string) ("type" . ,endpoint) ("limit" . "50"))
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               (message "Ok")
               ; (with-output-to-temp-buffer "*spotter debug*"
               ;   (print data))
               (helm :sources (helm-build-sync-source "Spotify tracks"
                                :multiline t
                                :candidates (spotify-format data)
                                :action-transformer 'helm-spotify-actions-for-track
                                )
                :buffer "*helm spotter*")
               ))
   :error
   (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                (setq helm-spotting-cache nil)
                (message "Got error: %S" error-thrown)))
   ))
