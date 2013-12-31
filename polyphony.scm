;;;
;;; How to test
;;;
;;; (1) Run this script as 'gosh polyphony.scm'
;;; (2) Access to http://localhost:8080/?time=now&person=1 by a web browser or any http client
;;;

;;;
;;; Modules
;;;

(use srfi-1)
(use srfi-17)
(use srfi-27)
(use gauche.collection)
(use gauche.sequence)
(use gauche.time)
(use gauche.net)
(use util.match)
(use rfc.822)
(use rfc.uri)
(use sxml.serializer)
(use www.cgi)

;;;
;;; Constants
;;;

(define point-zero '(0 . 0))
(define point-one '(1 . 1))
(define point-hundred '(100 . 100))

(define pixel-per-milimeter 0.5)        ; [pixel/mm]
(define screen-size '(10000 . 2000))    ; [mm]

(define p-appearance 0.01)              ; [/1]

(define one-tick 0.125)                 ; [s]
(define time-slot 600.0)                ; [s]
(define event-gate 5)                   ; [s]
(define event-gate-after 5)             ; [s]

(define freezing-duration 5)            ; [s]

(define id-matrix-2x2 '(1.0 0.0 0.0 1.0))
(define id-matrix-3x3 '(1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0))

(define image-prefix "http://localhost:80/~kanaya/Polyphony/") ; the simulator accesses this address. (the rendering client ignores this prefix.)
(define image-suffix ".png")

(define attack-and-decay-duration 10)   ; [tick]
(define increasing '(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0))
(define decreasing '(1.0 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1))

(define duration-of-ambient-sound 6.4)  ; [s]
(define *t-ambient-sound-started* 0)    ; [s]

(define birds-step -60)
(define elephant-step (* -150 4))
(define rabbit-to-right-step (* 160 2))
(define rabbit-to-left-step-x (* -140 2))
(define rabbit-to-left-step-y (* 30 2))
(define owl-step (* 50 2))
(define tanuki-stride (* -200 2))
(define papilionidae-stride -475)
(define fox-stride (* -250 4))

(define *t-last-event* 0)
(define *the-last-content* '())
(define *freezing?* #f)
(define *the-current-person* 0)

;;;
;;; Utilities
;;;

(define *the-start-up-time* 0)

(define (get-width p) (car p))
(define (get-height p) (cdr p))
(define (pixel->milimeter v) (/ v pixel-per-milimeter))
(define (milimeter->pixel v) (* v pixel-per-milimeter))
(define (current-time)
  (define (sec+usec->sec s us) (+ s (/ us 1000000.0)))
  (receive [sec usec]
   (sys-gettimeofday)
   (- (sec+usec->sec sec usec) *the-start-up-time*)))
(define (name->url name) (string-append image-prefix name image-suffix))

(define (times n lst) (concatenate (make-list n lst)))

;;;
;;; <frame-set> class and <frame> class
;;;
;;;   The <frame-set> class defines a set of elements for a block of animation.
;;;   It can have each frames and the associated values including offset, size,
;;;   transformation matrix, and color matrix.
;;;
;;;   For easiness of setting up the <frame-set> class has arrays of
;;;   each elements of <frame> but not array of <frame> itself.
;;;
;;;   The procedure (ref fs i) returns i-th frame out of frame set fs.
;;;

(define-class <frame-set> ()
  ([names                  :init-keyword :names                  :init-value '()]
   [n-names                :init-keyword :n-names                :init-value 0]
   [default-offsets        :init-keyword :default-offsets        :init-value '()]
   [default-sizes          :init-keyword :default-sizes          :init-value '()]
   [default-matrices       :init-keyword :default-matrices       :init-value '()]
   [default-alphas         :init-keyword :default-alphas         :init-value '()]
   [default-color-matrices :init-keyword :default-color-matrices :init-value '()]
   [default-depths         :init-keyword :default-depths         :init-value '()]
   [default-sounds         :init-keyword :default-sounds         :init-value '()]))

(define-class <frame> ()
  ([name                 :init-keyword :name                 :init-value "unnamed"]
   [default-offset       :init-keyword :default-offset       :init-value point-zero]
   [default-size         :init-keyword :default-size         :init-value point-hundred]
   [default-matrix       :init-keyword :default-matrix       :init-value id-matrix-2x2]
   [default-color-matrix :init-keyword :default-color-matrix :init-value id-matrix-3x3]
   [default-sound        :init-keyword :default-sound        :init-value 'none]))

(define-method ref ([fs <frame-set>] [i <integer>])
					; returns <frame>
  (let*
      ([n-names (ref fs 'n-names)]
       [j       (if (< i n-names) i 0)])      ; boundary check
    (make <frame>
      :name                 (ref (ref fs 'names) j)
      :default-offset       (ref (ref fs 'default-offsets) j)
      :default-size         (ref (ref fs 'default-sizes) j)
      :default-matrix       (ref (ref fs 'default-matrices) j)
      :default-color-matrix (ref (ref fs 'default-color-matrices) j)
      :default-sound        (ref (ref fs 'default-sounds) j))))

;;;
;;; <animation> class
;;;
;;;   The <animation> class is designed to hold a building block of
;;;   animation.  The class will have frames (<frame-set> class),
;;;   frame numbers (integer array), timings (real number array),
;;;   offsets (point array), sizez (point array), and transformation
;;;   matrices.
;;;
;;;   The procedure (ref a t) returns the frame of timing t out of
;;;   animation a.
;;;
;;;   A utility procedure (durations->timings durations) returns
;;;   timing array from given duration array; e.g. passing '(1 1 1) to
;;;   this procedure makes '(1 2 3).
;;;
;;;   Multiple instances are arrowed.
;;;

(define-class <animation> ()
  ([title         :init-keyword :title         :init-value 'untitled]
   [frames        :init-keyword :frames        :init-value #f]
   [n-frames      :init-keyword :n-frames      :init-value 0]
   [frame-numbers :init-keyword :frame-numbers :init-value '()]
   [timings       :init-keyword :timings       :init-value '()]
   [alphas        :init-keyword :alphas        :init-value '()]
   [offset        :init-keyword :offset        :init-value point-zero]
   [size          :init-keyword :size          :init-value point-one]
   [depth         :init-keyword :depth         :init-value 0]
   [matrix        :init-keyword :matrix        :init-value id-matrix-2x2]
   [animating     :init-keyword :animating     :init-value #f]
   [time-offset   :init-keyword :time-offset   :init-value 0]
   [x-random      :init-keyword :x-random      :init-value #f]
   [y-random      :init-keyword :y-random      :init-value #f]
   [from-jump?    :init-keyword :from-jump?    :init-value #f]
   [can-jump?     :init-keyword :can-jump?     :init-value #f]
   [jumps-at      :init-keyword :jumps-at      :init-value 0]
   [jumps-to      :init-keyword :jumps-to      :init-value 'none]
   [jumped-from   :init-keyword :jumped-from   :init-value 'none]
   [jump-offset   :init-keyword :jump-offset   :init-value point-zero]
   [to-jump       :init-keyword :to-jump       :init-value #f] ; to be removed
   [forking?      :init-keyword :forking?      :init-value #f]
   [options       :init-keyword :options       :init-value '()]))

;;;
;;; Randomize offset of animation
;;;

(define-method randomize-offset! ([animation <animation>])
  (define (random-offset animation)
    (let
	([rx       (* (random-real) (get-width screen-size))]
	 [ry       (* (random-real) (get-height screen-size))]
	 [x        (get-width (ref animation 'offset))]
	 [y        (get-height (ref animation 'offset))]
	 [x-random (ref animation 'x-random)]
	 [y-random (ref animation 'y-random)])
      (cond
       [(and x-random y-random) (cons rx ry)]
       [x-random                (cons rx y)]
       [y-random                (cons x ry)]
       [else                    (cons x y)])))
  (when (not (ref animation 'animating))
	(set! (ref animation 'offset) (random-offset animation))))

;;;
;;; Start animation
;;;

(define-method start! ([animation <animation>])
  (randomize-offset! animation)
  (set! (ref animation 'animating) #t)
  (set! (ref animation 'time-offset) (current-time)))

;;;
;;; Stop animation
;;;

(define-method stop! ([animation <animation>])
  (set! (ref animation 'animating) #f)
  (set! (ref animation 'time-offset) 0))

;;;
;;; Ref for animation
;;;

(define-method animation-ref-primitive ([animation <animation>] [n <number>])
					; returns <frame> and <number>
  (let1 n-frames (ref animation 'n-frames)
	(if (and (< n n-frames) (>= n 0))
	    (values
	     (ref (ref animation 'frames) n)
	     (ref (ref animation 'alphas) n))
	    (values
	     (ref (ref animation 'frames) (- n-frames 1))  ;; out of bound error?
	     (ref (ref animation 'alphas) (- n-frames 1))))))

(define-method ref ([animation <animation>] [time <number>])
					; returns <frame> and <number>
  (define (index-of-nearest-time timings t)
    (let1 less-than-time (partition (cut > t <>) timings)
	  (length less-than-time)))
  (define (real-modulo t d) ;; needs faster argolithm!!!
    (if (< t d)
	t
	(real-modulo (- t d) d)))
  (let*
      ([time-offset   (ref animation 'time-offset)]
       [relative-time (- time time-offset)]
       [modulo-time   (real-modulo relative-time (duration-of animation))]
       [timings       (ref animation 'timings)]
       [frame-numbers (ref animation 'frame-numbers)]
       [frame-index   (index-of-nearest-time timings modulo-time)]
       [frame-number  (ref frame-numbers frame-index)])
    (animation-ref-primitive animation frame-number)))

;;;
;;; Utilities for animation
;;;

(define-method duration-of ([animation <animation>])
  (last (ref animation 'timings)))

(define (durations->timings durations)
  (define (d->t durations)
    (define (sum lst) (fold + 0 lst))
    (if (null? durations)
	'()
	(cons (sum durations) (d->t (cdr durations)))))
  (reverse (d->t (reverse durations))))

;;;
;;; Server
;;;
;;;   The procedure run-server runs infinite loop of socket listening.
;;;   If the loop get a request, it calls handle-request procedure.
;;;   The handle-request procedure parses the request and build up SXML tree
;;;   upon the request, and then renders it in XML format.
;;;

;; To avoid confusion of emacs, I pull out this regal expression of the
;; following function run-server.
(define _pattern_ #/^(GET|HEAD)\s+(\S+)\s+HTTP\/\d+\.\d+$/)

(define (run-server)
  (define (get-request iport)
    (rxmatch-case (read-line iport)
		  [test       eof-object?       'bad-request]
		  [_pattern_  (_ meth abs-path) (list* meth abs-path (rfc822-read-headers iport))]
		  [#/^[A-Z]+/ ()                'not-implemented]
		  [else       'bad-request]))
  (define (handle-request request oport)
    (match request
     ['bad-request              (display "HTTP/1.1 400 Bad Request\n" oport)]
     ['not-implemented          (display "HTTP/1.1 501 Not Implemented\n" oport)]
     [(mthd abs-path . headers) (receive [auth path q frag] (uri-decompose-hierarchical abs-path)
					 (let1 content (render-content path (cgi-parse-parameters :query-string (or q "")) (current-time))
					       (when (not *freezing?*)
						     (set! *the-last-content* content))
					       (display "HTTP/1.1 200 OK\n" oport)
					       (display "Content-Type: text/xml; charset=utf-8\n" oport)
					       (display #`"Content-Length: ,(string-size content)\n" oport)
					       (display "\n" oport)
					       (when (equal? mthd "GET")
						     (if (not *freezing?*)
							 (display content oport)
							 (display *the-last-content* oport))))
					 (when (> (- (current-time) *t-last-event*) freezing-duration)
					       (set! *freezing?* #f)))]
     [else                      (display "Internal error. Sorry.")]))
  (let1 server-sock (make-server-socket 'inet 8080 :reuse-addr? #t)
	(guard (e [else (socket-close server-sock) (raise e)])
	       (let loop ([client (socket-accept server-sock)])
		 ;; animation
		 (animation-terminator! *the-animation-collection*)
		 (animation-random-starter! *the-animation-collection*)
		 ;; network
		 (guard (e [else (socket-close client) (raise e)])
			(handle-request
			 (get-request (socket-input-port client))
			 (socket-output-port client))
			(socket-close client))
		 (loop (socket-accept server-sock))))))

;;;
;;; Rendering unit
;;;

;; Converter: '((a b) (c d))   -> '(:a b :c d)
(define (cgi-parameters->keyword-style-parameters cgi-params)           
  (define (keywordize lst)              ; '((a b) (c d))   -> '((:a b) (:c d))
    (map
     (lambda [p] (cons (make-keyword (car p)) (cdr p)))
     lst))
  (define (unwrap lst)                  ; '((:a b) (:c d)) -> '(:a b :c d)
    (apply append lst))
  (unwrap (keywordize cgi-params)))

;; Converter: '((a b) (c d))   -> '(('a b) ('c d))
(define (cgi-parameters->sxml-style-parameters cgi-params) 
  (map
   (lambda [p] (list (string->symbol (car p)) (cadr p)))
   cgi-params))

(define (render-content _ params now)   ; parameter path is not used
  ;; animation -> tag
  (define (animation->tag animation time)
    (receive [frame alpha] (ref animation time)
	     (let* 
		 ([name             (ref frame 'name)]
		  [default-offset   (ref frame 'default-offset)]
		  [default-offset-x (get-width default-offset)]
		  [default-offset-y (get-height default-offset)]
		  [default-size     (ref frame 'default-size)]
		  [default-size-x   (get-width default-size)]
		  [default-size-y   (get-height default-size)]
		  [depth            (ref animation 'depth)]  ; ignoring default-depth of each frames
		  [sound            (car (hash-table-get *the-sound-collection* (ref frame 'default-sound)))]
		  [offset           (ref animation 'offset)]
		  [offset-x         (get-width offset)]
		  [offset-y         (get-height offset)])
	       `(image
		 (@
		  (source ,(name->url name))
		  (id ,name)
		  (position_x ,(+ default-offset-x offset-x))
		  (position_y ,(+ default-offset-y offset-y))
		  (position_z ,depth)
		  (size_x ,default-size-x)
		  (size_y ,default-size-y)
		  (alpha ,alpha)
		  (sound ,sound))
		 ,name))))
  (define (animating-animations animations)
					; animations is a hash table
					; of <animations>
    (hash-table-fold
     animations
     (lambda
	 [_ animation lst]
       (if (ref animation 'animating)
	   (cons animation lst)
	   lst))
     '()))
  (define (frame-tag-list animations time)
    (map
     (lambda [animation] (animation->tag animation time))
     (animating-animations animations)))
  (let-keywords
   (cgi-parameters->keyword-style-parameters params)
   ([time "now"] [person "none"] . unknown-parameters)
   (animation-event-catcher! *the-animation-collection* person)
   (let 
       ([the-frame-tag-list (cond [(string=? time "now") (frame-tag-list *the-animation-collection* now)]
				  [else                  (frame-tag-list *the-animation-collection* (string->number time))])]
	[stop-sound         (not (string=? person "none"))])
     (srl:sxml->xml
      (list
       '*TOP*
       '(*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
       `(xml
	 (mute
	  (@ 
	   ,(if stop-sound
		'(status "on")
		'(status "off")))
	  "mute")
	 (ambient
	  (@
	   ,(if
	     #t
	     #;(> (- (current-time) *t-ambient-sound-started*) duration-of-ambient-sound)
	     '(sound "ambient-64sec")
	     '(sound "no-sound")))
	  "ambient sound")
	 (frame
	  (@
	   ,@(cgi-parameters->sxml-style-parameters params))
	  ,@the-frame-tag-list)))))))

;;;
;;; Animation random starter
;;;

(define (animation-random-starter! animations)
  (define (start-animation-randomly! _ animation)
    (when (< (random-real) p-appearance)
	  (when (and (not (ref animation 'animating)) 
		     (not (ref animation 'from-jump?)))
		(start! animation)
		#;(print "starting " (symbol->string (ref animation 'title))) )))
  (hash-table-for-each animations start-animation-randomly!))

;;;
;;; Animation kick starter
;;;

(define (animation-kick-starter! animations key)
  (when (not (ref (hash-table-get animations key) 'animating))
	(set! (ref (hash-table-get animations key) 'animating) #t)
	(set! (ref (hash-table-get animations key) 'time-offset) (current-time))))

;;;
;;; Animation terminator
;;;

(define (animation-terminator! animations)
  (define (terminate-animation-if-time-is-out! _ animation)
    (let 
	([animating (ref animation 'animating)]
	 [beginning (ref animation 'time-offset)])
      (when (and animating
		 (>
		  (current-time)
		  (+ beginning (duration-of animation))))
					; Do I skip jumped animations???
	    (stop! animation)
	    #;(print "terminating " (symbol->string (ref animation 'title))) )))
  (hash-table-for-each animations terminate-animation-if-time-is-out!))

;;;
;;; Animation event catcher
;;;

(define (animation-event-catcher! animations person)
					; person is a string
  (define (pair-plus x y)
    (cons
     (+ (car x) (car y))
     (+ (cdr x) (cdr y))))
  #;(hash-table-for-each 
   animations
   (lambda
       [_ animation]
     (when (ref animation 'to-jump)     ; to be removed
	   (let*
	       [[now (current-time)]
		[jumps-at (ref animation 'jumps-at)]
		[time-offset (ref animation 'time-offset)]
		[jumps-at-relative (+ jumps-at time-offset)]]
	     (when #t #;(> jumps-at-relative now)
		   (let1 
		    jumps-to
		    (ref animation 'jumps-to)
		    (set!
		     (ref (hash-table-get animations jumps-to) 'animating)
		     #t)
		    (set!
		     (ref (hash-table-get animations jumps-to) 'time-offset)
		     (current-time))  ;; !!
		    (set! (ref animation 'to-jump) #f)
		    (when (not (ref animation 'forking?))
			  (set! (ref animation 'animating) #f))))))))
  (when (not (string=? person "none"))
	(print "jump")
	(set! *the-current-person* (string->number person))
	;; !!
	(animation-kick-starter! *the-animation-collection* 'apple)
	(animation-kick-starter! *the-animation-collection* 'elephant)
	(animation-kick-starter! *the-animation-collection* 'tanuki)
	(animation-kick-starter! *the-animation-collection* 'rabbit)
	(animation-kick-starter! *the-animation-collection* 'fawn)
	(animation-kick-starter! *the-animation-collection* 'fox)
	(animation-kick-starter! *the-animation-collection* 'squirrel)
	(animation-kick-starter! *the-animation-collection* 'baboon-weeing)
	(animation-kick-starter! *the-animation-collection* 'meercat)	
	;; !!
	(let1 now (current-time)
	 (when (> (- now *t-last-event*) event-gate)
	       (set! *t-last-event* now)
	       #;(set! *freezing?* #t)
	       #;(set! *loss-time* (+ *loss-time* freezing-duration))
	       (hash-table-for-each
		animations
		(lambda
		    [_ animation]
		  (let1 can-jump? (ref animation 'can-jump?)
		   (when can-jump?
			 (let*
			     ([next-animation-key    (ref animation 'jumps-to)]
			      [next-animation        (hash-table-get animations next-animation-key)]
			      [animation-offset      (ref animation 'offset)]
			      [animation-jump-offset (ref animation 'jump-offset)]
			      [the-offset            (pair-plus animation-offset animation-jump-offset)])
			   (if
			    (not 
			     (or
			      (eq? (ref animation 'title) 'baboon)
			      (eq? (ref animation 'title) 'baboon-weeing)))
			    (set! (ref next-animation 'offset) the-offset))
			   (set! (ref animation 'to-jump) #t))))))))))  ; to be removed

;;;
;;; Animations
;;;

(define
  (make-animation-primitive
   :key 
   [title         'untitled]
   [frame-names   '()]
   [frame-offsets '()]
   [frame-numbers '()]
   [alphas        '()]
   [canvas-size   point-zero]
   [offset        point-zero]
   [x-random      #f]
   [y-random      #f]
   [from-jump?    #f]
   [can-jump?     #f]
   [jumps-at      0] ; [tick]                ; to be removed
   [jumps-to      'none]
   [jumped-from   'none]
   [jump-offset   point-zero]
   [forking?      #f]
   [sounds       '()]
   [options      '()])
  (let*
      ([n-frames        (length frame-names)]
       [n-numbers       (length frame-numbers)]
       [default-offsets (if (null? frame-offsets)
			    (make-list n-numbers point-zero)
			    frame-offsets)]
       [frame-set       (make <frame-set>
			  :names                  frame-names
			  :n-names                n-frames
			  :default-offsets        default-offsets
			  :default-sizes          (make-list n-frames canvas-size)
			  :default-matrices       (make-list n-frames id-matrix-2x2)
			  :default-color-matrices (make-list n-frames id-matrix-3x3)
			  :default-sounds         sounds)]
       [timings         (durations->timings (make-list n-numbers one-tick))]
       [alphas          (if (null? alphas) (make-list n-numbers 1.0) alphas)])
    (make <animation>
      :title         title
      :frames        frame-set
      :n-frames      n-numbers
      :frame-numbers frame-numbers
      :timings       timings
      :alphas        alphas
      :depth         (random-real)
      :offset        offset
      :x-random      x-random
      :y-random      y-random
      :from-jump?    from-jump?
      :can-jump?     can-jump?
      :jumps-at      (* jumps-at one-tick) ; [sec]
      :jumps-to      jumps-to
      :jumped-from   jumped-from
      :jump-offset   jump-offset
      :forking?      forking?
      :options       options)))

(define
  (make-animation-with-fade-in/out
   :key 
   [title         'untitled]
   [frame-names   '()]
   [frame-offsets '()]
   [frame-numbers '()]
   [canvas-size   point-zero]
   [offset        point-zero]
   [x-random      #f]
   [y-random      #f]
   [from-jump?    #f]
   [can-jump?     #f]
   [jumps-at      0] ; [tick]
   [jumps-to      'none]
   [jumped-from   'none]
   [jump-offset   point-zero]
   [forking?      #f]
   [sounds        '()]
   [options       '()])
  (let* 
      ([n-frames-original      (length frame-names)]
       [n-frames               (+ n-frames-original (* 2 attack-and-decay-duration))]
       [frame-offsets-asserted (if (null? frame-offsets)
				   (make-list n-frames point-zero)
				   frame-offsets)]
       [sounds-asserted        (if (null? sounds)
				   (make-list n-frames 'none)
				   sounds)]
       [alphas                 (append
				increasing
				(make-list n-frames-original 1)
				decreasing)])
    (make-animation-primitive
     :title         title
     :frame-names   frame-names
     :frame-offsets (append
		     (make-list
		      attack-and-decay-duration
		      (first frame-offsets-asserted))
		     frame-offsets-asserted
		     (make-list
		      attack-and-decay-duration
		      (last frame-offsets-asserted)))
     :frame-numbers (append
		     (make-list
		      attack-and-decay-duration 
		      (first frame-numbers))
		     frame-numbers
		     (make-list
		      attack-and-decay-duration
		      (last frame-numbers)))
     :canvas-size   canvas-size
     :offset        offset
     :x-random      x-random
     :y-random      y-random
     :from-jump?    from-jump?
     :can-jump?     can-jump?
     :jumps-at      jumps-at
     :jumps-to      jumps-to
     :jumped-from   jumped-from
     :jump-offset   jump-offset
     :forking?      forking?
     :alphas        alphas
     :sounds        (append 
		     (make-list attack-and-decay-duration 'none) 
		     sounds-asserted
		     (make-list attack-and-decay-duration 'none))
     :options       options)))

(define
  (make-simple-animation
   :key
   [title             'untitled]
   [frame-name-prefix "{prefix}/"]
   [n-frames          0]
   [frame-offsets     '()]
   [canvas-size       point-zero]
   [offset            point-zero]
   [x-random          #f]
   [y-random          #f]
   [from-jump?        #f]
   [can-jump?         #f]
   [jumps-at          0] ; [tick]
   [jumps-to          'none]
   [jumped-from       'none]
   [jump-offset       point-zero]
   [forking?          #f]
   [sounds           '()]
   [options          '()])
  (make-animation-primitive ; make-animation-with-fade-in/out
   :title         title
   :frame-names   (map
		   (cut string-append frame-name-prefix <>)
		   (map number->string (iota n-frames 1)))
   :frame-numbers (iota n-frames)
   :frame-offsets frame-offsets
   :canvas-size   canvas-size
   :offset        offset
   :x-random      x-random
   :y-random      y-random
   :from-jump?    from-jump?
   :can-jump?     can-jump?
   :jumps-at      jumps-at
   :jumps-to      jumps-to
   :jumped-from   jumped-from
   :jump-offset   jump-offset
   :forking?      forking?
   :sounds        sounds
   :options       options))

(define
  (make-birds-animation
   :key
   [title    'birds-white]
   [prefix   "{prefix}/"]
   [jumps-to 'birds-white-take-off])
  (let1
   frame-name-primitive '(1 2 3 4 1 2 3 4 1 2 3 4 5 6 5 6 7 8 7)
   (make-animation-with-fade-in/out
    :title title
    :frame-names   (map
		    (cut string-append prefix <>)
		    (map number->string frame-name-primitive))
    :frame-offsets `(,point-zero              ; 1
		     ,point-zero              ; 2
		     ,point-zero              ; 3
		     ,point-zero              ; 4
		     (,birds-step . 0)        ; 1'
		     (,birds-step . 0)        ; 2'
		     (,birds-step . 0)        ; 3'
		     (,birds-step . 0)        ; 4'
		     (,(* birds-step 2) . 0)  ; 1''
		     (,(* birds-step 2) . 0)  ; 2''
		     (,(* birds-step 2) . 0)  ; 3''
		     (,(* birds-step 2) . 0)  ; 4''
		     (,(* birds-step 2) . 0)  ; 5
		     (,(* birds-step 2) . 0)  ; 6
		     (,(* birds-step 2) . 0)  ; 5
		     (,(* birds-step 2) . 0)  ; 6
		     (,(* birds-step 2) . 0)  ; 7
		     (,(* birds-step 2) . 0)  ; 8
		     (,(* birds-step 2) . 0)) ; 7
    :frame-numbers (iota (length frame-name-primitive))
    :canvas-size   '(585 . 425)
    :offset        point-zero
    :x-random      #t
    :y-random      #t
    :can-jump?     #t
    :jumps-to      jumps-to
    :sounds        (make-list (length frame-name-primitive) 'none)
    :options       '())))

(define
  (make-birds-take-off-animation
   :key
   [title       'birds-white-take-off]
   [prefix1     "{prefix}/"]
   [prefix2     "{prefix}/"]
   [jumped-from 'birds-white])
  (let1 frame-names (append 
		     (map
		      (cut string-append prefix1 <>)
		      (map number->string '(9 10 11 12 13)))
		     (map
		      (cut string-append prefix2 <>)
		      (map number->string (iota 8 1)))
		     (map 
		      (cut string-append prefix2 <>)
		      (map number->string (iota 8 1))))
	(make-animation-primitive
	 :title         title
	 :frame-names   frame-names
	 :frame-offsets (append
			 (make-list (+ 5 8) point-zero)
			 (make-list 8 '(-210 . 0)))
	 :frame-numbers (iota (length frame-names))
	 :canvas-size   '(585 . 425)
	 :from-jump?    #t
	 :jumped-from   jumped-from
	 :sounds        (append
			 (make-list 5 'none)
			 '(birds-flying)
			 (make-list 7 'none)
			 '(birds-flying)
			 (make-list 7 'none))
	 :options       '())))

(define
  (make-papilionidae-animation
   :key
   [title    'papilionidae-white]
   [prefix   "{prefix}/"]
   [jumps-to 'papilionidae-white-touch-down])
  (let1 frame-names (map
		     (cut string-append prefix <>)
		     (map number->string (times 3 (iota 11 1))))
	(make-animation-with-fade-in/out
	 :title         title
	 :frame-names   frame-names
	 :frame-offsets (append (make-list 11 point-zero)
				(make-list 11 (cons papilionidae-stride 0))
				(make-list
				 11
				 (cons (* papilionidae-stride 2) 0)))
	 :frame-numbers (iota (length frame-names))
	 :canvas-size   '(585 . 425)
	 :offset        point-zero
	 :x-random      #t
	 :y-random      #t
	 :can-jump?     #t
	 :jumps-at      22                 ; [tick]
	 :jumps-to      jumps-to
	 :jump-offset   `(,(* papilionidae-stride 2) . 0)
	 :sounds        (append
			 '(butterfly) (make-list 10 'none)
			 '(butterfly) (make-list 10 'none)
			 '(butterfly) (make-list 10 'none))
	 :options       '())))

(define
  (make-papilionidae-touch-down-animation
   :key
   [title       'papilionidae-white-touch-down]
   [prefix      "{prefix}/"]
   [jumped-from 'papilionidae-white])
  (let1 frame-names (map
		     (cut string-append prefix <>)
		     (map number->string (iota 4 1)))
	(make-animation-primitive
	 :title         title
	 :frame-names   frame-names
	 :frame-offsets (make-list (length frame-names) point-zero)
	 :frame-numbers (iota (length frame-names))
	 :canvas-size   '(585 . 425)
	 :from-jump?    #t
	 :jumped-from   jumped-from
	 :sounds        (make-list (length frame-names) 'none)
	 :options       '())))

(define
  (make-rapae-animation
   :key
   [title    'rapae-white]
   [prefix   "{prefix}/"]
   [jumps-to 'rapae-white-touch-down])
  (make-simple-animation 
   :title             title
   :frame-name-prefix prefix
   :n-frames          11
   :canvas-size       '(585 . 425)
   :x-random          #t
   :y-random          #t
   :can-jump?         #t
   :jumps-at          9
   :jumps-to          jumps-to
   :sounds            (append '(butterfly) (make-list 10 'none))
   :options           '()))

(define
  (make-rapae-touch-down-animation
   :key
   [title       'rapae-white-touch-down]
   [prefix      "{prefix}/"]
   [jumped-from 'rapae-white])
  (let1 frame-names (map
		     (cut string-append prefix <>)
		     (map number->string (iota 4 1)))
	(make-animation-primitive
	 :title         title
	 :frame-names   frame-names
	 :frame-offsets (make-list (length frame-names) point-zero)
	 :frame-numbers (iota (length frame-names))
	 :canvas-size   '(585 . 425)
	 :from-jump?    #t
	 :jumped-from   jumped-from
	 :sounds        (make-list (length frame-names) 'none)
	 :options       '())))


(define *the-animation-collection*
  (let
      ([apple                          (make-simple-animation
					:title 'apple
					:frame-name-prefix "Apple2/"
					:n-frames 31
					:offset '(4000 . 500)
					:canvas-size '(2313 . 1040)
					:from-jump? #t
					:sounds (append (make-list 15 'none)
							'(apple-touch-down)
							(make-list 15 'none)))]
       [baboon-weeing                  (make-animation-primitive 
					:title 'baboon-weeing
					:frame-names (append
						      (map
						       (cut string-append "Baboon2/" <>)
						       (map number->string (iota 66 1))))
					:frame-numbers (iota 66)
					:offset '(4500 . 0)
					:canvas-size `(,(* 585 4) . ,(* 637 4))
					:from-jump? #t
					:sounds (append (make-list 23 'none)
							'(wee)
							(make-list 43 'none))
					:options '())]
       [birds-blue                     (make-birds-animation
					:title 'birds-blue
					:prefix "Birds/Birds_Blue/"
					:jumps-to 'birds-blue-take-off)]
       [birds-orange                   (make-birds-animation
					:title 'birds-orange
					:prefix "Birds/Birds_Orange/"
					:jumps-to 'birds-orange-take-off)]
       [birds-blue-take-off            (make-birds-take-off-animation
					:title 'birds-blue-take-off
					:prefix1 "Birds/Birds_Blue/"
					:prefix2 "Birds/Birds_Blue/flying-"
					:jumped-from 'birds-blue)]
       [birds-orange-take-off          (make-birds-take-off-animation
					:title 'birds-orange-take-off
					:prefix1 "Birds/Birds_Orange/"
					:prefix2 "Birds/Birds_Orange/flying-"
					:jumped-from 'birds-orange)]
       [papilionidae-blue              (make-papilionidae-animation
					:title 'papilionidae-blue
					:prefix "Butterfly/Papilionidae_Blue/"
					:jumps-to 'papilionidae-blue-touch-down)]
       [papilionidae-purple            (make-papilionidae-animation
					:title 'papilionidae-purple
					:prefix "Butterfly/Papilionidae_Purple/"
					:jumps-to 'papilionidae-purple-touch-down)]
       [papilionidae-white             (make-papilionidae-animation
					:title 'papilionidae-white
					:prefix "Butterfly/Papilionidae_White/"
					:jumps-to 'papilionidae-white-touch-down)]
       [papilionidae-yellow            (make-papilionidae-animation
					:title 'papilionidae-yellow
					:prefix "Butterfly/Papilionidae_Yellow/"
					:jumps-to 'papilionidae-yellow-touch-down)]
       [papilionidae-blue-touch-down   (make-papilionidae-touch-down-animation
					:title 'papilionidae-blue-touch-down
					:prefix "Butterfly/Papilionidae_Blue/touch_down/10_"
					:jumped-from 'papilionidae-blue)]
       [papilionidae-white-touch-down  (make-papilionidae-touch-down-animation
					:title 'papilionidae-white-touch-down
					:prefix "Butterfly/Papilionidae_White/touch_down/10_"
					:jumped-from 'papilionidae-white)]
       [papilionidae-yellow-touch-down (make-papilionidae-touch-down-animation
					:title 'papilionidae-yellow-touch-down
					:prefix "Butterfly/Papilionidae_Yellow/touch_down/10_"
					:jumped-from 'papilionidae-yellow)]
       [papilionidae-purple-touch-down (make-papilionidae-touch-down-animation
					:title 'papilionidae-purple-touch-down
					:prefix "Butterfly/Papilionidae_Purple/touch_down/10_"
					:jumped-from 'papilionidae-purple)]
       [pieris-rapae-pink              (make-rapae-animation
					:title 'pieris-rapae-pink
					:prefix "Butterfly/Pieris_Rapae_Pink/" 
					:jumps-to 'pieris-rapae-pink-touch-down)]
       [pieris-rapae-yellow            (make-rapae-animation
					:title 'pieris-rapae-yellow
					:prefix "Butterfly/Pieris_Rapae_Yellow/" 
					:jumps-to 'pieris-rapae-yellow-touch-down)]
       [pieris-rapae-pink-touch-down   (make-rapae-touch-down-animation
					:title 'pieris-rapae-pink-touch-down 
					:prefix "Butterfly/Pieris_Rapae_Pink/touch_down/10_"
					:jumped-from 'pieris-rapae-pink)]
       [pieris-rapae-yellow-touch-down (make-rapae-touch-down-animation
					:title 'pieris-rapae-yellow-touch-down 
					:prefix "Butterfly/Pieris_Rapae_Yellow/touch_down/10_"
					:jumped-from 'pieris-rapae-yellow)]
       [elephant                       (let1 frame-names (map
							  (cut string-append "Elephant2/ex/ex" <>)
							  (map
							   number->string
							   (append (iota 28 1) (iota (- 260 28) 30))))
					     (make-animation-primitive
					      :title 'elephant
					      :frame-names frame-names
					      :frame-offsets (make-list (length frame-names) point-zero)
					      :frame-numbers (iota (length frame-names))
					      :alphas (make-list (length frame-names) 1.0)
					      :canvas-size `(,(* 984 10) . ,(* 289 10))  ; 8
					      :offset '(1000 . -600)
					      :from-jump? #t
					      :sounds (make-list (length frame-names) 'none)
					      :options '()))]
       [fawn                           (make-simple-animation
					:title 'fawn
					:frame-name-prefix "Fawn2/"
					:n-frames 116
					:from-jump? #t
					:canvas-size `(,(* 1188 8) . ,(* 213 8))
					:offset '(500 . 0)
					:sounds (make-list 116 'none))]
       [fox                            (make-simple-animation
					:title 'fox
					:frame-name-prefix "Fox2/"
					:n-frames 56
					:from-jump? #t
					:canvas-size `(,(* 1188 4) . ,(* 213 4))
					:offset '(0 . 0)
					:sounds (make-list 56 'none))]
       [meercat                        (make-simple-animation
					:title 'meercat
					:frame-name-prefix "Meercat2/"
					:n-frames 171
					:from-jump? #t
					:canvas-size `(,(* 1041 8) . ,(* 213 8))
					:offset '(0 . 200)
					:sounds (make-list 171 'none))]
       [owl                            (let*
					   ([frame-names-primitive (append '(1 2 3 3 3) (iota 14 4) (iota 12 6))]
					    [frame-names           (map (cut string-append "Owl/" <>) (map number->string frame-names-primitive))]
					    [n-frames              (length frame-names-primitive)]
					    [canvas-size           `(,(* 585 2) . ,(* 425 2))]
					    [default-offsets       (append
								    (make-list 7 point-zero)  ; 1 2 3 3 3 4 5
								    (map (cut cons <> 0) (durations->timings (make-list owl-step 28))))]
					    [owl-frame-set         (make <frame-set>
								     :names                  frame-names
								     :n-names                n-frames
								     :default-offsets        default-offsets
								     :default-sizes          (make-list n-frames canvas-size)
								     :default-matrices       (make-list n-frames id-matrix-2x2)
								     :default-alphas         (make-list n-frames 1.0)
								     :default-color-matrices (make-list n-frames id-matrix-3x3)
								     :default-depths         (make-list n-frames 0)
								     :default-sounds         (append
											      '(owl-coming)        ; 1
											      (make-list 6 'none)  ; 2 3 3 3 4 5
											      '(owl-flying)        ; 6
											      (make-list 5 'none)  ; 7-11
											      '(owl-flying)        ; 12
											      (make-list 5 'none)  ; 13-17
											      '(owl-flying)        ; 6
											      (make-list 5 'none)  ; 7-11
											      '(owl-flying)        ; 12
											      (make-list 5 'none)))]) ; 13-17
					 (make <animation>
					   :title         'owl
					   :frames        owl-frame-set
					   :frame-numbers (iota n-frames)
					   :timings       (durations->timings (make-list n-frames one-tick))
					   :alphas        (make-list (* n-frames 10) 1.0)
					   :depth         (random-real)
					   :offset        point-zero
					   :x-random      #t
					   :y-random      #f
					   :from-jump?    #f
					   :can-jump?     #f
					   :jumps-at      0
					   :jumps-to      'none
					   :jump-offset   point-zero
					   :forking?      #f
					   :loops-for     1
					   :options       '()))]
       [rabbit 	                       (make-simple-animation
					:title 'rabbit
					:frame-name-prefix "Rabbit2/"
					:n-frames 189
					:from-jump? #t
					:canvas-size `(,(* 1188 6) . ,(* 213 6))
					:offset '(500 . 0) ; test
					:sounds (make-list 189 'none))]
       [squirrel                       (make-simple-animation
					:title 'squirrel
					:frame-name-prefix "Squirrel2/"
					:n-frames 23
					:from-jump? #t
					:canvas-size `(,(* 421 1.5) . ,(* 306 1.5))
					:offset '(3000 . 200)
					:sounds (make-list 23 'none))]
       [tanuki                         (make-simple-animation
					:title 'tanuki
					:frame-name-prefix "Tanuki2/"
					:n-frames 50
					:from-jump? #t
					:canvas-size `(,(* 883 8) . ,(* 213 8)) ; test
					:offset '(1000 . 0) ; *** CHANGE ***
					:sounds (make-list 50 'none))])
    (let1 hash-table (make-hash-table 'eqv?)
	  (hash-table-put! hash-table 'apple                          apple)
	  (hash-table-put! hash-table 'baboon-weeing                  baboon-weeing)
	  #;(hash-table-put! hash-table 'baboon-wee baboon-wee)
	  (hash-table-put! hash-table 'birds-blue                     birds-blue)
	  (hash-table-put! hash-table 'birds-orange                   birds-orange)
	  (hash-table-put! hash-table 'birds-blue-take-off            birds-blue-take-off)
	  (hash-table-put! hash-table 'birds-orange-take-off          birds-orange-take-off)
	  (hash-table-put! hash-table 'papilionidae-blue              papilionidae-blue)
	  (hash-table-put! hash-table 'papilionidae-purple            papilionidae-purple)
	  (hash-table-put! hash-table 'papilionidae-white             papilionidae-white)
	  (hash-table-put! hash-table 'papilionidae-yellow            papilionidae-yellow)
	  (hash-table-put! hash-table 'papilionidae-blue-touch-down   papilionidae-blue-touch-down)
	  (hash-table-put! hash-table 'papilionidae-purple-touch-down papilionidae-purple-touch-down)
	  (hash-table-put! hash-table 'papilionidae-white-touch-down  papilionidae-white-touch-down)
	  (hash-table-put! hash-table 'papilionidae-yellow-touch-down papilionidae-yellow-touch-down)
	  (hash-table-put! hash-table 'pieris-rapae-pink              pieris-rapae-pink)
	  (hash-table-put! hash-table 'pieris-rapae-yellow            pieris-rapae-yellow)
	  (hash-table-put! hash-table 'pieris-rapae-pink-touch-down   pieris-rapae-pink-touch-down)
	  (hash-table-put! hash-table 'pieris-rapae-yellow-touch-down pieris-rapae-yellow-touch-down)
	  (hash-table-put! hash-table 'elephant                       elephant)
	  (hash-table-put! hash-table 'fawn                           fawn)
	  (hash-table-put! hash-table 'fox                            fox)
	  (hash-table-put! hash-table 'meercat                        meercat)
					;(hash-table-put! hash-table 'owl owl)
	  (hash-table-put! hash-table 'rabbit                         rabbit)
	  ;; (hash-table-put! hash-table 'rabbit-to-left rabbit-to-left)
	  ;; (hash-table-put! hash-table 'rabbit-to-right rabbit-to-right)
	  (hash-table-put! hash-table 'squirrel                       squirrel)
	  (hash-table-put! hash-table 'tanuki                         tanuki)
	  ;; (hash-table-put! hash-table 'tanuki-turn-back tanuki-turn-back)
	  hash-table)))

;;;
;;; Sounds
;;;

(define *the-sound-collection*
  (let1 hash-table (make-hash-table)
	(hash-table-put! hash-table 'ambient               '("ambient-64sec"              . 64))
	(hash-table-put! hash-table 'apple-touch-down      '("apple_touch_down-1sec"      . 1))
	(hash-table-put! hash-table 'apple-touch-down-2    '("apple_touch_down_2-1sec"    . 2))
	(hash-table-put! hash-table 'baboon-voice          '("baboon_voice-4sec"          . 4))
	(hash-table-put! hash-table 'birds-flying          '("birds_flying-3sec"          . 3))
	(hash-table-put! hash-table 'birds-touch-down      '("birds_touch_down-1sec"      . 1))
	(hash-table-put! hash-table 'birds-twitter         '("birds_twitter-4sec"         . 4))
	(hash-table-put! hash-table 'butterfly             '("butterfly-3sec"             . 3))
	(hash-table-put! hash-table 'deer                  '("deer-1sec"                  . 1))
	(hash-table-put! hash-table 'deer-snort            '("deer_snort-3sec"            . 3))
	(hash-table-put! hash-table 'elephant              '("elephant-11sec"             . 11))
	(hash-table-put! hash-table 'elephant-eating-apple '("elephant_eating_apple-6sec" . 6))
	(hash-table-put! hash-table 'fox                   '("fox-1sec"                   . 1))
	(hash-table-put! hash-table 'fox-tanuki-from-bush  '("fox_tanuki_from_bush-2sec"  . 2))
	(hash-table-put! hash-table 'meercat               '("meercat-5sec"               . 5))
	(hash-table-put! hash-table 'meercat-voice         '("meercat_voice-1sec"         . 1))
	(hash-table-put! hash-table 'owl-coming            '("owl_coming-6sec"            . 6))
	(hash-table-put! hash-table 'owl-crying            '("owl_crying-1sec"            . 1))
	(hash-table-put! hash-table 'owl-flying            '("owl_flying-1sec"            . 1))
	(hash-table-put! hash-table 'rabbit                '("rabit-1sec"                 . 1))           ; spell miss!!
	(hash-table-put! hash-table 'squirrel-voice        '("squirell_voice-2sec"        . 2))  ; spell miss!!
	(hash-table-put! hash-table 'tanuki                '("tanuki-1sec"                . 1))
	(hash-table-put! hash-table 'wee                   '("wee-9sec"                   . 9))
	(hash-table-put! hash-table 'none                  '("no-sound"                   . 0))
	hash-table))

;;;
;;; Main
;;;

(define (main _)
  (receive [current-sec current-usec] (sys-gettimeofday)
	   (set! *the-start-up-time* (+ current-sec (/ current-usec 1000000.0))))
  (run-server)
  0)
