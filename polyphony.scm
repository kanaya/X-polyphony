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
(define event-gate 5)                   ; [s]
(define freezing-duration 5)            ; [s]

(define id-matrix-2x2 '(1.0 0.0 0.0 1.0))
(define id-matrix-3x3 '(1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0))

(define image-prefix "file:///Users/kanaya/Documents/polyphony-animation-material/") ; the simulator accesses this address. (the rendering client ignores this prefix.)
(define image-suffix ".png")

(define duration-of-ambient-sound 6.4)  ; [s]
(define *t-ambient-sound-started* 0)    ; [s]

(define birds-step -60)
(define elephant-step -600)
(define rabbit-to-right-step 320)
(define rabbit-to-left-step-x -180)
(define rabbit-to-left-step-y 60)
(define owl-step 100)
(define tanuki-stride -400)
(define papilionidae-stride -475)
(define papilionidae-rev-stride 475)
(define fox-stride -1000)

(define *t-last-event* 0)
(define *the-last-content* '())
(define *freezing?* #f)
(define *the-current-person* 0)

;;;
;;; Utilities
;;;

(define (get-width p) (car p))
(define (get-height p) (cdr p))

(define *the-start-up-time* 0)
(define (current-time)
  (define (sec+usec->sec s us) (+ s (/ us 1000000.0)))
  (receive [sec usec]
   (sys-gettimeofday)
   (- (sec+usec->sec sec usec) *the-start-up-time*)))

(define (name->url name) (string-append image-prefix name image-suffix))

(define (times n lst) (concatenate (make-list n lst)))

;;;
;;; <cel-set> class and <cel> class
;;;
;;;   The <cel-set> class defines a set of elements for a block of clip.
;;;   It can have each cels and the associated values including offset, size,
;;;   transformation matrix, and color matrix.
;;;
;;;   For easiness of setting up the <cel-set> class has arrays of
;;;   each elements of <cel> but not array of <cel> itself.
;;;
;;;   The procedure (ref fs i) returns i-th cel out of cel set fs.
;;;

(define-class <cel-set> ()
  ([names                  :init-keyword :names                  :init-value '()]
   [n-names                :init-keyword :n-names                :init-value 0]
   [default-offsets        :init-keyword :default-offsets        :init-value '()]   ; remove?
   [default-sizes          :init-keyword :default-sizes          :init-value '()]   ; remove?
   [default-matrices       :init-keyword :default-matrices       :init-value '()]   ; remove?
   [default-alphas         :init-keyword :default-alphas         :init-value '()]   ; remove?
   [default-color-matrices :init-keyword :default-color-matrices :init-value '()]   ; remove?
   [default-depths         :init-keyword :default-depths         :init-value '()]   ; remove?
   [default-sounds         :init-keyword :default-sounds         :init-value '()])) ; remove?

(define-class <cel> ()
  ([name                 :init-keyword :name                 :init-value "unnamed"]
   [default-offset       :init-keyword :default-offset       :init-value point-zero] ; default... -> local...
   [default-size         :init-keyword :default-size         :init-value point-hundred]
   [default-matrix       :init-keyword :default-matrix       :init-value id-matrix-2x2]
   [default-color-matrix :init-keyword :default-color-matrix :init-value id-matrix-3x3]
   [default-sound        :init-keyword :default-sound        :init-value 'none]))
					; local-depth

(define-method ref ([cs <cel-set>] [i <integer>]) ; returns <cel>
  (let*
      ([n-names (ref cs 'n-names)]
       [j       (if (< i n-names) i 0)])      ; boundary check
    (make <cel>
      :name                 (ref (ref cs 'names) j)
      :default-offset       (ref (ref cs 'default-offsets) j) ; no, these default... should be relpaced with local...
      :default-size         (ref (ref cs 'default-sizes) j)
      :default-matrix       (ref (ref cs 'default-matrices) j)
      :default-color-matrix (ref (ref cs 'default-color-matrices) j)
      :default-sound        (ref (ref cs 'default-sounds) j))))

;;;
;;; <clip> class
;;;
;;;   The <clip> class is designed to hold a building block of
;;;   clip.  The class will have cels (<cel-set> class),
;;;   cel numbers (integer array), timings (real number array),
;;;   offsets (point array), sizez (point array), and transformation
;;;   matrices.
;;;
;;;   The procedure (ref a t) returns the cel of timing t out of
;;;   clip a.
;;;
;;;   A utility procedure (durations->timings durations) returns
;;;   timing array from given duration array; e.g. passing '(1 1 1) to
;;;   this procedure makes '(1 2 3).
;;;
;;;   Multiple instances are arrowed.
;;;

(define-class <clip> ()
  ([title       :init-keyword :title       :init-value 'untitled]     ; symbol
   [cels        :init-keyword :cels        :init-value #f]            ; <cel-set>
   [n-cels      :init-keyword :n-cels      :init-value 0]             ; cardinal
   [cel-numbers :init-keyword :cel-numbers :init-value '()]           ; array of cardinal
   [timings     :init-keyword :timings     :init-value '()]           ; array of real
   [alphas      :init-keyword :alphas      :init-value '()]           ; array of real
   [offset      :init-keyword :offset      :init-value point-zero]    ; array of pair of real
   [size        :init-keyword :size        :init-value point-one]     ; array of pair of real
   [depth       :init-keyword :depth       :init-value 0]             ; real
   [matrix      :init-keyword :matrix      :init-value id-matrix-2x2] ; array of real
   [animating   :init-keyword :animating   :init-value #f]            ; boolean
   [time-offset :init-keyword :time-offset :init-value 0]             ; real
   [x-random    :init-keyword :x-random    :init-value #f]            ; boolean
   [y-random    :init-keyword :y-random    :init-value #f]            ; boolean
   [bottom-half :init-keyword :bottom-half :init-value #f]            ; boolean
   [reactive?   :init-keyword :reactive?   :init-value #f]            ; boolean
   [options     :init-keyword :options     :init-value '()]))

;;;
;;; Randomize offset of clip
;;;

(define-method randomize-offset! ([clip <clip>])
  (define (random-offset clip)
    (let
	([rx       (* (random-real) (get-width screen-size))]
	 [ry       (* (random-real) (get-height screen-size))]
	 [rybh     (* (random-real) (* (get-height screen-size) 0.5))]
	 [x        (get-width (ref clip 'offset))]
	 [y        (get-height (ref clip 'offset))]
	 [x-random (ref clip 'x-random)]
	 [y-random (ref clip 'y-random)]
	 [bh       (ref clip 'bottom-half)])
      (if (not bh)
	  (cond
	   [(and x-random y-random) (cons rx ry)]
	   [x-random                (cons rx y)]
	   [y-random                (cons x ry)]
	   [else                    (cons x y)])
	  (cond
	   [(and x-random y-random) (cons rx rybh)]
	   [x-random                (cons rx y)]
	   [y-random                (cons x rybh)]
	   [else                    (cons x y)]))))
  (when (not (ref clip 'animating))
	(set! (ref clip 'offset) (random-offset clip))))

;;;
;;; Start clip
;;;

(define-method start! ([clip <clip>])
  (randomize-offset! clip)
  (set! (ref clip 'animating) #t)
  (set! (ref clip 'time-offset) (current-time)))

;;;
;;; Stop clip
;;;

(define-method stop! ([clip <clip>])
  (set! (ref clip 'animating) #f)
  (set! (ref clip 'time-offset) 0))

;;;
;;; Ref for clip
;;;

(define-method clip-ref-primitive ([clip <clip>] [n <number>]) ; returns <cel> and <number>
  (let1 n-cels (ref clip 'n-cels)
	(if (and (< n n-cels) (>= n 0))
	    (values
	     (ref (ref clip 'cels) n)
	     (ref (ref clip 'alphas) n))
	    (values
	     (ref (ref clip 'cels) (- n-cels 1))  ;; out of bound error?
	     0.5 #;(ref (ref clip 'alphas) (- n-cels 1))))))  ; ok?

(define-method ref ([clip <clip>] [time <number>]) ; returns <cel> and <number>
  (define (index-of-nearest-time timings t)
    (let1 less-than-time (partition (cut > t <>) timings)
	  (length less-than-time)))
  (define (real-modulo t d) ;; needs faster argolithm!!!
    (if (< t d)
	t
	(real-modulo (- t d) d)))
  (let*
      ([time-offset   (ref clip 'time-offset)]
       [relative-time (- time time-offset)]
       [modulo-time   (real-modulo relative-time (duration-of clip))]
       [timings       (ref clip 'timings)]
       [cel-numbers   (ref clip 'cel-numbers)]
       [cel-index     (index-of-nearest-time timings modulo-time)]
       [cel-number    (ref cel-numbers cel-index)])
    (clip-ref-primitive clip cel-number)))

;;;
;;; Utilities for clip
;;;

(define-method duration-of ([clip <clip>])
  (last (ref clip 'timings)))

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
		 ;; clip
		 (clip-terminator! *the-clip-collection*)
		 (clip-random-starter! *the-clip-collection*)
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
  ;; clip -> tag
  (define (clip->tag clip time)
    (receive [cel alpha] (ref clip time)
	     (let* 
		 ([name             (ref cel 'name)]
		  [default-offset   (ref cel 'default-offset)]
		  [default-offset-x (get-width default-offset)]
		  [default-offset-y (get-height default-offset)]
		  [default-size     (ref cel 'default-size)]
		  [default-size-x   (get-width default-size)]
		  [default-size-y   (get-height default-size)]
		  [depth            (ref clip 'depth)]
		  [sound            (car (hash-table-get *the-sound-collection* (ref cel 'default-sound)))]
		  [offset           (ref clip 'offset)]
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
  (define (animating-clips clips)
    (hash-table-fold
     clips
     (lambda [_ clip lst]
       (if (ref clip 'animating)
	   (cons clip lst)
	   lst))
     '()))
  (define (cel-tag-list clips time)
    (map
     (lambda [clip] (clip->tag clip time))
     (animating-clips clips)))
  (let-keywords
   (cgi-parameters->keyword-style-parameters params)
   ([time "now"] [person "none"] . unknown-parameters)
   (clip-event-catcher! *the-clip-collection* person)
   (let 
       ([the-cel-tag-list (cond [(string=? time "now") (cel-tag-list *the-clip-collection* now)]
				[else                  (cel-tag-list *the-clip-collection* (string->number time))])]
	[stop-sound       (not (string=? person "none"))])
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
	  ,@the-cel-tag-list)))))))

;;;
;;; Clip random starter
;;;

(define (clip-random-starter! clips)
  (define (start-clip-randomly! _ clip)
    (when (< (random-real) p-appearance)
	  (when (and (not (ref clip 'animating)) 
		     (not (ref clip 'reactive?)))
		(start! clip)
		#;(print "starting " (symbol->string (ref clip 'title))) )))
  (hash-table-for-each clips start-clip-randomly!))

;;;
;;; Clip kick starter
;;;

(define (clip-kick-starter! clips key)
  (when (not (ref (hash-table-get clips key) 'animating))
	(set! (ref (hash-table-get clips key) 'animating) #t)
	(set! (ref (hash-table-get clips key) 'time-offset) (current-time))))

;;;
;;; Clip terminator
;;;

(define (clip-terminator! clips)
  (define (terminate-clip-if-time-is-out! _ clip)
    (let 
	([animating (ref clip 'animating)]
	 [beginning (ref clip 'time-offset)])
      (when (and animating
		 (>
		  (current-time)
		  (+ beginning (duration-of clip))))
					; Do I skip jumped clips???
	    (stop! clip)
	    #;(print "terminating " (symbol->string (ref clip 'title))) )))
  (hash-table-for-each clips terminate-clip-if-time-is-out!))

;;;
;;; Clip event catcher
;;;

(define (clip-event-catcher! clips person)
					; person is a string
  (define (pair-plus x y)
    (cons 
     (+ (car x) (car y))
     (+ (cdr x) (cdr y))))
  (when (not (string=? person "none"))
	(print "jump")
	(set! *the-current-person* (string->number person))
	;; !!
	(clip-kick-starter! *the-clip-collection* 'apple)
	(clip-kick-starter! *the-clip-collection* 'elephant)
	(clip-kick-starter! *the-clip-collection* 'tanuki)
	(clip-kick-starter! *the-clip-collection* 'rabbit)
	(clip-kick-starter! *the-clip-collection* 'fawn)
	(clip-kick-starter! *the-clip-collection* 'fox)
	(clip-kick-starter! *the-clip-collection* 'squirrel)
	(clip-kick-starter! *the-clip-collection* 'baboon-weeing)
	(clip-kick-starter! *the-clip-collection* 'meercat)))

;;;
;;; Clips
;;;

;;
;; Primitive clip setter
;;

(define
  (make-clip-primitive
   :key 
   [title       'untitled]
   [cel-names   '()]
   [cel-offsets '()]
   [cel-numbers '()]
   [alphas      '()]
   [canvas-size point-zero]
   [offset      point-zero]
   [x-random    #f]
   [y-random    #f]
   [bottom-half #f]
   [reactive?  #f]
   [sounds      ()]
   [options     ()])
  (let*
      ([n-cels          (length cel-names)]
       [n-numbers       (length cel-numbers)]
       [default-offsets (if (null? cel-offsets)
			    (make-list n-numbers offset)
			    cel-offsets)]
       [cel-set         (make <cel-set>
			  :names                  cel-names
			  :n-names                n-cels
			  :default-offsets        default-offsets
			  :default-sizes          (make-list n-cels canvas-size)
			  :default-matrices       (make-list n-cels id-matrix-2x2)
			  :default-color-matrices (make-list n-cels id-matrix-3x3)
			  :default-sounds         sounds)]
       [timings         (durations->timings (make-list n-numbers one-tick))]
       [alphas          (if (null? alphas) (make-list n-numbers 1.0) alphas)])
    (make <clip>
      :title       title
      :cels        cel-set
      :n-cels      n-numbers
      :cel-numbers cel-numbers
      :timings     timings
      :alphas      alphas
      :depth       (random-real)
      :offset      offset
      :x-random    x-random
      :y-random    y-random
      :bottom-half bottom-half
      :reactive?   reactive?
      :options     options)))

;;;
;;; clip-append
;;;

(define (clip-append clip1 clip2)
  (let
      ([new-title       (ref clip1 'title)]
       [cels1           (ref clip1 'cels)]
       [cels2           (ref clip2 'cels)]
       [new-n-cels      (+ (ref clip1 'n-cels) (ref clip2 'n-cels))]
       [new-cel-numbers (append
			 (ref clip1 'cel-numbers)
			 (map (cut + (last (ref clip1 'cel-numbers)) <>) (ref clip2 'cel-numbers)))]
       [new-timings     (append
			 (ref clip1 'timings)
			 (map (cut + (last (ref clip1 'timings)) <>) (ref clip2 'timings)))]
       [new-alphas      (append (ref clip1 'alphas) (ref clip2 'alphas))]
       [new-offset      (ref clip1 'offset)] ; be careful
       [new-size        (ref clip1 'size)] ; not used?
       [new-depth       (ref clip1 'depth)]
       [new-matrix      (ref clip1 'matrix)]
       [new-animating   #f]
       [new-time-offset (ref clip1 'time-offset)]
       [new-x-random    (ref clip1 'x-random)]
       [new-y-random    (ref clip1 'y-random)]
       [new-bottom-half (ref clip1 'bottom-half)]
       [new-reactive?   (ref clip1 'reactive?)]
       [new-options     (ref clip1 'options)])
    (let1 new-cels (make <cel-set>
		     :names                  (append (ref cels1 'names)                  (ref cels2 'names))
		     :n-names                (+      (ref cels1 'n-names)                (ref cels2 'n-names))
		     :default-offsets        (append (ref cels1 'default-offsets)        (ref cels2 'default-offsets))
		     :default-sizes          (append (ref cels1 'default-sizes)          (ref cels2 'default-sizes))
		     :default-matrices       (append (ref cels1 'default-matrices)       (ref cels2 'default-matrices))
		     :default-alphas         (append (ref cels1 'default-alphas)         (ref cels2 'default-alphas))
		     :default-color-matrices (append (ref cels1 'default-color-matrices) (ref cels2 'default-color-matrices))
		     :default-depths         (append (ref cels1 'default-depths)         (ref cels2 'default-depths))
		     :default-sounds         (append (ref cels1 'default-sounds)         (ref cels2 'default-sounds)))
	  (make <clip> 
	    :title new-title
	    :cels new-cels
	    :n-cels new-n-cels
	    :cel-numbers new-cel-numbers
	    :timings new-timings
	    :alphas new-alphas
	    :depth new-depth
	    :offset new-offset
	    :x-random new-x-random
	    :y-random new-y-random 
	    :bottom-half new-bottom-half
	    :reactive? new-reactive?
	    :options new-options))))

;;;
;;; clip->clip-with-fade-out
;;;

(define (clip->clip clip)  ; copy constructor
  (let
      ([title         (ref clip 'title)]
       [cels          (ref clip 'cels)]
       [n-cels        (ref clip 'n-cels)]
       [cel-numbers   (ref clip 'cel-numbers)]
       [timings       (ref clip 'timings)]
       [alphas        (ref clip 'alphas)]
       [offset        (ref clip 'offset)]
       [size          (ref clip 'size)]
       [depth         (ref clip 'depth)]
       [matrix        (ref clip 'matrix)]
       [animating     (ref clip 'animating)]
       [time-offset   (ref clip 'time-offset)]
       [x-random      (ref clip 'x-random)]
       [y-random      (ref clip 'y-random)]
       [reactive?     (ref clip 'reactive?)]
       [options       (ref clip 'options)])
    (make <clip>
     :title         title
     :cels          cels
     :n-cels        n-cels
     :cel-numbers   cel-numbers
     :timings       timings
     :alphas        alphas
     :offset        offset
     :size          size
     :depth         depth
     :matrix        matrix
     :animating     animating
     :time-offset   time-offset
     :x-random      x-random
     :y-random      y-random
     :reactive?     reactive?
     :options       options)))

(define (clip->clip-with-fade-out clip)
  (let
      ([title         (ref clip 'title)]
       [cels          (ref clip 'cels)]
       [n-cels        (ref clip 'n-cels)]
       [cel-numbers   (ref clip 'cel-numbers)]
       [timings       (ref clip 'timings)]
       [alphas        (let1 alphas-original (ref clip 'alphas)
			    (append
			     (make-list (- (length alphas-original) 10) 1.0)
			     '(1.0 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1)))]
       [offset        (ref clip 'offset)]
       [size          (ref clip 'size)]
       [depth         (ref clip 'depth)]
       [matrix        (ref clip 'matrix)]
       [animating     (ref clip 'animating)]
       [time-offset   (ref clip 'time-offset)]
       [x-random      (ref clip 'x-random)]
       [y-random      (ref clip 'y-random)]
       [reactive?     (ref clip 'reactive?)]
       [options       (ref clip 'options)])
    (make <clip>
     :title       title
     :cels        cels
     :n-cels      n-cels
     :cel-numbers cel-numbers
     :timings     timings
     :alphas      alphas
     :offset      offset
     :size        size
     :depth       depth
     :matrix      matrix
     :animating   animating
     :time-offset time-offset
     :x-random    x-random
     :y-random    y-random
     :reactive?   reactive?
     :options     options)))


(define
  (make-simple-clip
   :key
   [title           'untitled]
   [cel-name-prefix "{prefix}/"]
   [n-cels          0]
   [cel-offsets     '()]
   [canvas-size     point-zero]
   [offset          point-zero]
   [x-random        #f]
   [y-random        #f]
   [reactive?       #f]
   [sounds          ()]
   [options         ()])
  (make-clip-primitive
   :title       title
   :cel-names   (map
		 (cut string-append cel-name-prefix <>)
		 (map number->string (iota n-cels 1)))
   :cel-numbers (iota n-cels)
   :cel-offsets cel-offsets
   :canvas-size canvas-size
   :offset      offset
   :x-random    x-random
   :y-random    y-random
   :reactive?   reactive?
   :sounds      sounds
   :options     options))

(define
  (make-birds-clip
   :key
   [title    'birds-white]
   [prefix   "{prefix}/"]
   [jumps-to 'birds-white-take-off])
  (let1
   cel-name-primitive '(1 2 3 4 1 2 3 4 1 2 3 4 5 6 5 6 7 8 7)
   (make-clip-primitive ; make-clip-with-fade-in/out
    :title       title
    :cel-names   (map
		  (cut string-append prefix <>)
		  (map number->string cel-name-primitive))
    :cel-offsets `(,point-zero              ; 1
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
    :cel-numbers (iota (length cel-name-primitive))
    :canvas-size '(585 . 425)
    :offset      point-zero
    :x-random    #t
    :y-random    #t
    :bottom-half #t
    :sounds      (make-list (length cel-name-primitive) 'none)
    :options     '())))

(define
  (make-birds-take-off-clip
   :key
   [title       'birds-white-take-off]
   [prefix1     "{prefix}/"]
   [prefix2     "{prefix}/"]
   [jumped-from 'birds-white])
  (let1 cel-names (append 
		   (map
		    (cut string-append prefix1 <>)
		    (map number->string '(9 10 11 12 13)))
		   (map
		    (cut string-append prefix2 <>)
		    (map number->string (iota 8 1)))
		   (map 
		    (cut string-append prefix2 <>)
		    (map number->string (iota 8 1))))
	(make-clip-primitive
	 :title       title
	 :cel-names   cel-names
	 :cel-offsets (append
		       (make-list (+ 5 8) point-zero)
		       (make-list 8 '(-210 . 0)))
	 :cel-numbers (iota (length cel-names))
	 :canvas-size '(585 . 425)
	 :reactive?   #t
	 :sounds      (append
		       (make-list 5 'none)
		       '(birds-flying)
		       (make-list 7 'none)
		       '(birds-flying)
		       (make-list 7 'none))
	 :options     '())))

(define (make-papilionidae-clip :key [title 'papilionidae-white] [prefix "{prefix}/"] [jumps-to 'papilionidae-white-touch-down])
  (let1 cel-names (map (cut string-append prefix <>) (map number->string (times 3 (iota 11 1))))
	(make-clip-primitive
	 :title       title
	 :cel-names   cel-names
	 :cel-offsets (append
		       (make-list 11 point-zero)
		       (make-list 11 (cons papilionidae-stride 0))
		       (make-list 11 (cons (* papilionidae-stride 2) 0)))
	 :cel-numbers (iota (length cel-names))
	 :canvas-size '(585 . 425)
	 :offset      point-zero
	 :x-random    #t
	 :y-random    #t
	 :sounds      (append
		       '(butterfly) (make-list 10 'none)
		       '(butterfly) (make-list 10 'none)
		       '(butterfly) (make-list 10 'none))
	 :options     '())))

(define (make-papilionidae-rev-clip :key [title 'papilionidae-white] [prefix "{prefix}/"] [jumps-to 'papilionidae-white-touch-down])
  (let1 cel-names (map (cut string-append prefix <>) (map number->string (times 3 (iota 11 1))))
	(make-clip-primitive
	 :title       title
	 :cel-names   cel-names
	 :cel-offsets (append
		       (make-list 11 point-zero)
		       (make-list 11 (cons papilionidae-rev-stride 0))
		       (make-list 11 (cons (* papilionidae-rev-stride 2) 0)))
	 :cel-numbers (iota (length cel-names))
	 :canvas-size '(585 . 425)
	 :offset      point-zero
	 :x-random    #t
	 :y-random    #t
	 :sounds      (append
		       '(butterfly) (make-list 10 'none)
		       '(butterfly) (make-list 10 'none)
		       '(butterfly) (make-list 10 'none))
	 :options     '())))

(define
  (make-papilionidae-touch-down-clip
   :key
   [title       'papilionidae-white-touch-down]
   [prefix      "{prefix}/"]
   [jumped-from 'papilionidae-white])
  (let1 cel-names (map
		   (cut string-append prefix <>)
		   (map number->string (iota 4 1)))
	(make-clip-primitive
	 :title       title
	 :cel-names   cel-names
	 :cel-offsets (make-list (length cel-names) point-zero)
	 :cel-numbers (iota (length cel-names))
	 :canvas-size '(585 . 425)
	 :reactive?   #t
	 :sounds      (make-list (length cel-names) 'none)
	 :options     '())))

(define (make-rapae-clip :key [title 'rapae-white] [prefix "{prefix}/"] [jumps-to 'rapae-white-touch-down]) 
  (make-simple-clip 
   :title           title
   :cel-name-prefix prefix
   :n-cels          11
   :canvas-size     '(585 . 425)
   :x-random        #t
   :y-random        #t
   :sounds          (append '(butterfly) (make-list 10 'none))
   :options         '()))

(define (make-rapae-rev-clip :key [title 'rapae-rev-white] [prefix "{prefix}/"] [jumps-to 'rapae-white-touch-down])
  (make-simple-clip
   :title           title
   :cel-name-prefix prefix
   :n-cels          11
   :canvas-size     '(585 . 425)
   :x-random        #t
   :y-random        #t
   :sounds          (append '(butterfly) (make-list 10 'none))
   :options         '()))

(define
  (make-rapae-touch-down-clip
   :key
   [title       'rapae-white-touch-down]  ;; ???
   [prefix      "{prefix}/"]
   [jumped-from 'rapae-white]) ;; ???
  (let1 cel-names (map
		   (cut string-append prefix <>)
		   (map number->string (iota 4 1)))
	(make-clip-primitive
	 :title       title
	 :cel-names   cel-names
	 :cel-offsets (make-list (length cel-names) point-zero)
	 :cel-numbers (iota (length cel-names))
	 :canvas-size '(585 . 425)
	 :reactive?   #t
	 :sounds      (make-list (length cel-names) 'none)
	 :options     '())))


(define *the-clip-collection*
  (let
      (
       ;; Mazak Birds
       ;; Simple clip
       [mazak-birds                    (make-simple-clip
					:title 'mazak-birds
					:cel-name-prefix "Mazak/"
					:n-cels 106
					:offset '(2200 . 1400) ; cel.offset
					:canvas-size '(2313 . 1040)
					:reactive? #t
					:sounds (make-list 106 'none))]
       ;; Apple
       ;; Simple clip
       [apple                          (make-simple-clip
					:title 'apple
					:cel-name-prefix "Apple2/"
					:n-cels 31
					:offset '(2200 . 500)
					:canvas-size '(2313 . 1040)
					:reactive? #t
					:sounds (append (make-list 15 'none)
							'(apple-touch-down)
							(make-list 15 'none)))]
       ;; Baboon-weeing
       [baboon-weeing                  (make-clip-primitive 
					:title 'baboon-weeing
					:cel-names (map (cut string-append "Baboon2/" <>) (map number->string (iota 66 1)))
					:cel-numbers (iota 66)
					:offset '(2200 . 0) ; cel.offset
					:canvas-size `(,(* 585 4) . ,(* 637 4))
					:reactive? #t
					:sounds (append (make-list 23 'none)
							'(wee)
							(make-list 43 'none))
					:options '())]
       ;; Birds Blue
       ;; Bird clip
       [birds-blue                     (make-birds-clip :title 'birds-blue :prefix "Birds/Birds_Blue/")]
       ;; Birds Orange
       ;; Bird clip
       [birds-orange                   (make-birds-clip :title 'birds-orange :prefix "Birds/Birds_Orange/")]
       ;; Birds Blue Take-off
       ;; Bird-take-off clip
       [birds-blue-take-off            (make-birds-take-off-clip :title 'birds-blue-take-off :prefix1 "Birds/Birds_Blue/" :prefix2 "Birds/Birds_Blue/flying-")]
       ;; Birds Orange Take-off
       ;; Bird-take-off clip
       [birds-orange-take-off          (make-birds-take-off-clip :title 'birds-orange-take-off :prefix1 "Birds/Birds_Orange/" :prefix2 "Birds/Birds_Orange/flying-")]
       ;; Papilionidae Blue
       ;; Papilionidae clip
       [papilionidae-blue              (make-papilionidae-clip :title 'papilionidae-blue :prefix "Butterfly/Papilionidae_Blue/")]
       [papilionidae-blue-rev          (make-papilionidae-rev-clip :title 'papilionidae-blue-rev :prefix "Butterfly/Papilionidae_Blue_Rev/")]
       ;; Papilionidae Purple
       ;; Papilioniade clip
       [papilionidae-purple            (make-papilionidae-clip :title 'papilionidae-purple :prefix "Butterfly/Papilionidae_Purple/")]
       [papilionidae-purple-rev        (make-papilionidae-rev-clip :title 'papilionidae-purple-rev :prefix "Butterfly/Papilionidae_Purple_Rev/")]
       ;; Papilionidae White
       ;; Papilionidae clip
       [papilionidae-white             (make-papilionidae-clip :title 'papilionidae-white :prefix "Butterfly/Papilionidae_White/")]
       [papilionidae-white-rev         (make-papilionidae-rev-clip :title 'papilionidae-white-rev :prefix "Butterfly/Papilionidae_White_Rev/")]
       ;; Papilionidae Yellow
       ;; Papilionidae clip
       [papilionidae-yellow            (make-papilionidae-clip :title 'papilionidae-yellow :prefix "Butterfly/Papilionidae_Yellow/")]
       [papilionidae-yellow-rev        (make-papilionidae-rev-clip :title 'papilionidae-yellow-rev :prefix "Butterfly/Papilionidae_Yellow_Rev/")]
       ;; Papilionidae Blue Touch-down
       ;; Papilionidae-touch-down clip
       [papilionidae-blue-touch-down   (make-papilionidae-touch-down-clip :title 'papilionidae-blue-touch-down :prefix "Butterfly/Papilionidae_Blue/touch_down/10_")]
       ;; Papilionidae White Touch-down
       ;; Papilionidae-touch-down clip
       [papilionidae-white-touch-down  (make-papilionidae-touch-down-clip :title 'papilionidae-white-touch-down :prefix "Butterfly/Papilionidae_White/touch_down/10_")]
       ;; Papilionidae Yellow Touch-down
       ;; Papilionidae-touch-down clip
       [papilionidae-yellow-touch-down (make-papilionidae-touch-down-clip :title 'papilionidae-yellow-touch-down :prefix "Butterfly/Papilionidae_Yellow/touch_down/10_")]
       ;; Papilionidae Purpule Touch-down
       ;; Papilionidae-touch-down clip
       [papilionidae-purple-touch-down (make-papilionidae-touch-down-clip :title 'papilionidae-purple-touch-down :prefix "Butterfly/Papilionidae_Purple/touch_down/10_")]
       ;; Pieris-Rapae Pink
       ;; Rapae clip
       [pieris-rapae-pink              (make-rapae-clip :title 'pieris-rapae-pink :prefix "Butterfly/Pieris_Rapae_Pink/")]
       [pieris-rapae-pink-rev          (make-rapae-rev-clip :title 'pieris-rapae-pink-rev :prefix "Butterfly/Pieris_Rapae_Pink_Rev/")] ; dummy
       ;; Pieris-Rapae Yellow
       ;; Rapae clip
       [pieris-rapae-yellow            (make-rapae-clip	:title 'pieris-rapae-yellow :prefix "Butterfly/Pieris_Rapae_Yellow/")]
       [pieris-rapae-yellow-rev        (make-rapae-rev-clip :title 'pieris-rapae-yellow-rev :prefix  "Butterfly/Pieris_Rapae_Yellow_Rev/")] ; dummy
       ;; Pieris-Rapae Pink Touch-down
       ;; Rapae-touch-douwn clip
       [pieris-rapae-pink-touch-down   (make-rapae-touch-down-clip :title 'pieris-rapae-pink-touch-down :prefix "Butterfly/Pieris_Rapae_Pink/touch_down/10_")]
       ;; Pieris-Rapae Yellow Touch-down
       ;; Rapae-touch-down clip
       [pieris-rapae-yellow-touch-down (make-rapae-touch-down-clip :title 'pieris-rapae-yellow-touch-down :prefix "Butterfly/Pieris_Rapae_Yellow/touch_down/10_")]
       ;; Elephant
       [elephant                       (let1 cel-names (map
							  (cut string-append "Elephant2/ex/ex" <>)
							  (map
							   number->string
							   (append (iota 28 1) (iota (- 260 28) 30))))
					     (make-clip-primitive
					      :title 'elephant
					      :cel-names cel-names
					      :cel-offsets (make-list (length cel-names) point-zero)
					      :cel-numbers (iota (length cel-names))
					      :alphas (make-list (length cel-names) 1.0)
					      :canvas-size `(,(* 984 10) . ,(* 289 10))  ; 8
					      :offset '(1000 . -300)
					      :reactive? #t
					      :sounds (make-list (length cel-names) 'none)
					      :options '()))]
       ;; Fawn
       ;; Simple clip
       [fawn                           (make-simple-clip
					:title 'fawn
					:cel-name-prefix "Fawn2/"
					:n-cels 116
					:reactive? #t
					:canvas-size `(,(* 1188 8) . ,(* 213 8))
					:offset '(500 . 100)
					:sounds (make-list 116 'none))]
       ;; Fox
       ;; Simple clip
       [fox                            (make-simple-clip
					:title 'fox
					:cel-name-prefix "Fox2/"
					:n-cels 56
					:reactive? #t
					:canvas-size `(,(* 1188 4) . ,(* 213 4))
					:offset '(0 . 0)
					:sounds (make-list 56 'none))]
       ;; Meercat
       ;; Simple clip
       [meercat                        (make-simple-clip
					:title 'meercat
					:cel-name-prefix "Meercat2/"
					:n-cels 171
					:reactive? #t
					:canvas-size `(,(* 1041 8) . ,(* 213 8))
					:offset '(0 . 200)
					:sounds (make-list 171 'none))]
       ;; Owl
       [owl                            (let*
					   ([cel-names-primitive (append '(1 2 3 3 3) (iota 14 4) (iota 12 6))]
					    [cel-names           (map (cut string-append "Owl/" <>) (map number->string cel-names-primitive))]
					    [n-cels              (length cel-names-primitive)]
					    [canvas-size           `(,(* 585 2) . ,(* 425 2))]
					    [default-offsets       (append
								    (make-list 7 point-zero)  ; 1 2 3 3 3 4 5
								    (map (cut cons <> 0) (durations->timings (make-list owl-step 28))))]
					    [owl-cel-set         (make <cel-set>
								     :names                  cel-names
								     :n-names                n-cels
								     :default-offsets        default-offsets
								     :default-sizes          (make-list n-cels canvas-size)
								     :default-matrices       (make-list n-cels id-matrix-2x2)
								     :default-alphas         (make-list n-cels 1.0)
								     :default-color-matrices (make-list n-cels id-matrix-3x3)
								     :default-depths         (make-list n-cels 0)
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
					 (make <clip>
					   :title         'owl
					   :cels        owl-cel-set
					   :cel-numbers (iota n-cels)
					   :timings       (durations->timings (make-list n-cels one-tick))
					   :alphas        (make-list (* n-cels 10) 1.0)
					   :depth         (random-real)
					   :offset        point-zero
					   :x-random      #t
					   :y-random      #f
					   :reactive?     #f
					   :loops-for     1
					   :options       '()))]
       ;; Rabbit
       ;; Simple clip
       [rabbit 	                       (make-simple-clip
					:title 'rabbit
					:cel-name-prefix "Rabbit2/"
					:n-cels 189
					:reactive? #t
					:canvas-size `(,(* 1188 6) . ,(* 213 6))
					:offset '(500 . 0) ; test
					:sounds (make-list 189 'none))]
       ;; Squirrel
       ;; Simple clip
       [squirrel                       (make-simple-clip
					:title 'squirrel
					:cel-name-prefix "Squirrel2/"
					:n-cels 23
					:reactive? #t
					:canvas-size `(,(* 421 1.5) . ,(* 306 1.5))
					:offset '(3000 . 0)
					:sounds (make-list 23 'none))]
       ;; Tanuki
       ;; Simple clip
       [tanuki                         (make-simple-clip
					:title 'tanuki
					:cel-name-prefix "Tanuki2/"
					:n-cels 50
					:reactive? #t
					:canvas-size `(,(* 883 8) . ,(* 213 8)) ; test
					:offset '(1000 . 0)
					:sounds (make-list 50 'none))])
    (let1 hash-table (make-hash-table 'eqv?)
	  (hash-table-put! hash-table 'apple                          (clip->clip-with-fade-out apple))
	  (hash-table-put! hash-table 'baboon-weeing                  (clip-append baboon-weeing mazak-birds))
	  (hash-table-put! hash-table 'birds-blue                     (clip->clip-with-fade-out birds-blue))
	  (hash-table-put! hash-table 'birds-orange                   (clip->clip-with-fade-out birds-orange))
	  (hash-table-put! hash-table 'birds-blue-take-off            birds-blue-take-off)
	  (hash-table-put! hash-table 'birds-orange-take-off          birds-orange-take-off)
	  (hash-table-put! hash-table 'papilionidae-blue              (clip->clip-with-fade-out papilionidae-blue))
	  (hash-table-put! hash-table 'papilionidae-purple            (clip->clip-with-fade-out papilionidae-purple))
	  (hash-table-put! hash-table 'papilionidae-white             (clip->clip-with-fade-out papilionidae-white))
	  (hash-table-put! hash-table 'papilionidae-yellow            (clip->clip-with-fade-out papilionidae-yellow))
	  (hash-table-put! hash-table 'papilionidae-blue-rev          (clip->clip-with-fade-out papilionidae-blue-rev))
	  (hash-table-put! hash-table 'papilionidae-purple-rev        (clip->clip-with-fade-out papilionidae-purple-rev))
	  (hash-table-put! hash-table 'papilionidae-white-rev         (clip->clip-with-fade-out papilionidae-white-rev))
	  (hash-table-put! hash-table 'papilionidae-yellow-rev        (clip->clip-with-fade-out papilionidae-yellow-rev))
	  (hash-table-put! hash-table 'papilionidae-blue-touch-down   papilionidae-blue-touch-down)
	  (hash-table-put! hash-table 'papilionidae-purple-touch-down papilionidae-purple-touch-down)
	  (hash-table-put! hash-table 'papilionidae-white-touch-down  papilionidae-white-touch-down)
	  (hash-table-put! hash-table 'papilionidae-yellow-touch-down papilionidae-yellow-touch-down)
	  (hash-table-put! hash-table 'pieris-rapae-pink              (clip->clip-with-fade-out pieris-rapae-pink))
	  (hash-table-put! hash-table 'pieris-rapae-yellow            (clip->clip-with-fade-out pieris-rapae-yellow))
	  (hash-table-put! hash-table 'pieris-rapae-pink-rev          (clip->clip-with-fade-out pieris-rapae-pink-rev))
	  (hash-table-put! hash-table 'pieris-rapae-yellow-rev        (clip->clip-with-fade-out pieris-rapae-yellow-rev))
	  (hash-table-put! hash-table 'pieris-rapae-pink-touch-down   pieris-rapae-pink-touch-down)
	  (hash-table-put! hash-table 'pieris-rapae-yellow-touch-down pieris-rapae-yellow-touch-down)
	  (hash-table-put! hash-table 'elephant                       (clip->clip-with-fade-out elephant))
	  (hash-table-put! hash-table 'fawn                           (clip->clip-with-fade-out fawn))
	  (hash-table-put! hash-table 'fox                            (clip->clip-with-fade-out fox))
	  (hash-table-put! hash-table 'meercat                        (clip->clip-with-fade-out meercat))
	  ;; (hash-table-put! hash-table 'owl owl)
	  (hash-table-put! hash-table 'rabbit                         (clip->clip-with-fade-out rabbit))
	  ;; (hash-table-put! hash-table 'rabbit-to-left rabbit-to-left)
	  ;; (hash-table-put! hash-table 'rabbit-to-right rabbit-to-right)
	  (hash-table-put! hash-table 'squirrel                       squirrel)
	  (hash-table-put! hash-table 'tanuki                         (clip->clip-with-fade-out tanuki))
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
	(hash-table-put! hash-table 'rabbit                '("rabit-1sec"                 . 1))  ; spell miss!!
	(hash-table-put! hash-table 'squirrel-voice        '("squirell_voice-2sec"        . 2))  ; spell miss!!
	(hash-table-put! hash-table 'tanuki                '("tanuki-1sec"                . 1))
	(hash-table-put! hash-table 'wee                   '("wee-9sec"                   . 9))
	(hash-table-put! hash-table 'none                  '("no-sound"                   . 0))
	hash-table))

;;;
;;; Main
;;;

(define (main _)
  (set! *the-start-up-time* (current-time))
  (run-server)
  0)
