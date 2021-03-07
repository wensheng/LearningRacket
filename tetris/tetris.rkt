#lang racket
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)

(define tile-size 30)
(define frame-rate 2)
(define frame-interval (/ 1.0 frame-rate))

(define board
  (rectangle (* tile-size 10) 
             (* tile-size 21)
             'solid
             'black))

(define (mino-square c)
  (overlay (square tile-size "solid" c)
           (square tile-size "outline" "white")))

;; set to transparent so we can use equal? on them
(struct posn (x y) #:transparent)

(define p00 (posn 0 0))
(define p10 (posn 1 0))
(define p20 (posn 2 0))
(define p30 (posn 3 0))
(define p01 (posn 0 1))
(define p02 (posn 0 2))
(define p03 (posn 0 3))
(define p11 (posn 1 1))
(define p12 (posn 1 2))
(define p21 (posn 2 1))

(struct mino ([img #:mutable]
              colr
              [idx #:mutable]
              blockposns))

;OOOO                  
(define mino1
  (local [(define s (mino-square "cyan"))]
    (mino (beside s s s s)
          "cyan"
          0
          (list (list p00 p10 p20 p30)
                (list p00 p01 p02 p03)))))

;O
;OOO
(define mino2
  (local [(define s (mino-square "blue"))]
    (mino (above/align "left" s (beside s s s))
          "blue"
          0
          (list (list p00 p01 p11 p21)
                (list p00 p10 p01 p02)
                (list p00 p10 p20 p21)
                (list p10 p11 p02 p12)))))

; O
;OOO
(define mino3
  (local [(define s (mino-square "violet"))]
    (mino (above s (beside s s s))
          "violet"
          0
          (list (list p10 p01 p11 p21)
                (list p00 p01 p11 p02)
                (list p00 p10 p20 p11)
                (list p10 p01 p11 p12)))))

;  O
;OOO
(define mino4
  (local [(define s (mino-square "orange"))]
    (mino (above/align "right" s (beside s s s))
          "orange"
          0
          (list (list p20 p01 p11 p21)
                (list p00 p01 p02 p12)
                (list p00 p10 p20 p01)
                (list p00 p10 p11 p12)))))

;OO
;OO
(define mino5
  (local [(define s (mino-square "yellow"))]
    (mino (above (beside s s) (beside s s))
          "yellow"
          0
          (list (list p00 p10 p01 p11)))))

; OO
;OO
(define mino6
  (local [(define s (mino-square "green"))]
    (mino (beside/align "bottom"
                             s
                             (above/align "left"
                                          (beside s s)
                                          s))
          "green"
          0
          (list (list p10 p20 p01 p11)
                (list p00 p01 p11 p12)))))

;OO
; OO
(define mino7
  (local [(define s (mino-square "red"))]
    (mino (beside/align "top"
                             s
                             (above/align "left"
                                          s
                                          (beside s s)))
          "red"
          0
          (list (list p00 p10 p11 p21)
                (list p10 p01 p11 p02)))))

; list of posns -> integer
(define (max-x lops)
  (apply max (map posn-x lops)))
(define (max-y lops)
  (apply max (map posn-y lops)))

(check-eq? (max-x (list p10 p20 p01 p11)) 2)

(define minos
  (list mino1 mino2 mino3 mino4 mino5 mino6 mino7))

;;current mino is (mino, posn)
;;settled is (image, posn)
(struct game-object ([block #:mutable]
                     [position #:mutable]))

;current moving mino
;There's always only one mino, when mino settle, it's occupied space
;become tile blocks, the mino is reset at the top and get new random shape
(define current
  (game-object (list-ref minos
                         (random (length minos)))
               (posn 4 0)))


;; mino and all tile blocks
(define all-game-objects
  (append (map (λ (x)
                 (game-object (mino-square "grey")
                              (posn x 20)))
               (range 10)) ;bottom grey tiles
          (list current)))

(define (render o)
  (cond [(mino? (game-object-block o))
         (mino-img (game-object-block o))]
        [else
         (game-object-block o)]))

(define (cannot-rotate?)
  (local [(define c (game-object-block current))
          (define pss (mino-blockposns c))
          (define nidx (if (= (+ (mino-idx c) 1)
                              (length pss))
                           0
                           (+ (mino-idx c) 1)))
          (define ps (list-ref pss nidx))
          (define p (game-object-position current))
          (define x (posn-x p))
          (define y (posn-y p))]
    (or (> (+ (posn-x p) (max-x ps)) 9)
        (ormap (λ (a)
                 (member a (get-tileps)))
               (map (λ (a)
                      (posn (+ (posn-x a) x)
                      (+ (posn-y a) y)))
                    ps)))))
                      
;; rotate current falling mino unless constrained
(define (rotate-current!)
  (local [(define m (game-object-block current))]
    (unless (cannot-rotate?)
      (begin (set-mino-img! m (rotate 270 (mino-img m)))
             (if (= (+ (mino-idx m) 1)
                    (length (mino-blockposns m)))
                 (set-mino-idx! m 0)
                 (set-mino-idx! m (+ (mino-idx m) 1)))))))

;; posns of current mino
(define (posns)
  (local [(define c (game-object-block current))
          (define ps (list-ref (mino-blockposns c)
                               (mino-idx c)))
          (define p (game-object-position current))
          (define x (posn-x p))
          (define y (posn-y p))]
    (map (λ (a)
           (posn (+ (posn-x a) x)
                 (+ (posn-y a) y)))
         ps)))

;; integer(1 or -1) -> boolean
;; determine if mino can move aside
(define (can-move? direction)
  (local [(define c (game-object-block current))
          (define size (max-x (list-ref (mino-blockposns c)
                                        (mino-idx c))))
          (define p (game-object-position current))
          (define x (posn-x p))]
    (and (>= (+ x direction) 0)
         (<= (+ x direction) (- 9 size))
         (andmap (λ (a)
                   (not (member a (get-tileps))))
                 (map (λ (pn)
                        (posn (+ (posn-x pn) direction)
                              (posn-y pn)))
                      (posns))))))
                   

;; move sideway: 1 for right, -1 for left
(define (move-aside! direction)
  (local [(define p (game-object-position current))
          (define x (posn-x p))
          (define y (posn-y p))]
    (when (can-move? direction)
      (set-game-object-position! current
                                 (posn (+ x direction) y)))))

(define (get-tileps)
  (map game-object-position
       (filter (λ(a) (not (mino? (game-object-block a))))
               all-game-objects)))

(define (get-tile-posns-at x)
  (filter (λ(a) (= x (posn-x a)))
          (get-tileps)))

;; find highest tile
;; highest here means smallest
(define (highest-y-at x)
  (apply min (map posn-y (get-tile-posns-at x))))
   
;; how many space can mino drop
(define (space-to-drop)
  (local [(define m (game-object-block current))
          (define pos (game-object-position current))]
    (apply min (map (λ (p)
                      (- (highest-y-at (+ (posn-x p)
                                          (posn-x pos)))
                         (+ (posn-y p)
                            (posn-y pos))
                         1))
                    (list-ref (mino-blockposns m)
                              (mino-idx m))))))

#| this doesn't work
the newly added text/image will not be rendered in todraw
(define (game-over? g)
  (if (and (<= (space-to-drop) 0)
           (= (posn-y (game-object-position current)) 0))
      (begin
        (set! all-game-objects
              (cons (game-object (text/font "Game Over!" 32 "white"
                                            #f "modern" "normal"
                                            "bold" #f)
                                 (posn 0 0))
                    all-game-objects))
        #t)
      #f))|#

(define (game-over? g)
  (and (<= (space-to-drop) 0)
       (= (posn-y (game-object-position current)) 0)))

(define (settle-mino!)
  (local [(define c (game-object-block current))
          (define pos (game-object-position current))
          (define colr (mino-colr c))]
    (begin (for-each (λ (p)
                       (set! all-game-objects
                            (cons (game-object (mino-square colr)
                                               (posn (+ (posn-x p)
                                                        (posn-x pos))
                                                     (+ (posn-y p)
                                                        (posn-y pos))))
                                  all-game-objects)))
                     (list-ref (mino-blockposns c) (mino-idx c)))
           (set-game-object-block! current
                                   (list-ref minos
                                             (random (length minos))))
           (set-game-object-position! current
                                      (posn 4 0)))))
            
(define (drop-current-mino!)
  (local [(define pos (game-object-position current))]
    (set-game-object-position! current
                             (posn (posn-x pos)
                                   (+ (posn-y pos)
                                      (space-to-drop))))))
  
(define (on-key-press key)
  (cond [(equal? key "up")
         (rotate-current!)]
        [(equal? key "left")
         (move-aside! -1)]
        [(equal? key "right")
         (move-aside! 1)]
        [(equal? key " ")
         (drop-current-mino!)]
        [else null]))


(define (process-row! y)
  (local [(define bs (filter (λ (a) (not (mino? (game-object-block a))))
                             all-game-objects))
          (define ts (filter (λ (a) (= (posn-y (game-object-position a))
                                       y))
                             bs))]
    (when (= (length ts)
             10)
      (begin
        (for-each (λ (a)
                    (set! all-game-objects
                          (remove a all-game-objects)))
                    ts)
        (for-each (λ (a)
                    (set-game-object-position! a (posn (posn-x (game-object-position a))
                                                       (+ (posn-y (game-object-position a)) 1))))
                  (filter (λ (a) (and (not (mino? (game-object-block a)))
                                      (< (posn-y (game-object-position a)) y)))
                          all-game-objects))))))

(define (remove-complete!)
  (local [(define bs (filter (λ(a) (not (mino? (game-object-block a))))
                             all-game-objects))
          (define ps (map game-object-position bs))
          (define ys (remove-duplicates (map posn-y ps)))]
    (for-each process-row! (remove 20 ys))))

(define (update! obj)
  (local [(define pos (game-object-position current))]
    (when (mino? (game-object-block obj))
      (begin (remove-complete!)
             (if (<= (space-to-drop) 0)
                 (settle-mino!)
                 (set-game-object-position! current
                                           (posn (posn-x pos)
                                                 (+ (posn-y pos) 1))))))))


(define (start-game)
  (local [(define (todraw objs)
            (foldl (λ (object scene)
                     (place-image/align (render object)
                                        (* tile-size
                                           (posn-x (game-object-position object)))
                                        (* tile-size
                                           (posn-y (game-object-position object)))
                                        "left"
                                        "top"
                                        scene))
                   board
                   objs))
          (define (draw-gameover objs)
            (place-image/align (text/font "Game Over!" 32 "white"
                                          #f "modern" "normal"
                                          "bold" #f)
                               50
                               250
                               "left"
                               "top"
                               (todraw objs)))]                
    (big-bang all-game-objects
              (on-key (λ (ignore key)
                        (begin (on-key-press key)
                               all-game-objects)))
              (on-tick (λ (obj)
                         (begin (for-each update! obj)
                                all-game-objects))
                       frame-interval)
              (stop-when game-over? draw-gameover)
              (to-draw todraw))))

(start-game)
