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

(struct posn (x y))
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

;; TODO: disable rotation when constrained by tiles
(define (rotate-current!)
  (local [(define m (game-object-block current))]
    (begin (set-mino-img! m (rotate 270 (mino-img m)))
           (if (= (+ (mino-idx m) 1)
                  (length (mino-blockposns m)))
               (set-mino-idx! m 0)
               (set-mino-idx! m (+ (mino-idx m) 1))))))

;; TODO: disable move when blocked by tiles
(define (move-aside! moveright?)
  (local [(define c (game-object-block current))
          (define p (game-object-position current))
          (define x (posn-x p))
          (define y (posn-y p))]
    (cond [moveright?
           (when (< x (- 9
                         (max-x (list-ref (mino-blockposns c)
                                          (mino-idx c)))))
             (set-game-object-position! current
                                        (posn (+ 1 x) y)))]
          [else
           (when (> x 0)
             (set-game-object-position! current
                                        (posn (- x 1) y)))])))

(define (get-tiles)
  (filter (λ(a) (not (mino? (game-object-block a))))
          all-game-objects))

(define (get-tile-posns-at x)
  (filter (λ(a) (= x (posn-x a)))
          (map game-object-position (get-tiles))))

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
  (local [(define c (game-object-block current))
          (define colr (mino-colr c))]
    (begin (for-each (λ (p)
                       (set! all-game-objects
                            (cons (game-object (mino-square colr)
                                               (posn (+ (posn-x p)
                                                        (posn-x (game-object-position current)))
                                                     (+ (posn-y p) 18)))
                                  all-game-objects)))
                     (list-ref (mino-blockposns c) (mino-idx c)))
           (set-game-object-block! current
                                   (list-ref minos
                                             (random (length minos))))
           (set-game-object-position! current
                                      (posn 4 0))
           (set-mino-idx! (game-object-block current) 0))))
  
(define (drop-current-mino2!)
  (local [(define pos (game-object-position current))]
    (set-game-object-position! current
                             (posn (posn-x pos)
                                   (+ (posn-y pos)
                                      (space-to-drop))))))
  
(define (on-key-press key)
  (cond [(equal? key "up")
         (rotate-current!)]
        [(equal? key "left")
         (move-aside! false)]
        [(equal? key "right")
         (move-aside! true)]
        [(equal? key " ")
         (drop-current-mino2!)]
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
  (begin (void)
         (big-bang all-game-objects
           (on-key (λ (ignore key)
                     (begin (on-key-press key)
                            all-game-objects)))
           (on-tick (lambda (obj)
                      (begin (for-each update! obj)
                             all-game-objects))
                    frame-interval)
           (to-draw (lambda (game-objects)
                      (foldl (lambda (object scene)
                               (place-image/align
                                (render object)
                                (* tile-size
                                   (posn-x (game-object-position object)))
                                (* tile-size
                                   (posn-y (game-object-position object)))
                                "left"
                                "top"
                                scene))
                             board
                             game-objects))
                    (* tile-size 10) 
                    (* tile-size 21)))))

(start-game)
