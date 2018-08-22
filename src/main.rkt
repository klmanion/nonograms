;;-
;main.rkt
; created by: Kurt L. Manion
; on: Wed., 22 Aug 2018
; desc: solve a nonogram
;

#lang racket/base

;; DESIDERATUM {{{
; Given the width W and height H followed by W lines of space seperated lengths
; to be continguously marked in the collumn and H lines of similarly formatted
; lengths for the rows, solve the nonogram.
;
; Each length should be its own object and have knowledge of where it's placed;
; at the time it is first placed it will have to choose the most adventitious
; placement by ranking each placement according to the food count of each
; aisle marked, and selecting the option that consumes food from the aisles
; with the greatest abundance of food, thus placing the length in such a way
; that the placement chosen allows for the greatest flexibility.  The
; simulation will run from this point placing the lengths that would grow off
; of the first length placed, and in so doing traces back moves when the
; simulation hits a length that cannot be placed, for this reason the lengths
; will have to maintain a memory of their ranking of other possible placements.
;
; The first length to place must be one whose aisle's lengths determine the
; most space.  Keeping in mind that there must be a space between lengths, the 
; equation is: the sum of all the lengths + n-1, where n is number of lengths.
; }}}

(define aisle-rule->food-required
  (λ (rule)
    (cond [(number? rule) rule]
          [else (+ (apply + rule)
                   (- (length rule) 1))])))

(define is-legal?
  (λ (board row-rules col-rules)
    #f))

(define possible-moves
  (λ (rule len)
    '()))

(define move
  (λ (board row-rules col-rules ratings)
    (cond
      [(null? ratings) board]
      [else
        (let ([r (car ratings)])
          (let ([orient (list-ref r 1)]
                [dex (list-ref r 2)])
            (let ([aisle (cond [(equal? orient #\r) row-rules]
                               [(equal? orient #\c) col-rules])])
              (let ([rule (list-ref aisle dex)])
                (let ([moves (filter
                               (λ (board)
                                 (is-legal? board row-rules col-rules))
                               (map (λ (m)
                                      (place-move board m orient dex))
                                    (possible-moves rule (length aisle))))])
                  (cond
                    [(null? moves) #f]
                    [else
                      (for/or ([b (in-list moves)])
                        (move b row-rules col-rules (cdr ratings)))]))))))])))

(define aisle-rules->food-required
  (λ (aisle-rules ch)
    (for/list ([rule (in-list aisle-rules)]
               [i (in-range (- (length aisle-rules) 1))])
      (list
        (aisle-rule->food-required rule)
        ch
        i))))

(define run
  (λ (row-rules col-rules)
    (let ([row-rat (aisle-rules->food-required row-rules #\r)]
          [col-rat (aisle-rules->food-required col-rules #\c)])
      (let ([ratings (sort (append col-rat row-rat)
                           (λ (lst0 lst1)
                             (> (list-ref lst0 0) (list-ref lst1 0))))]
            [board (make-list (length row-rules)
                              (make-list (length col-rules) #\_))])
        (move board row-rules col-rules ratings)))))

(print-board
  (λ (board)
    (unless (null? board)
      (map display board)))) ; TODO

(define solve
  (λ ([in (current-input-port)])
    (parameterize ([current-input-port in])
      (let ([l1 (string-split (read-line))])
        (let ([W (string->number (car l1))]
              [H (string->number (cadr l1))])
          (let ([col-rules (for/list ([w (in-range (- W 1))])
                             (map string->number (string-split (read-line))))]
                [row-rules (for/list ([h (in-range (- H 1))])
                             (map string->number (string-split (read-line))))])
            (board-print (run row-rules col-rules))))))))

(module+ main
  (call-with-input-file "input.txt"
                        solve
                        #:mode 'text))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
