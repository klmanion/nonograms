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

(print-board
  (λ (lst)
    (void)))

(define move
  (λ (col-food row-food ratings)
    (let ([r (car ratings)])
      (let ([orient (list-ref r 1)]
            [dex (list-ref r 2)])
        (let ([subj (list-ref (cond [(equal? orient #\c) col-food]
                                    [(equal? orient #\r) row-food])
                              dex)])
          ; TODO figure out the moves, and rank them
          (void))))))

(define run
  (λ (col-food row-food)
    (let ([col-rat (for/list ([lens (in-list col-food)]
                              [i (in-range (- (length col-food) 1))])
                     (list
                       (aisle-length->size lens)
                       #\c
                       i))]
          [row-rat (for/list ([lens (in-list row-food)]
                              [i (in-range (- (length row-food) 1))])
                     (list
                       (aisle-length->size lens)
                       #\r
                       i))])
      (let ([ratings (sort (append col-rat row-rat)
                           (λ (lst0 lst1)
                             (> (list-ref lst0 0) (list-ref lst1 0))))])
        (move col-food row-food ratings)))))

(define solve
  (λ ([in (current-input-port)])
    (parameterize ([current-input-port in])
      (let ([l1 (string-split (read-line))])
        (let ([W (string->number (car l1))]
              [H (string->number (cadr l1))])
          (let ([col-food (for/list ([w (in-range (- W 1))])
                            (map string->number (string-split (read-line))))]
                [row-food (for/list ([h (in-range (- H 1))])
                            (map string->number (string-split (read-line))))])
            (board-print (run col-food row-food))))))))

(module+ main
  (call-with-input-file "input.txt"
                        solve
                        #:mode 'text))

; vim: set ts=2 sw=2 expandtab lisp tw=79:
