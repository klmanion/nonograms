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
; Each segment should be its own object and have knowledge of where it's placed;
; at the time it is first placed it will have to choose the most adventitious
; placement by ranking each placement according to the food count of each
; aisle marked, and selecting the option that consumes food from the aisles
; with the greatest abundance of food, thus placing the segment in such a way
; that the placement chosen allows for the greatest flexibility.  The
; simulation will run from this point placing the segments that were ranked
; under the first segment placed, and in so doing traces back moves when the
; simulation hits a segment that cannot be placed, for this reason the segments
; will have to maintain a memory of their ranking of other possible placements.
;
; The first segment to place must be one whose aisle's rules determine the
; most space.  Keeping in mind that there must be a space between segments,
; the equation is: the sum of all the lengths + n-1, where n is the number of
; segments.
; }}}

(require racket/string
  racket/list)

;; Predicates {{{
;
(define conforms-to-rule?
  (λ (lst rule)
    (let ([seg-lst (remove* (list "")
                            (string-split (list->string lst) #rx"_"))])
      (and
        (= (length seg-lst) (length rule))
        (for/and ([seg (in-list seg-lst)]
                  [len (in-list rule)])
          (= (string-length seg) len))))))

(define breaks-rule?
  (λ (lst rule)
    (let ([seg-lst (remove* (list "")
                            (string-split (list->string lst) #rx"_"))])
      (> (apply + (map string-length seg-lst))
         (apply + rule)))))

(define is-legal?
  (λ (board row-rules col-rules)
    (and
      (for/and ([row (in-list board)]
                [rule (in-list row-rules)])
        (not (breaks-rule? row rule)))
      (for/and ([col (in-list (for/list ([i (in-range (length (car board)))])
                                (map (λ (row)
                                       (list-ref row i))
                                     board)))]
                [rule (in-list col-rules)])
        (not (breaks-rule? col rule))))))
;; }}}

;; Conversions {{{
;
(define aisle-rule->food-required
  (λ (rule)
    (cond [(null? rule) 0]
          [else (+ (apply + rule)
                   (- (length rule) 1))])))

(define aisle-rules->food-required
  (λ (aisle-rules orient)
    (for/list ([rule (in-list aisle-rules)]
               [dex (in-range (length aisle-rules))])
      (list
        (aisle-rule->food-required rule)
        orient
        dex))))
;; }}}

;; Helpers {{{
;
(define possible-moves
  (λ (rule len)
    (filter
      (λ (move)
        (conforms-to-rule? move rule))
      (remove-duplicates
        (map flatten
             (permutations
               (append
                 (map (λ (seg-len)
                        (make-list seg-len #\x))
                      rule)
                 (make-list (- len (apply + rule)) #\_))))))))

(define place-move
  (λ (board move orient dex)
    (cond
      [(equal? orient 'row)
       (let-values ([(head tail) (split-at board dex)])
         (append head (list move) (cdr tail)))]
      [(equal? orient 'col)
       (for/list ([i (in-range (length board))])
         (for/list ([j (in-range (length (car board)))])
           (if (or (and (= j dex)
                        (eq? (list-ref move i) #\x))
                   (eq? (list-ref (list-ref board i) j) #\x))
               #\x
               #\_)))])))

(define board-print
  (λ (board)
    (cond
      [(null? board) (void)]
      [(eq? board #f) (printf "no solutions\n")]
      [else (for-each (λ (row) (printf "~a\n" row)) board)]))) ; TODO
;; }}}

;; Workhorses {{{
;
(define move
  (λ (board row-rules col-rules ratings)
    (cond
      [(null? ratings) board]
      [else
       (let ([r (car ratings)])
         (let ([orient (list-ref r 1)]
               [dex (list-ref r 2)])
           (let ([rules (cond [(equal? orient 'row) row-rules]
                              [(equal? orient 'col) col-rules])]
                 [len (length (cond [(equal? orient 'row) (car board)]
                                    [(equal? orient 'col) board]))])
             (let ([rule (list-ref rules dex)])
               (let ([moves (filter
                              (λ (board)
                                (is-legal? board row-rules col-rules))
                              (map (λ (m)
                                     (place-move board m orient dex))
                                   (possible-moves rule len)))])
                 (cond
                   [(null? moves) #f]
                   [else
                    (for/or ([b (in-list moves)])
                      (move b row-rules col-rules (cdr ratings)))]))))))])))

(define run
  (λ (row-rules col-rules)
    (let ([row-rat (aisle-rules->food-required row-rules 'row)]
          [col-rat (aisle-rules->food-required col-rules 'col)])
      (let ([ratings (sort (append row-rat col-rat)
                           (λ (lst0 lst1)
                             (> (list-ref lst0 0) (list-ref lst1 0))))]
            [board (make-list (length row-rules)
                              (make-list (length col-rules) #\_))])
        (move board row-rules col-rules ratings)))))

(define solve
  (λ ([in (current-input-port)])
    (parameterize ([current-input-port in])
      (let ([l0 (string-split (read-line))])
        (let ([W (string->number (car l0))]
              [H (string->number (cadr l0))])
          (let ([row-rules (for/list ([h (in-range H)])
                             (map string->number (string-split (read-line))))]
                [col-rules (for/list ([w (in-range W)])
                             (map string->number (string-split (read-line))))])
            (board-print (run row-rules col-rules))))))))
;; }}}

;; main {{{
;
(module+ main
  (call-with-input-file "input.txt"
                        solve
                        #:mode 'text))
;; }}}

; vim: set ts=2 sw=2 expandtab lisp tw=79:
