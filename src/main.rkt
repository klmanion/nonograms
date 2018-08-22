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
; }}}



; vim: set ts=2 sw=2 expandtab lisp tw=79:
