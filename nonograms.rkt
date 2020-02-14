#lang racket

(define (make-board spec)
  (define height (spec-height spec))
  (define width (spec-width spec))
  (list->vector (map (lambda (x) (make-vector height -1)) (range width))))

(define (board-copy board)
  (vector-map vector-copy board))

(define (board-width board)
  (vector-length board))

(define (board-height board)
  (vector-length (vector-ref board 0)))

(define (board-get board x y)
  (vector-ref (vector-ref board x) y))

(define (board-set! board x y v)
  (vector-set! (vector-ref board x) y v))

(define (board-get-col board x)
  (define height (board-height board))

  (map (lambda (y) (board-get board x y))
       (range height)))

(define (board-get-row board y)
  (define width (board-width board))

  (map (lambda (x) (board-get board x y))
       (range width)))

(define (board-apply-col board x col)
  (for-each
   (lambda (y) (board-set! board x y (list-ref col y)))
   (range (board-height board))))

(define (board-apply-row board y row)
  (for-each
   (lambda (x) (board-set! board x y (list-ref row x)))
   (range (board-width board))))

(define (print-board board)
  (define (board-location->string content)
    (cond ((= content 1) "#")
          ((= content 0) " ")
          ((= content -1) "?")))
  (for-each
   (lambda (y)
     (for-each
      (lambda (x)
        (display (board-location->string
                  (board-get board x y))))
      (range (board-width board)))
     (newline))
   (range (board-height board))))

(define (file->spec path)
  (define (spec-string->spec spec-string)
    (map
     (lambda (spec-bit)
       (map string->number (string-split (string-trim spec-bit "\"") ",")))
     (regexp-match* #px"\"([0-9]+,)*[0-9]+\"" spec-string)))
  
  (define lines (file->lines path))
  (define width (string->number (car lines)))
  (define height (string->number (cadr lines)))
  (define cols-spec-string (caddr lines))
  (define rows-spec-string (cadddr lines))

  (list width
        height
        (spec-string->spec cols-spec-string)
        (spec-string->spec rows-spec-string)))

(define spec-width car)
(define spec-height cadr)
(define spec-cols caddr)
(define spec-rows cadddr)

;; TODO: This might be worth optimizing, by aborting if we get into the situation where there isn't enough space to fit the spec in the remaining line, and putting entire bits in one go after checking for space
(define (all-lines line spec)
  (define (go line spec started)
    (cond

      ;; After the end of the spec, we have only zeroes, or no result
      ((null? spec)
       (if (member 1 line)
           (list)
           (list (build-list (length line) (const 0)))))
      ;; At the end of the line, with just a 0 spec remaining, there's the empty list
      ((and (null? line)
            (= 0 (car spec))
            (null? (cdr spec)))
       (list (list)))
      ;; At the end of the line, with the spec nonempty, there are no results
      ((null? line)
       (list))
      ;; If not started but the next index is a 1, there is one option: we have to dec the current spec-bit, leave 1 in front of whatever comes later, and start.
      ((and (not started)
            (= (car line)
               1))
       (map ((curry cons) 1)
            (go (cdr line) (cons (- (car spec) 1) (cdr spec)) #t)))
      ;; If not started and the next index is a 0, there is one option: we have to leave 0 in front of whatever comes later
      ((and (not started)
            (= (car line)
               0))
       (map ((curry cons) 0)
            (go (cdr line) spec #f)))
      ;; If not started and the next index is a -1, we have two options: Start, leave a 1 in front of whatever comes later, and dec the current spec-bit; or don't start, and leave a 0 in front of whatever comes later
      ((and (not started)
            (= (car line)
               -1))
       (append
        (map ((curry cons) 1)
             (go (cdr line) (cons (- (car spec) 1) (cdr spec)) #t))
        (map ((curry cons) 0)
             (go (cdr line) spec #f))))
      ;; If started and the current spec-bit is 0, the next index can't be 1 or there are no results. We have to leave a zero, remove the current spec bit, and stop.
      ((and started
            (= (car spec)
               0))
       (if (= (car line)
              1)
           (list)
           (map ((curry cons) 0)
                (go (cdr line) (cdr spec) #f))))
      ;; If started, the next index can't be 0 or there are no results. We have to leave a 1, dec the current spec bit.
      (started
       (if (= (car line)
              0)
           (list)
           (map ((curry cons) 1)
                (go (cdr line) (cons (- (car spec) 1) (cdr spec)) #t))))
      (else
       (displayln "You, you're finally awake;"))))
      
  (go line spec #f))

(define (line-intersections lines)
  ;; Assumes all lines are the same length
  ;; Takes a bunch of lines; places a 1 where they all have 1, a 0 where they all have a 0, and a -1 everywhere else
  (define (compare-firsts firsts)
    (cond ((andmap ((curry =) 1) firsts)
           1)
          ((andmap ((curry =) 0) firsts)
           0)
          (else
           -1)))

  (if (null? (car lines))
      (list)
      (cons
       (compare-firsts (map car lines))
       (line-intersections (map cdr lines)))))

(define (apply-constraints board spec)
  (print-board board)
  (newline)
  (define old (board-copy board))
  (for-each
   (lambda (x)
     (board-apply-col board
                      x
                      (line-intersections
                       (all-lines
                        (board-get-col board x)
                        (list-ref (spec-cols spec) x)))))
   (range (board-width board)))
  (for-each
   (lambda (y)
     (board-apply-row board
                      y
                      (line-intersections
                       (all-lines
                        (board-get-row board y)
                        (list-ref (spec-rows spec) y)))))
   (range (board-height board)))
  (unless (equal? board old)
    (apply-constraints board spec)))

(define (run path)
  (define spec (file->spec path))
  (define board (make-board spec))
  (apply-constraints board spec))

(run (vector-ref (current-command-line-arguments) 0))

#|
Optimizations:
 - all-lines has room for improvement (see above function def)
 - rather than going through all the columns and rows in order; make a smart priority list for the order: Start going through from line with least room to most room, and while going through, move rows and cols that changed to start of the queue
 - Integrate a search algorithm for when constraint-satisfaction falls short.
|#
