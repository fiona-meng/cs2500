;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw11p2-starter (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 11, Problem 2 ==

; Consider the following the following definition of a graph:

(define-struct graph [v e])
 
; A Graph is a (make-graph [List-of Nat] [Nat Nat -> Boolean])
; Interpretation: the vertices (nodes) and edges in a graph,
; where each vertex is identified by a unique natural number.
; All of the numbers in vertices are assumed to be unique and
; both inputs to the edges function are assumed to be
; valid vertices.

; Note: Below, we'll refer to inputs to the function in order as
; (s)ource then (d)estination.

(define G-2-1
  (make-graph
   (list 3 7)
   (λ (s d)
     (cond [(= s 3) #true]
           [(= s 7) #false]))))

(define G-2-2
  (make-graph
   (list 3 7)
   (λ (s d)
     (cond [(= s 3) (= d 7)]
           [(= s 7) (= d 3)]))))

(define G-3-1
  (make-graph
   (build-list 3 add1)
   (λ (s d)
     (cond [(= s 1) (= d 2)]
           [(= s 2) (= d 1)]
           [(= s 3) #false]))))

(define G-3-2
  (make-graph
   (build-list 3 add1)
   (λ (s d)
     (cond [(= s 1) (or (= d 2)
                        (= d 3))]
           [(= s 2) (or (= d 1)
                        (= d 3))]
           [(= s 3) (or (= d 1)
                        (= d 2))]))))

(define G-5-1
  (make-graph
   (build-list 5 add1)
   (λ (s d)
     (cond [(= s 1) (not (= d 1))]
           [(= s 2) (= d 3)]
           [(= s 3) #false]
           [(= s 4) (= d 5)]
           [(= s 5) (= d 4)]))))

(define G-5-2
  (make-graph
   (build-list 5 add1)
   (λ (s d)
     (cond [(= s 1) (or (= d 2)
                        (= d 4))]
           [(= s 2) (= d 5)]
           [(= s 3) (= d 5)]
           [(= s 4) #false]
           [(= s 5) (= d 3)]))))

(define (graph-temp g)
  (... (lon-temp (graph-v g)) ...
       (graph-e g) ...))



; TODO #1: design the function lonely? that takes a graph and a vertex and determines
; if no edge exists with the supplied vertex as the destination (excluding those where
; the source is also the supplied vertex). You have been supplied some tests for clarity.

; lonely? : Graph Nat -> Boolean
; determines if no other vertex is a source of
; an edge to the supplied vertex

(check-expect
 (lonely? G-2-1 3)
 #true)

(check-expect
 (lonely? G-2-2 3)
 #false)

(check-expect
 (lonely? G-3-1 3)
 #true)

(check-expect
 (lonely? G-5-1 1)
 #true)

(check-expect
 (lonely? G-5-1 2)
 #false)

(define (lonely? g n)
  (if (not (ormap (lambda (x) ((graph-e g) x n)) (graph-v g)))
      #true
      (ormap (lambda (y) ((graph-e g) n n)) (graph-v g))))

; TODO #2: design the function is-tight? that takes a graph and a list of vertices from that graph
; and determines if all the supplied vertices are the source for edges whose destinations are all
; other vertices in the list (they may still be connected to other vertices not in the list).
; Another way of saying this is that all the supplied vertices are adjacent to each other, that
; they form a clique (https://en.wikipedia.org/wiki/Clique_(graph_theory)).
; You have been supplied some tests for clarity.

; is-tight? : Graph [List-of Number] -> Boolean
; determines if the supplied vertices connect to each other

(check-expect
 (is-tight? G-2-1 (list 1))
 #true)

(check-expect
 (is-tight? G-2-1 (list 2))
 #true)

(check-expect
 (is-tight? G-2-1 (list 3 7))
 #false)

(check-expect
 (is-tight? G-2-2 (list 3 7))
 #true)

(check-expect
 (is-tight? G-3-1 (list 1 2))
 #true)

(check-expect
 (is-tight? G-3-1 (list 1 2 3))
 #false)

(check-expect
 (is-tight? G-3-2 (list 1 2))
 #true)

(check-expect
 (is-tight? G-3-2 (list 1 2 3))
 #true)

(define (is-tight? graph lon)
  (local [; every : [List-of Number] -> Boolean
          ; determines if a number in the given list is equal to any of the rest of the list
          (define (every lon)
            (cond
              [(empty? lon) #true]
              [(cons? lon)
               (and
                (and (andmap (lambda (x) ((graph-e graph) (first lon) x)) (rest lon))
                     (every (rest lon)))
                (and (andmap (lambda (x) ((graph-e graph) (first (reverse lon)) x))
                             (rest (reverse lon)))
                     (every (rest (reverse lon)))))]))]
    (if (= (length lon) 1)
        #true
        (every lon))))

; TODO #3: design the function adjacency-matrix that takes a graph and produces a
; matrix (a list of lists) where each row represents a vertex, each column also
; represents a vertex, and if (row, column) is a 1, then there is an edge from the
; vertex of the row to the vertex of the column.

; For example, G-3-1 would be represented as...

(define AM-3-1
  (list
   (list 0 1 0)
   (list 1 0 0)
   (list 0 0 0)))

; meaning...
; - row 1 (representing vertex 1): can only reach vertex 2 (via column 2)
; - row 2 (representing vertex 2): can only reach vertex 1 (via column 1)
; - row 3 (representing vertex 3): can't reach any other vertex (since the row is all 0's)

; You have been supplied some tests for clarity.

; adjacency-matrix : Graph -> [List-of [List-of {0, 1}]]
; producing the adjacency-matrix representation of a graph

(check-expect
 (adjacency-matrix G-2-1)
 (list
  (list 1 1)
  (list 0 0)))

(check-expect
 (adjacency-matrix G-3-1)
 AM-3-1)

(check-expect
 (adjacency-matrix G-5-2)
 (list
  (list 0 1 0 1 0)
  (list 0 0 0 0 1)
  (list 0 0 0 0 1)
  (list 0 0 0 0 0)
  (list 0 0 1 0 0)))

(define (adjacency-matrix graph)
  (local [; rest-lon : [List-of Number] -> [List-of Number]
          ; returns a list of number with the given list of number
          (define (rest-lon lon)
            (cond
              [(empty? lon) '()]
              [(cons? lon)
               (cons (connect? (first lon) (graph-v graph))
                     (rest-lon (rest lon)))]))
          ; connect? : Number [List-of Number] -> [List-of NUmber]
          ; returns a list of number with the given number and given list of number
          (define (connect? s l)
            (cond
              [(empty? l) '()]
              [(cons? l)
               (if ((graph-e graph) s (first l))
                   (cons 1 (connect? s (rest l)))
                   (cons 0 (connect? s (rest l))))]))]
    (append (rest-lon (graph-v graph)))))


; TODO #4: design the function am->graph that produces a graph given an adjacency matrix,
; as described in the previous todo. The vertices should be the numbers 1, 2, ... (length
; of the matrix). You have been supplied some tests for clarity.

; Hint: You will likely find the list-ref function quite useful.

; am->graph : [List-of [List-of {0, 1}]] -> Graph
; produces a graph given an adjacency matrix

(check-expect
 (graph-v
  (am->graph AM-3-1))
 (list 1 2 3))

(check-expect
 ((graph-e
   (am->graph AM-3-1))
  1 1)
 #false)

(check-expect
 ((graph-e
   (am->graph AM-3-1))
  1 2)
 #true)

(check-expect
 ((graph-e
   (am->graph AM-3-1))
  1 3)
 #false)

(check-expect
 ((graph-e
   (am->graph AM-3-1))
  2 1)
 #true)

(check-expect
 ((graph-e
   (am->graph AM-3-1))
  2 2)
 #false)

(check-expect
 ((graph-e
   (am->graph AM-3-1))
  3 1)
 #false)

(check-expect
 ((graph-e
   (am->graph AM-3-1))
  3 2)
 #false)

(define (am->graph matrix)
  (local [(define LIST (build-list (length matrix) add1))
          ; matrix->graph : Number Number [List-of Number] [List-of [List-of {0, 1}]] -> Boolean
          ; determines if the given destination returns #true with
          ; the given source, the list in the graph, and the matrix
          (define (matrix->graph s d los matrix)
            (cond
              [(empty? los) #false]
              [(cons? los)
               (if (= s (first los))
                   (= 1 (list-ref (first matrix) (sub1 d)))
                   (matrix->graph s d (rest los) (rest matrix)))]))]
    (make-graph LIST
                (λ (s d)
                  (matrix->graph s d LIST matrix)))))

; TODO #5: design the function redirect that takes a graph and two vertices,
; and then produces a new graph that doesn't contain the first vertex and in which
; all edges whose destination was previously the first vertex should now be the second.
; You have been supplied some tests for clarity.

; redirect : Graph Nat Nat -> Graph
; returns with a graph with a list that is without the first given vertex,
; and the edges whose destination is the first vertex should be changed to the second vertex

(check-expect
 (graph-v (redirect G-2-2 3 7))
 (list 7))

(check-expect
 ((graph-e (redirect G-2-2 3 7)) 7 7)
 #true)

(check-expect
 (graph-v (redirect G-2-2 7 3))
 (list 3))

(check-expect
 ((graph-e (redirect G-2-2 7 3)) 3 3)
 #true)

(check-expect
 (graph-v (redirect G-5-2 3 4))
 (list 1 2 4 5))

(check-expect
 ((graph-e (redirect G-5-2 3 4)) 2 5)
 #true)

(check-expect
 ((graph-e (redirect G-5-2 3 4)) 5 4)
 #true)

(check-expect
 (graph-v (redirect G-5-2 4 3))
 (list 1 2 3 5))

(check-expect
 ((graph-e (redirect G-5-2 4 3)) 2 5)
 #true)

(check-expect
 ((graph-e (redirect G-5-2 4 3)) 1 3)
 #true)

(define (redirect graph n1 n2)
  (local [; redirect-list : [List-of Number] Number -> [List-of Number]
          ; creates a new list with the given list and number
          ; by getting rid of the first number from the list
          (define (redirect-list lon n)
            (cond
              [(empty? lon) '()]
              [(cons? lon)
               (if (= n (first lon))
                   (redirect-list (rest lon) n)
                   (cons (first lon) (redirect-list (rest lon) n)))]))
          ; s->d : Number Number -> Boolean
          ; determines if the first given number is the destination,
          ; if it is, then determines if the destination is the second given number
          ; if not, follow the original graph to determine if the destination is #true
          (define (s->d s d)
            (if ((graph-e graph) s n1)
                (= d n2)
                ((graph-e graph) s d)))]
    (make-graph (redirect-list (graph-v graph) n1)
                (λ (s d)
                  (s->d s d)))))