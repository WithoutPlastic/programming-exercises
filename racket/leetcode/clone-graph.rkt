#lang racket

;Problem:
;Clone an undirected graph. Each node in the graph contains a label and a list
;of its neighbors.
;
;OJ's undirected graph serialization:
;
;Nodes are labeled uniquely.
;We use # as a separator for each node, and , as a separator for node label and
;each neighbor of the node.
;
;As an example, consider the serialized graph {0,1,2#1,2#2,2}.
;
;The graph has a total of three nodes, and therefore contains three parts as
;separated by #.
;
; - First node is labeled as 0. Connect node 0 to both nodes 1 and 2.
; - Second node is labeled as 1. Connect node 1 to node 2.
; - Third node is labeled as 2. Connect node 2 to node 2 (itself), thus forming a self-cycle.
;
;Visually, the graph looks like the following:
;
;       1
;      / \
;     /   \
;    0 --- 2
;         / \
;         \_/

;Utils
(define make-node mcons)
(define node-payload mcar)
(define node-connections mcdr)
(define node-set-payload! set-mcar!)
(define node-set-connections! set-mcdr!)

(define [clone-graph node]
  (define [list-elt-idx lst elt]
    (define [iter remaining cnt]
      (cond ([null? remaining] false)
            ([eq? (car remaining) elt] cnt)
            (else (iter (cdr remaining) (add1 cnt)))))

    (iter lst 0))
  (define [bfs-result node]
    (define [iter pendings knowns]
      (if [null? pendings] knowns
        (let* ([ext-nodes (append-map node-connections pendings)]
               [not-visited-nodes (filter-not (curryr memq knowns) ext-nodes)])
          (iter not-visited-nodes (append knowns not-visited-nodes)))))

    (iter (list node) (list node)))

  (let* ([result (bfs-result node)]
         [copied (map (compose (curryr make-node '()) node-payload) result)])
    (define [build-connections! copied-nodes nodes]
      (map (λ [cp od] (node-set-connections! cp
                        (map (compose (curry list-ref copied-nodes)
                                      (curry list-elt-idx nodes))
                             (node-connections od))))
           copied-nodes nodes))

    (build-connections! copied result)
    (car copied)))

(let ([first-node (make-node 0 '())]
      [second-node (make-node 1 '())]
      [third-node (make-node 2 '())])
  (node-set-connections! first-node (list second-node third-node))
  (node-set-connections! second-node (list first-node third-node))
  (node-set-connections! third-node (list first-node second-node third-node))
  (displayln first-node)
  (let ([copied (clone-graph first-node)])
    (displayln copied)
    [eq? copied first-node]))
