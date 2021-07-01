#!/usr/bin/env racket

#lang racket

(require pict pict/tree-layout file/convertible)

(define (framed-text str)
  (define txt (text str 'modern 32))
  (pin-over (filled-rectangle (+ 4 (pict-width txt))
                              (+ 4 (pict-height txt))
                              #:border-color "black"
                              #:color "white"
                              #:border-width 3)
            2 2
            txt))

(define (ast->tree node)
  (cond
    [(list? node)
     (define typed? (bytes? (cadr node)))
     (define ast-node
       (apply tree-layout
              #:pict (framed-text (symbol->string (car node)))
              (map ast->tree ((if typed? cddr cdr) node))))
     (if typed?
         (tree-layout #:pict (framed-text (~a (cadr node)))
                      (tree-edge ast-node #:edge-style 'dot))
         ast-node)]

    [(string? node)
     (tree-layout
      #:pict (framed-text (~s node)))]))

(define tree
  (linewidth
   5
   (naive-layered
    (ast->tree (read))
    #:y-spacing 64)))

(write-bytes (convert (pict->bitmap tree) 'png-bytes))
