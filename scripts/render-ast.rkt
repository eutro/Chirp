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
     (apply tree-layout
            #:pict (framed-text (symbol->string (car node)))
            (map ast->tree (cdr node)))]

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
