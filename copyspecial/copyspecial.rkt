#lang racket
(require racket/cmdline)


(define USAGE "usage: [--todir dir][--tozip zipfile] dir [dir ...]")

(parameterize ([todir ""]
               [tozip ""])

(command-line
 #:program "copyspecial"
 #:once-each
 [("--todir") dir
  ...]
 [("--tozip") zipfile
  ...]

 #:args
 dirs
 ))
