#lang info

(define name "float-herbie")
(define deps '("math-lib" "base" "generic-flonum" ("herbie" #:version "1.5")))
(define pkg-desc "Herbie plugin for generic IEEE-754 floating-point numbers")
(define version "1.0")
(define pkg-authors '("Brett Saiki"))

(define herbie-plugin 'float-herbie)
