Herbie support for generic IEEE-754 floating-point numbers
===

This repository contains a plugin for [Herbie](https://herbie.uwplse.org) to support generic IEEE-754 floating-point numbers

Floating-point numbers are defined with `es` integer bits and `n` total bits. Use in Herbie by specifying `:precision (float es n)`.

This package contains:

+ Definitions of generic floating-point numbers and their operators for Herbie
+ Rewrite rules for these operators

Installation:

1. Clone this repository
2. Run `raco pkg install -n float-herbie` in this folder