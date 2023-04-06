# prettier-builtins

Stop being annoyed by Common Lisp and start using prettier-builtins.

```lisp
CL-USER> (ql:quickload "prettier-builtins")
To load "prettier-builtins":
  Load 1 ASDF system:
    prettier-builtins
; Loading "prettier-builtins"

("prettier-builtins")
; processing (DEFPACKAGE :PRETTIER-BUILTINS ...)
CL-USER> (prettier-builtins:prettify '(and array (not string)) 'hash-table) ; currently only these two types are supported
NIL
CL-USER> (ql:quickload "array-operations")
To load "array-operations":
  Load 1 ASDF system:
    array-operations
; Loading "array-operations"

("array-operations")
CL-USER> (aops:rand* 'single-float '(5 5))
#<SIMPLE-ARRAY :ROW-MAJOR 5x5 SINGLE-FLOAT
  (  0.914       0.220       0.971       0.596       0.601    )
  (  0.594       0.284       0.010       0.844       0.225    )
  (  0.831       0.280       0.584       0.757       0.919    )
  (  0.007       0.311       0.596       0.071       0.723    )
  (  0.698       0.424       0.868       0.363       0.357    )
   {1007E0230F}>
CL-USER> (setq *print-length* 10)
10
CL-USER> (alexandria:plist-hash-table (iota 100))
#<HASH-TABLE :TEST 50 :COUNT EQL
  0                 =>  1
  2                 =>  3
  4                 =>  5
  6                 =>  7
  8                 =>  9
  10                =>  11
  12                =>  13
  14                =>  15
  16                =>  17
  18                =>  19
  ...
   {1009F7AB53}>
```
