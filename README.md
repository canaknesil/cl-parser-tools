# cl-parser-tools 

Parser tools for Common Lisp.

The purpose of this project is to better understand parser concepts by
implementing myself.

## Tools

### nullable-first-follow

This tool takes a grammar from the user and computes nullable, FIRST,
and FOLLOW for each terminal and non-terminal verbosing each iteration.

    (nullable-first-follow
      (create-grammar
        (z b) (z x y z)
        (x a) (x y)
        (y c) (y)))
			  
    nullable(Y) = f
    nullable(C) = f
    nullable(X) = f
    nullable(A) = f
    nullable(Z) = f
    nullable(B) = f
    FIRST(Y) = {}
    FIRST(C) = {C}
    FIRST(X) = {}
    FIRST(A) = {A}
    FIRST(Z) = {}
    FIRST(B) = {B}
    FOLLOW(Y) = {}
    FOLLOW(C) = {}
    FOLLOW(X) = {}
    FOLLOW(A) = {}
    FOLLOW(Z) = {}
    FOLLOW(B) = {}
    
    nullable(Y) = t
    nullable(C) = f
    nullable(X) = f
    nullable(A) = f
    nullable(Z) = f
    nullable(B) = f
    FIRST(Y) = {C}
    FIRST(C) = {C}
    FIRST(X) = {A}
    FIRST(A) = {A}
    FIRST(Z) = {B}
    FIRST(B) = {B}
    FOLLOW(Y) = {B}
    FOLLOW(C) = {B}
    FOLLOW(X) = {}
    FOLLOW(A) = {}
    FOLLOW(Z) = {}
    FOLLOW(B) = {}
    
    nullable(Y) = t
    nullable(C) = f
    nullable(X) = t
    nullable(A) = f
    nullable(Z) = f
    nullable(B) = f
    FIRST(Y) = {C}
    FIRST(C) = {C}
    FIRST(X) = {C, A}
    FIRST(A) = {A}
    FIRST(Z) = {A, B}
    FIRST(B) = {B}
    FOLLOW(Y) = {C, A, B}
    FOLLOW(C) = {A, C, B}
    FOLLOW(X) = {B, A, C}
    FOLLOW(A) = {C, A, B}
    FOLLOW(Z) = {}
    FOLLOW(B) = {}
    
    nullable(Y) = t
    nullable(C) = f
    nullable(X) = t
    nullable(A) = f
    nullable(Z) = f
    nullable(B) = f
    FIRST(Y) = {C}
    FIRST(C) = {C}
    FIRST(X) = {A, C}
    FIRST(A) = {A}
    FIRST(Z) = {B, A, C}
    FIRST(B) = {B}
    FOLLOW(Y) = {C, A, B}
    FOLLOW(C) = {A, C, B}
    FOLLOW(X) = {A, B, C}
    FOLLOW(A) = {C, A, B}
    FOLLOW(Z) = {}
    FOLLOW(B) = {}
    

