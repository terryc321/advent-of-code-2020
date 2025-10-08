# day2

puzzle if a character appears in some range from min to max number of times .

data parsed using antlr java library which clojure can easily access.

using antlr seems fairly easy ,

admittedly its a simple one shot parse - if it works okay , but what if it does not , what
chance is there of error correction or taking some alternative action ?

hands in air : shrug :

```
include [clj-antlr "0.2.14"] in project.clj dependencies
```


The Day2 Grammar file we created , not used to antlr at moment , but it did its job great.
bit of trial and error.

```

/*

int - int char: chars+ 

*/
grammar Day2;

/* dash INT WS ALPHA COLON WS alphas */
top:
 INT DASH INT WS ALPHA COLON WS ALPHAS
;

DASH: '-' ;

INT: [0-9] + ; 

COLON: ':' ;

dash: '-' ;

ALPHA:  [a-z] ;

ALPHAS: [a-z] + ;

WS : ' ' ;


/*
WS
  : ( ' '
  | '\t'
  | '\n'
  | '\r'
  ) -> channel(HIDDEN)
;
*/

```



