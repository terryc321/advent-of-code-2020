# aoc 2020 day 19

# Continuation passing style (CPS)

CPS continuation passing style gave control required to
traverse regular expression tree

see chicken directory for code that implements this 

# Sequential branch

Do this followed by this

If want br0 to succeed , br1 must succeed followed by br2

```lisp
br0 : (seq br1 br2)

(define (br0 str good bad)
  (br1 str
       (lambda (str2)
          (br2 str2 good bad))
       bad))
```


# Alternate branch

Do this , if it fails , do that

If want br0 to succeed , either br1 must succeed or br2 must succed

In this case we assume if br1 succeeds then thats all we need to do.
There could be a case when even IF br1 succeeds , we may want to
revisit this situation and choose to ALSO go down br2 branch with str

```lisp
br0 : (alt br1 br2)

(define (br0 str good bad)
  (br1 str
       good
       (lambda (str2)
          (br2 str good bad))))
```


# Random Thoughts

almost like a regular expression pattern recogniser 

given a pattern some sequence of letter a and sequence letter b mixed up 
does table index 0 yield that pattern

is it a search problem ?

is the problem such that we can narrow the potential strings produced and thereby
easily disgard patterns that do not match

