# day6

split input based on empty lines
then we have groups of strings

simply record which character appears uniquely in those strings a - z
atmost 26 most characters

used clojure map and merge
```
(set (seq "asdfasdfasdf"))
```

would have done the job , but we used hash map and merge , meh



