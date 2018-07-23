# parser

A simple parser for DSL.

* Boolean operators: | (or) , & (and) , ^ (xor)
* Algebra: +, - , /, *
* Inequalities: <,<=, >, =>, !=, =
* Built-in functions: startsWith, max, min, lc, uc, exp, log, exists, date

Examples:


(1 + 2 - 3 = 0)  |  (-1)*7 > 0 


's1' + 's2' < 'aa' - literals


max(0,1)=1 - maximum


startsWith(abcd,'a') - variable abcd starts with 'a'


How to use:
* Install maven
* mvn test


