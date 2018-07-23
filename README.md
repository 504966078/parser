# parser

A simple parser for DSL.

* Boolean operators: | (or) , & (and) , ^ (xor)
* Algebra: +, - , /, *
* Inequalities: <,<=, >, =>, !=, =
* Built-in functions: startsWith, max, min, lc, uc, exp, log, exists, date

A test case:


  test("evaluate expression"){
    
    val Success(e,_) = parseAll(expr,"startsWith(abcd,'a') & 1+globalvar/10-7>0")
    
    assert(true === e.evaluate(Map[Object,Any](
        "globalvar" -> 70.0,
        "abcd" -> "abcd")))
    
    assert(false === e.evaluate(Map[Object,Any](
        "globalvar" -> 70.0,
        "abcd" -> "bcd")))
    
    assert(false === e.evaluate(Map[Object,Any](
        "globalvar" -> 0.0,
        "abcd" -> "abcd")))
    
    assert(false === e.evaluate(Map[Object,Any]()))
  }


How to use:
* Install maven
* mvn test


