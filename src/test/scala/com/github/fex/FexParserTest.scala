package com.github.fex

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuiteLike
import collection.JavaConversions._

@RunWith(classOf[JUnitRunner])
class FexParserTest extends FexParser with FunSuiteLike{

  test("parse basic arichmetics"){
    val Success(Eq(Subtraction(Addition(a,b),c),d),_) = parseAll(expr,"1 + 2 - 3 = 0")
    assert(Number(1) == a)
    assert(Number(2) == b)
    assert(Number(3) == c)
    assert(Number(0) == d)
    val Success(Gt(Multiplication(x,y),z),_) = parseAll(expr,"(-1)*7 > 0")
    assert(Number(-1) == x)
    assert(Number(7) == y)
    assert(Number(0) == z)
    val Success(Lt(Division(k,m),l),_) = parseAll(expr,"1/7 < 0")
    assert(Number(1) == k)
    assert(Number(7) == m)
    assert(Number(0) == l)
  }


  test("parse literal"){
    val Success(result,_) = parseAll(expr,"'s1' + 's2' < 'aa'")
    assert(Lt(Addition(Literal("s1"),Literal("s2")),Literal("aa")) === result)
  }

  test("parse variable"){
    val Success(Gt(Addition(Var(x),_),_),_) = parseAll(expr,"ts_name + 1>1/2")
    assert("ts_name" === x)
  }

  test("exists function"){
    val Success(result,_) = parseAll(expr,"exists(x + y)")
    assert(Exists(Addition(Var("x"),Var("y"))) === result)
  }

  test("max function"){
    val Success(result,_) = parseAll(expr,"max(0,1)=1")
    assert(Eq(Max(Number(0),Number(1)),Number(1))=== result)
  }

  test("startsWith function"){
    val Success(result,_) = parseAll(expr,"startsWith(abcd,'a')")
    assert(StartsWith(Var("abcd"),Literal("a")) === result)
  }

  test("evaluate exists function"){
    val Success(e,_) = parseAll(expr,"!exists(not_initialized)")
    assert(true === e.evaluate(Map[Object,Any]("not_initialized"->null)))
  }

  test("date function"){
    val Success(e,_) = parseAll(expr,"(date() - time_var)/(3600*24)=1")
    assert(true === e.evaluate(Map[Object,Any]("time_var" -> new java.util.Date(System.currentTimeMillis - 24*3600*1000))))
  }

  test("boolean expression test"){
    val Success(result,_) = parseAll(expr,"startsWith('abcd','a') & 1+globalvar/10-7>0 | (27=exp(var2) & log(var3*var4)>1)")
    assert(Or(And(
                StartsWith(Literal("abcd"), Literal("a")),
                Gt(Subtraction(Addition(Number(1),Division(Var("globalvar"),Number(10))),Number(7)),Number(0))),
              And(
                Eq(Number(27),Exp(Var("var2"))),
                Gt(Log(Multiplication(Var("var3"),Var("var4"))),Number(1)))) === result)
  }

  private implicit def getter(fn:Map[Object,Any]):Getter = new Getter{
    def getValue(k:Object) = fn.getOrElse(k,null)
  }

  
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

  test("not resolved function"){
    val Failure(msg,input) = parseAll(expr,"1+2=functionX() & true")
    assert(false === msg.isEmpty)
    assert(15 === input.offset)
  }

  test("not initialized variable"){
    val Success(result,_) = parseAll(expr,"variable")
    assert(None === result(Map[Object,Getter]()))
  }


  def testEmpty = ()
  
}
