package com.github.fex

import scala.util.parsing.combinator._
import collection.JavaConversions._

trait Getter{
  def getValue(key:Object):Any
}


abstract class Expr extends Function1[Getter,Option[Any]]{
  def evaluate(mappings:Getter):Boolean = apply(mappings) match{
      case Some(b:Boolean) => b
      case _ => false
  }
}

case class Number(num:Double) extends Expr{
  def apply(mappings:Getter) = Some(num)
}

case class Literal(value:String) extends Expr{
  def apply(mappings:Getter) = Some(value)
}

case class Var(name:String) extends Expr{
  def apply(mappings:Getter) = mappings.getValue(name) match {
    case b:java.lang.Boolean => Some(b.booleanValue())
    case n:java.lang.Number => Some(n.doubleValue())
    case d:java.util.Date => Some((d.getTime() / 1000) toDouble)
    case s:java.lang.String => Some(s)
    case _ => None
  }    
}

case class Max(x:Expr,y:Expr) extends Expr{
  def apply(mappings:Getter) = {
    val eval= (e:Expr) => e(mappings).collect({case n:Double => n})
    (for(a <- eval(x);b <- eval(y)) yield Math.max(a,b)) headOption
  }
}

case class Min(x:Expr,y:Expr) extends Expr{
  def apply(mappings:Getter) = {
    val eval= (e:Expr) => e(mappings).collect({case n:Double => n})
    (for(a <- eval(x);b <- eval(y)) yield Math.min(a,b)) headOption
  }
}

case class Contains(x:Expr,y:Expr) extends Expr{
  def apply(mappings:Getter) = {
    val eval= (e:Expr) => e(mappings).collect({case s:String => s})
    (for(a <- eval(x);b <- eval(y)) yield a.contains(b)) headOption
  }
}

case class StartsWith(x:Expr,y:Expr) extends Expr{
  def apply(mappings:Getter) = {
    val eval= (e:Expr) => e(mappings).collect({case s:String => s})
    (for(a <- eval(x);b <- eval(y)) yield a.startsWith(b)) headOption
  }
}

case class Lc(x:Expr) extends Expr{
  def apply(mappings:Getter) = x(mappings).collect{
    case s:String => s.toLowerCase
  }
}

case class Uc(x:Expr) extends Expr{
  def apply(mappings:Getter) = x(mappings).collect{
    case s:String => s.toUpperCase
  }
}

case class Exp(x:Expr) extends Expr{
  def apply(mappings:Getter) = x(mappings).collect{
    case n:Double => Math.exp(n)
  }
}

case class Log(x:Expr) extends Expr{
  def apply(mappings:Getter) = x(mappings).collect{
    case n:Double => Math.log(n)
  }
}

case class Exists(x:Expr) extends Expr{
  def apply(mappings:Getter) = Some(x(mappings).isDefined)
}

case class Date() extends Expr{
  def apply(mappings:Getter) = Some((System.currentTimeMillis / 1000) toDouble)
}

case class Addition(left:Expr,right:Expr) extends Expr{
  def apply(mappings:Getter) = (left(mappings),right(mappings)) match{
    case (Some(x:Double),Some(y:Double)) => Some(x+y)
    case _ => None
  }
}

case class Subtraction(left:Expr,right:Expr) extends Expr{
  def apply(mappings:Getter) = (left(mappings),right(mappings)) match{
    case (Some(x:Double),Some(y:Double)) => Some(x-y)
    case _ => None
  }
}

case class Division(dividend:Expr,divisor:Expr) extends Expr{
  def apply(mappings:Getter) = (dividend(mappings),divisor(mappings)) match{
    case (Some(x:Double),Some(y:Double)) => Some(x/y)
    case _ => None
  }
}

case class Multiplication(left:Expr,right:Expr) extends Expr{
  def apply(mappings:Getter) = (left(mappings),right(mappings)) match{
    case (Some(x:Double),Some(y:Double)) => Some(x*y)
    case _ => None
  }
}

case class Not(value:Expr) extends Expr{
  def apply(mappings:Getter) = value(mappings).collect({case (b:Boolean) => !b})
}

case class And(left:Expr,right:Expr) extends Expr{
  def apply(mappings:Getter) = left(mappings) match{
    case Some(true) => right(mappings).collect({case (b:Boolean) => b})
    case x => x
  }
}

case class Or(left:Expr,right:Expr) extends Expr{
  def apply(mappings:Getter) = left(mappings) match{
    case Some(false) => right(mappings).collect({case (b:Boolean) => b})
    case x => x
  }
}


case class Xr(left:Expr,right:Expr) extends Expr{
  def apply(mappings:Getter) = (left(mappings),right(mappings)) match{
    case (Some(x:Boolean),Some(y:Boolean)) => Some(x ^ y)
    case _ => None
  }
}

case class Lt(left:Expr,right:Expr) extends Expr{
  def apply(mappings:Getter) = (left(mappings),right(mappings)) match{
    case (Some(x:Double),Some(y:Double)) => Some(x<y)
    case (Some(x:String),Some(y:String)) => Some(x<y)
    case _ => None
  }
}

case class Lte(left:Expr,right:Expr) extends Expr{
  def apply(mappings:Getter) = (left(mappings),right(mappings)) match{
    case (Some(x:Double),Some(y:Double)) => Some(x<=y)
    case (Some(x:String),Some(y:String)) => Some(x<=y)
    case _ => None
  }
}

case class Gt(left:Expr,right:Expr) extends Expr{
  def apply(mappings:Getter) = (left(mappings),right(mappings)) match{
    case (Some(x:Double),Some(y:Double)) => Some(x>y)
    case (Some(x:String),Some(y:String)) => Some(x>y)
    case _ => None
  }
}

case class Gte(left:Expr,right:Expr) extends Expr{
  def apply(mappings:Getter) = (left(mappings),right(mappings)) match{
    case (Some(x:Double),Some(y:Double)) => Some(x>=y)
    case (Some(x:String),Some(y:String)) => Some(x>=y)
    case _ => None
  }
}

case class Eq(left:Expr,right:Expr) extends Expr{
  def apply(mappings:Getter) = (left(mappings),right(mappings)) match{
    case (Some(x),Some(y)) => Some(x==y)
    case _ => None
  }
}

case class Ne(left:Expr,right:Expr) extends Expr{
  def apply(mappings:Getter) = (left(mappings),right(mappings)) match{
    case (Some(x),Some(y)) => Some(x!=y)
    case _ => None
  }
}

class FexParser extends RegexParsers {

  def parseAsFex(s:String):ParseResult[Expr]  = parseAll(expr,s)

  private def fold:(~[Expr,List[~[String,Expr]]] => Expr) = {
    case x~Nil => x
    case x~xs => xs.foldLeft(x)((x,ys)=>ys match{
      case "+"~y => Addition(x,y)
      case "-"~y => Subtraction(x,y)
      case "/"~y => Division(x,y)
      case "*"~y => Multiplication(x,y)
      case "|"~y => Or(x,y)
      case "&"~y => And(x,y)
      case "^"~y => Xr(x,y)
    })
  }

  def expr: Parser[Expr] = predicate~rep("&"~predicate | "|"~predicate | "^"~predicate) ^^ fold

  def predicate: Parser[Expr] = negation | compare | arith_expr

  def negation: Parser[Expr] = "!"~>predicate ^^ (x=>Not(x))

  def compare: Parser[Expr] = arith_expr~(">"|"<"|">="|"<="|"="|"!=")~arith_expr ^^ {
    case x~"<"~y => Lt(x,y)
    case x~"<="~y => Lte(x,y)
    case x~">"~y => Gt(x,y)
    case x~">="~y => Gte(x,y)
    case x~"="~y => Eq(x,y)
    case x~"!="~y => Ne(x,y)
  } 

  def arith_expr: Parser[Expr] = term~rep("+"~term | "-"~term) ^^ fold

  def term: Parser[Expr] = factor~rep("*"~factor | "/"~factor) ^^ fold

  def factor: Parser[Expr] = number | literal | function | variable | "("~>(expr<~")")

  def number: Parser[Expr] = """-?(\d+(\.\d*)?|\d*\.\d+)""".r ^^ (s=>Number(s.toDouble))

  def literal: Parser[Expr] = """'([^']*)'""".r ^^ (s=>Literal(s.substring(1,s.length-1)))

  def variable: Parser[Expr] = ident ^^ (x=>Var(x))

  def function: Parser[Expr] = ident~("("~>(repsep(expr,",")<~")")) ^? ({
    case "contains"~List(a,b) => Contains(a,b)
    case "startsWith"~List(a,b) => StartsWith(a,b)
    case "max"~List(a,b) => Max(a,b)
    case "min"~List(a,b) => Min(a,b)
    case "lc"~List(a) => Lc(a)
    case "uc"~List(a) => Uc(a)
    case "exp"~List(a) => Exp(a)
    case "log"~List(a) => Log(a)
    case "exists"~List(a) => Exists(a)
    case "date"~Nil => Date()
  },{
    case r~_ => "bad function name or number of parameters "+r
  })

  private def ident: Parser[String] = """[a-zA-Z][_a-zA-Z0-9]*""".r

}
