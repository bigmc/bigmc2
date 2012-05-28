package org.bigraph.bigmc.parser

import org.bigraph.bigmc._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.immutable.List

/*
nameList ::= x, nameList
         ::= x

l ::= forall nameList : u

u ::= x parent-of y
  ::= x child-of y
  ::= x ancestor-of y
  ::= x descendant-of y
  ::= x child-of y
  ::= x[n] -/- y_n
  ::= x_n != y_n
  ::= u \/ u
  ::= u /\ u
  ::= ctrl(x) = y
  ::= ctrl(x) != y
  ::= ( u )
  ::= u => u

*/


sealed abstract class LOper {

}

case class ParentOf extends LOper { override def toString = "↖" }
case class ChildOf extends LOper { override def toString = "↘" }
case class AncestorOf extends LOper { override def toString = "⇱" }
case class DescendantOf extends LOper { override def toString = "⇲" }
case class NotLinked extends LOper { override def toString = "-/-" }
case class Linked extends LOper { override def toString = "--" }
case class NotEqual extends LOper { override def toString = "!=" }
case class Equal extends LOper { override def toString = "=" }
case class LAnd extends LOper { override def toString = "∧" }
case class LOr extends LOper { override def toString = "∨" }
case class CtrlEq extends LOper { override def toString = "has-ctrl" }
case class Implies extends LOper { override def toString = "=>" }

sealed abstract class LTerm {

}

case class LBinOp(lhs:LTerm, pred: LOper, rhs:LTerm) extends LTerm { override def toString = "(" + lhs + " " + pred + " " + rhs + ")" }
case class LIdent(id : String) extends LTerm { override def toString = id }
case class LPort(lhs:LTerm, id : Int) extends LTerm { override def toString = lhs + "[" + id + "]" }
case class LPredicate(vars: List[String], body:LTerm) extends LTerm { override def toString = "∀" + vars.mkString(",") + " : " + body }


object LogicParser extends StandardTokenParsers {
    lexical.delimiters ++= List("(",")","↖", "↘", "⇱","⇲","-/-","∧","∨","∀",":",",","[","]","!=","=>","=","--" )
    lexical.reserved ++= List("ctrl")

    lazy val ctrl = "ctrl" ~> ("(" ~> ident <~ ")") ~ ("=" ~> ident) ^^ { case x ~ y => LBinOp(LIdent(x),CtrlEq(),LIdent(y)) }

    lazy val term = ctrl | (ident ~ ("[" ~> numericLit <~ "]")) ^^ { case x ~ y => LPort(LIdent(x),y.toInt) } | ident ^^ { x => LIdent(x) } 

    lazy val binop = "↖" ^^^ {ParentOf()} | "↘" ^^^ {ChildOf()} | "⇱" ^^^ {AncestorOf()} | "⇲" ^^^ {DescendantOf()} | "-/-" ^^^ {NotLinked()} | "!=" ^^^ {NotEqual()} | "--" ^^^ {Linked()} | "=" ^^^ {Equal()}

    lazy val expr : Parser[LTerm] = term ~ (binop ~ expr) ^^ { case x ~ (y ~ z) => LBinOp(x,y,z) } | term | "(" ~> expr <~ ")"

    lazy val tlexpr : Parser[LTerm] = expr ~ ("∧" ~> tlexpr) ^^ { case x ~ y => LBinOp(x,LAnd(),y) } | expr ~ ("∨" ~> tlexpr) ^^ { case x ~ y => LBinOp(x,LOr(),y) } | "(" ~> tlexpr <~ ")" | expr

    lazy val impl : Parser[LTerm] = tlexpr ~ ("=>" ~> tlexpr) ^^ { case x ~ y => LBinOp(x,Implies(),y) } | tlexpr

    lazy val nameList : Parser[List[String]] = ident ~ ("," ~> nameList) ^^ { case x ~ y => x :: y } | ident ^^ { x => List(x) }

    lazy val pred = "∀" ~> ((nameList <~ ":") ~ impl) ^^ { case x ~ y => LPredicate(x,y) } 

    def parse(s:String) = {
        val tokens = new lexical.Scanner(s)
        phrase(pred)(tokens)
    }
    
    def apply(s:String):LTerm = {
        parse(s) match {
            case Success(tree, _) => tree
            case e: NoSuccess => {
                Console.err.println(e);
                   throw new IllegalArgumentException("Bad syntax: "+s)
            }
        }
    }

    def test(exprstr: String) = {
        parse(exprstr) match {
            case Success(tree, _) => {true}
            case e: NoSuccess => {
                Console.err.println(e);
                false
            }
        }
    }

}


