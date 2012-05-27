package org.bigraph.bigmc.parser

import org.bigraph.bigmc._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

import scala.collection.immutable.Map
import scala.collection.immutable.Set

/*
nameList ::= x, nameList
         ::= x

l ::= forall nameList : u

u ::= x parent-of y
  ::= x child-of y
  ::= x ancestor-of y
  ::= x descendant-of y
  ::= x child-of y
  ::= x_n -/- y_n
  ::= x_n != y_n
  ::= u \/ u
  ::= u /\ u
  ::= ctrl(x) = y
  ::= ( u )

*/


sealed abstract class LOper {

}

case class ParentOf extends LOper { }
case class ChildOf extends LOper { }
case class AncestorOf extends LOper { }
case class DescendantOf extends LOper { }
case class NotLinked extends LOper { }
case class NotEqual extends LOper { }
case class And extends LOper { }
case class Or extends LOper { }
case class CtrlEq extends LOper { }

sealed abstract class LTerm {

}

case class LBinaryPred(lhs:LTerm, pred: LOper, rhs:LTerm) extends LTerm { }
case class LIdent(id : String) extends LTerm { }
case class LPort(lhs:LTerm, id : Int) extends LTerm { }


object LogicParser extends StandardTokenParsers {
    lexical.delimiters ++= List("(",")","↖", "↘", "⇱","⇲","≁","∧","∨","∀",":","," )
    lexical.reserved ++= List("ctrl")

    lazy val expr = "ctrl" ^^^ { LIdent("ctrl") }

    def parse(s:String) = {
        val tokens = new lexical.Scanner(s)
        phrase(expr)(tokens)
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


