package org.bigraph.bigmc.parser

import org.bigraph.bigmc._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/*
 r ::= r || r
   ::= p
   ::= 0
   ::= (vx) r
   ::= [x |-> y]
   ::= (r)
 
p ::= c[nameList].p
  ::= c.p
  ::= p | p
  ::= nil
  ::= (vx) p
  ::= $n
  ::= (p)

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

*/


class Ident(id:String) {
    override def toString:String = id
}

sealed abstract class Term {
}

// lhs || rhs
case class TWPar(lhs:Term, rhs:Term) extends Term { }
// 0, the nil term for wide terms.
case class TZero extends Term { }
// (v ident) term, name restriction for wide contexts
case class TWideNew(ident:Ident, body:Term) extends Term { }
// [identL |-> identR], links.
case class TLink(identL:Ident, identR:Ident) extends Term { }
// c[idents].term, prefixing
case class TPrefix(control: Ident, idents: List[Ident], suffix : Term) extends Term { }
// p | p, parallel for prime contexts.
case class TPar(lhs:Term, rhs:Term) extends Term { }
// nil for prime processes.
case class TNil extends Term { }
// (v ident) term, name restriction for prime contexts
case class TNew(ident:Ident, body:Term) extends Term { }
// $n, hole with index n
case class THole(index:Int) extends Term { }


object MetaCalcParser extends StandardTokenParsers {
    lexical.delimiters ++= List(".","$","[","]","(",")","||","|","|->")
    lexical.reserved ++= List("nil","0")

    lazy val hole = "$" ~ numericLit ^^ { case a ~ b => THole(b.toInt) }
    lazy val nil = "nil" ^^^ { TNil() }
    lazy val zero = "0" ^^^ { TZero() }
    lazy val ctrl = ident ^^ { s => new Ident(s) }

    lazy val prefix = ctrl ~ ("." ~> expr) ^^ {
        case c ~ s => TPrefix(c,List(),s)
    }

    lazy val terminal = hole | nil | zero | prefix

    lazy val expr : Parser[Term] = terminal | ("(" ~> expr <~ ")")

    def parse(s:String) = {
        val tokens = new lexical.Scanner(s)
        phrase(expr)(tokens)
    }
    
    def apply(s:String):Term = {
        parse(s) match {
            case Success(tree, _) => tree
            case e: NoSuccess =>
                   throw new IllegalArgumentException("Bad syntax: "+s)
        }
    }

    def test(exprstr: String) = {
        parse(exprstr) match {
            case Success(tree, _) => true
            case e: NoSuccess => {
                Console.err.println(e);
                false
            }
        }
    }
}

