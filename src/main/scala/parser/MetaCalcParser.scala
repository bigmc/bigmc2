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

sealed abstract class Term {
}

// lhs || rhs
case class TWPar(lhs:Term, rhs:Term) { }
// 0, the nil term for wide terms.
case class TZero { }
// (v ident) term, name restriction for wide contexts
case class TWideNew(ident:Ident, body:Term) { }
// [identL |-> identR], links.
case class TLink(identL:Ident, identR:Ident) { }
// c[idents].term, prefixing
case class TPrefix(control: Ident, idents: List[Ident], suffix : Term) { }
// p | p, parallel for prime contexts.
case class TPar(lhs:Term, rhs:Term) { }
// nil for prime processes.
case class TNil { }
// (v ident) term, name restriction for prime contexts
case class TNew(ident:Ident, body:Term) { }
// $n, hole with index n
case class THole(index:Int) { }


object MetaCalcParser extends StandardTokenParsers {
    lexical.delimiters ++= List(".","$","[","]","(",")","||","|","|->")

    def value = "$" ~ numericLit ^^ { s => THole(s.toInt) }

    def parse(s:String) = {
        val tokens = new lexical.Scanner(s)
        phrase(value)(tokens)
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

