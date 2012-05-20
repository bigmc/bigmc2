package org.bigraph.bigmc.parser

import org.bigraph.bigmc._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/*
 r ::= r || r
   ::= p
   ::= 0
   ::= (νx) r
   ::= [x |-> y]
   ::= (r)
 
p ::= c[nameList].p
  ::= c.p
  ::= p | p
  ::= nil
  ::= (νx) p (keep in mind that's a 'ν' character, not a 'v'!)
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


class Ident(val id:String) {
    override def toString:String = id

    override def equals(other : Any) : Boolean = other match {
        case that : Ident => that.id == id
        case that : String => that == id
        case _ => false
    }

}

object FreshName {
    var n = 0
    def generate : Ident = {
        n = n + 1
        new Ident("_v" + n)
    }
}

sealed abstract class Term {
    def normaliseOnce(t : Term) : Term = t match {
        case TWPar(TNew(i,r),rp) => {
            if(!rp.freeNames(Set()).contains(i)) {
                // 'i' does not occur free, we can safely lift the New to the top level.
                TNew(i,TWPar(normaliseOnce(r),normaliseOnce(rp)))
            } else {
                // 'i' occurs free, we need to substitute it out from 'r' before lifting it.
                val fr = FreshName.generate
                val nr = r.substitute(i,fr)
                TNew(fr,TWPar(normaliseOnce(nr),normaliseOnce(rp)))
            }
        }
        case TWPar(x,TNew(i,r)) => normaliseOnce(TWPar(TNew(i,r),x))
        case TWPar(TZero(),l) => TWPar(normaliseOnce(l),TZero())
        case TWPar(l,TWPar(m,n)) => TWPar(TWPar(normaliseOnce(l),normaliseOnce(m)),normaliseOnce(n))
        case TWPar(l,r) => TWPar(normaliseOnce(l),normaliseOnce(r))
        case TPar(TNil(),l) => normaliseOnce(l)
        case TPar(l,TNil()) => normaliseOnce(l)
        case TPar(l,TPar(m,n)) => TPar(TPar(normaliseOnce(l),normaliseOnce(m)),normaliseOnce(n))
        case TPar(l,r) => TPar(normaliseOnce(l),normaliseOnce(r))
        case TPrefix(c,l,TNew(i,r)) => {
            if(!l.contains(i)) {
                // c[l] doesn't contain i, we can lift immediately.
                TNew(i,TPrefix(c,l,normaliseOnce(r)))
            } else {
                // i occurs in c[l], need to rename first, then lift.
                val fr = FreshName.generate
                val nr = r.substitute(i,fr)
                TNew(fr,TPrefix(c,l,normaliseOnce(nr)))
            }
        }
        case TPrefix(c,l,s) => TPrefix(c,l,normaliseOnce(s))
        case x => x
    }

    def normalise(t:Term) : Term = {
        def iter (t1:Term,t2:Term) : Term = {
            if(t1.toString == t2.toString) {
                t1
            } else {
                iter(t2,normaliseOnce(t2))
            }
        }

        iter(t,normaliseOnce(t))
    }

    def freeNames(bound : Set[Ident]) : Set[Ident]

    def substitute(x : Ident, y : Ident) : Term
}

// lhs || rhs
case class TWPar(lhs:Term, rhs:Term) extends Term {
    override def toString = "(" + lhs + " || " + rhs + ")"

    def freeNames(bound : Set[Ident]) = lhs.freeNames(bound) ++ rhs.freeNames(bound)

    def substitute(x: Ident, y: Ident) = TWPar(lhs.substitute(x,y), rhs.substitute(x,y))
}
// 0, the nil term for wide terms.
case class TZero extends Term {
    override def toString = "0"

    def freeNames(bound : Set[Ident]) = Set()

    def substitute(x: Ident, y: Ident) = this
}
// [identL |-> identR], links.
case class TLink(identL:Ident, identR:Ident) extends Term { 
    override def toString = "[" + identL + " |-> " + identR + "]"

    def freeNames(bound : Set[Ident]) = Set()
    def substitute(x: Ident, y: Ident) = {
	val lp = if(x == identL) y else identL
	val rp = if(x == identR) y else identR
	TLink(lp,rp)
    }
}
// c[idents].term, prefixing
case class TPrefix(control: Ident, idents: List[Ident], suffix : Term) extends Term {
    override def toString = control + "[" + idents.mkString(",") + "]." + suffix

    def freeNames(bound : Set[Ident]) = (idents.toSet -- bound) ++ suffix.freeNames(bound)
    def substitute(x: Ident, y: Ident) = TPrefix(control,idents.map(n => if (n == x) y else n),suffix.substitute(x,y))
}
// p | p, parallel for prime contexts.
case class TPar(lhs:Term, rhs:Term) extends Term {
    override def toString = "(" + lhs + " | " + rhs + ")"
    def freeNames(bound : Set[Ident]) = lhs.freeNames(bound) ++ rhs.freeNames(bound)
    def substitute(x: Ident, y: Ident) = TPar(lhs.substitute(x,y), rhs.substitute(x,y))
}
// nil for prime processes.
case class TNil extends Term {
    override def toString = "nil"
    def freeNames(bound : Set[Ident]) = Set()
    def substitute(x: Ident, y: Ident) = this
}
// (v ident) term, name restriction for prime contexts
case class TNew(ident:Ident, body:Term) extends Term {
    override def toString = "(ν" + ident + ")" + body

    def freeNames(bound : Set[Ident]) = body.freeNames(bound + ident)
    def substitute(x: Ident, y: Ident) = if(x == ident) {
        // Need to stop substituting x, we've hit a binder for it.
        TNew(ident,body)
    } else {
        TNew(ident,body.substitute(x,y))
    }

}
// $n, hole with index n
case class THole(index:Int) extends Term {
    override def toString = "$" + index
    def freeNames(bound : Set[Ident]) = Set()
    def substitute(x: Ident, y: Ident) = this
}

class BigraphTranslator {
    var nodes : Set[Node] = Set()
    var nodeId = 0
    var ctrl : Map[Node,Control] = Map()
    var prnt : Map[Place,Place] = Map()
    var outerWidth = 1
    var innerWidth = 0

    def translate(t : Term, parent : Place) : Unit = t match {
        case TWPar(l,r) => {
            translate(l,parent)
            outerWidth = outerWidth + 1
            translate(r,new Region(outerWidth))
        }
        case TZero() => ()
        case TPrefix(c,i,suff) => {
            val n = new Node(nodeId)
            nodeId = nodeId + 1
            nodes += n
            val cn = new Control(c.toString)
            ctrl += n -> cn
            prnt += n -> parent

            translate(suff,n)
        }
        case THole(i) => {
            if(innerWidth < i+1) {
                innerWidth = i+1
            }

            prnt += (new Hole(i)) -> parent
        }
        case TPar(l,r) => {
            translate(l,parent)
            translate(r,parent)
        }
        case x => ()
    }

    def toBigraph(t : Term) : Bigraph = {
        translate(t.normalise(t),new Region(1))

        val b = new Bigraph(nodes,Set(),ctrl,prnt,Map(),new Face(innerWidth,Set()), new Face(outerWidth,Set()))

        println(b)

        b
    }
    
}


object MetaCalcParser extends StandardTokenParsers {
    lexical.delimiters ++= List(".",",","$","[","]","(",")","||","|","|->","(ν")
    lexical.reserved ++= List("nil")

    lazy val hole = "$" ~> numericLit ^^ { b => THole(b.toInt) }
    lazy val nil = "nil" ^^^ { TNil() }
    lazy val wnil = numericLit ^^ { case "0" => TZero()
                                    case n => TPrefix(new Ident(n),List(),TNil()) }
    lazy val ctrl = (ident ~ ("[" ~> nameList <~ "]")) ^^ { case i ~ n => (new Ident(i), n) } | (ident ^^ { s => (new Ident(s),List()) })

    lazy val prefix : Parser[Term] = ctrl ~ ("." ~> ("(" ~> expr <~ ")")) ^^ { case (c,n) ~ s => TPrefix(c,n,s) }  | 
        ctrl ~ ("." ~> prefix) ^^ {
            case (c,n) ~ s => TPrefix(c,n,s)
        } | ctrl ^^ { case (c,n) => TPrefix(c,n,TNil()) } | nil | hole | "(" ~> nu <~ ")" | nu 

    lazy val nameList : Parser[List[Ident]] = ident ~ ("," ~> nameList) ^^ { case i ~ n => (new Ident(i)) :: n } | ident ^^ ( i => List(new Ident(i)) )

    lazy val nu = ("(ν" ~> ident <~ ")") ~ expr ^^ { case i ~ e => TNew(new Ident(i), e) }

    lazy val par = terminal ~ ("|" ~> expr) ^^ { case e1 ~ e2 => TPar(e1,e2) }

    lazy val terminal = hole | nil | wnil | prefix | nu 

    lazy val expr : Parser[Term] = par | terminal | "(" ~> expr <~ ")"

    lazy val link = "[" ~> (ident ~ ("|->" ~> ident)) <~ "]" ^^ { case i1 ~ i2 => TLink(new Ident(i1), new Ident(i2)) }

    lazy val wterm : Parser[Term] = link | expr

    lazy val wpar = wterm ~ ("||" ~> wexpr) ^^ { case e1 ~ e2 => TWPar(e1,e2) }

    lazy val wnu = ("(ν" ~> ident <~ ")") ~ wexpr ^^ { case i ~ e => TNew(new Ident(i), e) }

    lazy val wexpr : Parser[Term] = wpar | wterm | wnu | "(" ~> wexpr <~ ")"

    def parse(s:String) = {
        val tokens = new lexical.Scanner(s)
        phrase(wexpr)(tokens)
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
            case Success(tree, _) => {println(tree); println("NF: " + tree.normalise(tree)); true}
            case e: NoSuccess => {
                Console.err.println(e);
                false
            }
        }
    }

    def toBigraph(s:String) : Bigraph = {
        val t = apply(s)
        val b = new BigraphTranslator()
        b.toBigraph(t)
    }
}

