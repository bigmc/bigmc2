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


class Ident(id:String) {
    override def toString:String = id
}

sealed abstract class Term {
    def normalise(t : Term) : Term = t match {
        case TWPar(TZero(),l) => TWPar(normalise(l),TZero())
        case TWPar(l,TWPar(m,n)) => TWPar(TWPar(normalise(l),normalise(m)),normalise(n))
        case TWPar(l,r) => TWPar(normalise(l),normalise(r))
        case TPar(TNil(),l) => normalise(l)
        case TPar(l,TNil()) => normalise(l)
        case TPar(l,TPar(m,n)) => TPar(TPar(normalise(l),normalise(m)),normalise(n))
        case TPar(l,r) => TPar(normalise(l),normalise(r))
        case TPrefix(c,l,s) => TPrefix(c,l,normalise(s))
        case x => x
    }
}

// lhs || rhs
case class TWPar(lhs:Term, rhs:Term) extends Term {
    override def toString = "(" + lhs + " || " + rhs + ")"
}
// 0, the nil term for wide terms.
case class TZero extends Term {
    override def toString = "0" 
}
// (v ident) term, name restriction for wide contexts
case class TWideNew(ident:Ident, body:Term) extends Term {
    override def toString = "(ν" + ident + ")" + body
}
// [identL |-> identR], links.
case class TLink(identL:Ident, identR:Ident) extends Term { 
    override def toString = "[" + identL + " |-> " + identR + "]"
}
// c[idents].term, prefixing
case class TPrefix(control: Ident, idents: List[Ident], suffix : Term) extends Term {
    override def toString = control + "[" + idents.mkString(",") + "]." + suffix
}
// p | p, parallel for prime contexts.
case class TPar(lhs:Term, rhs:Term) extends Term {
    override def toString = "(" + lhs + " | " + rhs + ")"
}
// nil for prime processes.
case class TNil extends Term {
    override def toString = "nil"
}
// (v ident) term, name restriction for prime contexts
case class TNew(ident:Ident, body:Term) extends Term {
    override def toString = "(ν" + ident + ")" + body
}
// $n, hole with index n
case class THole(index:Int) extends Term {
    override def toString = "$" + index
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
        } | ctrl ^^ { case (c,n) => TPrefix(c,n,TNil()) } | nil | hole

    lazy val nameList : Parser[List[Ident]] = ident ~ ("," ~> nameList) ^^ { case i ~ n => (new Ident(i)) :: n } | ident ^^ ( i => List(new Ident(i)) )

    lazy val nu = ("(ν" ~> ident <~ ")") ~ expr ^^ { case i ~ e => TNew(new Ident(i), e) }

    lazy val par = terminal ~ ("|" ~> expr) ^^ { case e1 ~ e2 => TPar(e1,e2) }

    lazy val terminal = hole | nil | wnil | prefix | nu 

    lazy val expr : Parser[Term] = par | terminal | "(" ~> expr <~ ")"

    lazy val link = "[" ~> (ident ~ ("|->" ~> ident)) <~ "]" ^^ { case i1 ~ i2 => TLink(new Ident(i1), new Ident(i2)) }

    lazy val wterm : Parser[Term] = link | expr

    lazy val wpar = wterm ~ ("||" ~> wexpr) ^^ { case e1 ~ e2 => TWPar(e1,e2) }

    lazy val wexpr : Parser[Term] = wpar | wterm | "(" ~> wexpr <~ ")"

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

