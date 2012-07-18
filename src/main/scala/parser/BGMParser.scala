package org.bigraph.bigmc.parser

import org.bigraph.bigmc._
import scala.util.parsing.combinator._

import scala.collection.immutable.Map
import scala.collection.immutable.Set

import java.io._

abstract class BGMTerm {

}
case class BGMActive(s : String, arity : Int) extends BGMTerm
case class BGMPassive(s : String, arity : Int) extends BGMTerm
case class BGMRule(n : String, redex : String, reactum : String) extends BGMTerm
case class BGMAgent(s : String) extends BGMTerm
case class BGMProp(s : String, p : String) extends BGMTerm
case class BGMNil extends BGMTerm

object BGMTerm {
    def toReactiveSystem(t : List[BGMTerm]) : BigraphicalReactiveSystem = {
        val activeL : List[BGMActive] = t.filter(_ match {
            case BGMActive(_,_) => true
            case _ => false
        }).map(_.asInstanceOf[BGMActive])
        
        val active = activeL.map(_.s)

        val passiveL : List[BGMPassive] = t.filter(_ match {
            case BGMPassive(_,_) => true
            case _ => false
        }).map(_.asInstanceOf[BGMPassive])
        
        val passive = passiveL.map(x => new Control(x.s,false))

        val agentL = t.filter(_ match {
            case BGMAgent(_) => true
            case _ => false
        }).head.asInstanceOf[BGMAgent]

        val agent = MetaCalcParser.toBigraph(agentL.s, Set() ++ active)

        val rules : List[ReactionRule] = t.filter(_ match {
            case BGMRule(_,_,_) => true
            case _ => false
        }).map(rr => {
            val rrp = rr.asInstanceOf[BGMRule]
            val redex = MetaCalcParser.toBigraph(rrp.redex, Set() ++ active)
            val reactum = MetaCalcParser.toBigraph(rrp.reactum, Set() ++ active)
            new ReactionRule(redex,reactum)
        })


        //agent : Bigraph, signature : Set[Control], rules : Set[ReactionRule], sorting : Sorting

        new BigraphicalReactiveSystem(agent, Set() ++ active.map(new Control(_,true)) ++ passive, Set()++rules, null)
    }
}

object BGMParser extends RegexParsers {
    def exp = "[^;]*".r
    def ident = "[^ \t\n\r;]+".r
    def ws = "[ \t]*".r
    
    def EOL : Parser[String] = ws ~ ";" ~ ws ^^^ ""

    def stmt : Parser[BGMTerm] = "%active" ~> (ws ~> ident ~ (ws ~> ":" ~> ws ~> ident)) ^^ { case i~a => BGMActive(i,a.toInt) } |
                                "%passive" ~> (ws ~> ident ~ (ws ~> ":" ~> ws ~> ident)) ^^  { case i~a => BGMPassive(i,a.toInt) } | 
                                "%rule" ~> (ws ~> ident ~ (ws ~> exp)) ^^  { case i~s => {
                                    val rr = s.split("->")
                                    BGMRule(i,rr(0),rr(1))
                                } } | 
                                "%agent" ~> (ws ~> exp) ^^ { x => BGMAgent(x) } | 
                                "%property" ~> (ws ~> ident ~ (ws ~> exp)) ^^  { case i~p => BGMProp(i,p) } |
                                "%check" ^^^ { BGMNil() }

    def stmtList : Parser[List[BGMTerm]] = stmt ~ (EOL ~> stmtList) ^^ { case x~xs => x :: xs } |
                                          stmt <~ EOL ^^ { x => x :: Nil }

    def parse(s:File) : List[BGMTerm] = parseAll(stmtList, io.Source.fromFile(s).mkString) match {
        case Success(res, _) => res
        case e => throw new Exception(e.toString)
    }
}

