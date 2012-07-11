package org.bigraph.bigmc.parser

import org.bigraph.bigmc._
import scala.util.parsing.combinator._

import scala.collection.immutable.Map
import scala.collection.immutable.Set

import java.io._

abstract class BGMTerm { }
case class BGMActive(s : String, arity : Int) extends BGMTerm
case class BGMPassive(s : String, arity : Int) extends BGMTerm
case class BGMRule(n : String, redex : String, reactum : String) extends BGMTerm
case class BGMAgent(s : String) extends BGMTerm
case class BGMProp(s : String, p : String) extends BGMTerm
case class BGMNil extends BGMTerm

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

