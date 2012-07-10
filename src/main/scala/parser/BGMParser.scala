package org.bigraph.bigmc.parser

import org.bigraph.bigmc._
import scala.util.parsing.combinator._

import scala.collection.immutable.Map
import scala.collection.immutable.Set

object BGMParser extends RegexParsers {
    def exp = "[^;]".r
    def ident = "[^ \t\n\r;]".r
    def ws = "[ \t]+".r
    
    def EOL : Parser[String] = ws ~ ";" ~ ws ^^^ ""

    def stmt : Parser[String] = "%active" ~> (ws ~> ident ~ (ws ~> ":" ~> ws ~> ident)) ^^^ { "active" } |
                                "%passive" ~> (ws ~> ident ~ (ws ~> ":" ~> ws ~> ident)) ^^^  { "passive" } 

    def stmtList : Parser[List[String]] = stmt ~ (EOL ~> stmtList) ^^ { case x~xs => x :: xs } |
                                          stmt <~ EOL ^^ { x => x :: Nil }

    def parse(s:String) = parseAll(stmtList, s) match {
        case Success(res, _) => res
        case e => throw new Exception(e.toString)
    }
}

