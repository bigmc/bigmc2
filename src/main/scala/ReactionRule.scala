package org.bigraph.bigmc

import org.bigraph.bigmc.matcher._

import scala.collection.immutable.Set

class ReactionRule(val redex : Bigraph, react : Bigraph) {
    override def toString = redex.toNiceString + " -> " + reactum.toNiceString

    val reactum : Bigraph = 
        new Bigraph(react.V, 
                    react.E, 
                    react.ctrl, 
                    react.prnt, 
                    react.link, 
                    new Face(react.inner.width, redex.inner.names ++ react.inner.names),
                    new Face(react.outer.width, redex.outer.names ++ react.outer.names))

    def apply(agent : Bigraph) : Set[Bigraph] = {
        val matches = new Matcher(agent,redex)
        val ma = matches.all
        
        println("ReactionRule apply: " + ma)
        for(m <- ma) yield {
            agent.apply(m,reactum)
        }
    }
}



