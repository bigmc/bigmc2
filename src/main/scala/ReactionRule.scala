package org.bigraph.bigmc

import org.bigraph.bigmc.matcher._

import scala.collection.immutable.Set

class ReactionRule(val redex : Bigraph, val reactum : Bigraph) {
    override def toString = redex.toNiceString + " -> " + reactum.toNiceString

    def apply(agent : Bigraph) : Set[Bigraph] = {
        val matches = new Matcher(agent,redex)
        val ma = matches.all
        
        println("ReactionRule apply: " + ma)
        for(m <- ma) yield {
            agent.apply(m,reactum)
        }
    }
}



