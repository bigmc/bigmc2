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
        
        ma.map(m => {
            val B = m.toContext
    
            val C = B._1
            val D = B._3

            if(C.isActiveContext)
                Some(agent.apply(m,C,reactum,D))
            else None
        }).filter(x => !x.isEmpty).map(_.get)
    }
}



