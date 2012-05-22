package org.bigraph.bigmc.matcher

import org.bigraph.bigmc._

class Match (val B : Bigraph, 
             val redex : Bigraph, 
             val remaining : List[Place],
             val context : Set[Node],
             val params : Map[Place,Set[Place]],
             val root : Place) {

    val candidates : Set[Node] = B.V -- context

    val complete = remaining.size == 0

    def next : Set[Match] = {
        if(remaining.size == 0) return Set(this)

        // Next thing to match
        val n = remaining.head

        Set()
    }

    override def toString = 
        "Match[\n" +
        "  remaining: " + remaining + "\n" +
        "  context: " + context + "\n" +
        "  params: " + params + "\n" +
        "  root: " + root + "\n"
        "]"
}

