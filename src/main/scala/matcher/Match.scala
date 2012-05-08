package org.bigraph.bigmc.matcher

import org.bigraph.bigmc._

/** An occurence of some bigraph B in a context C with parameters D **/
class Match (val C : Bigraph, val B : Bigraph, val D : Bigraph) {
    override def toString = 
        "Match[" + C + " o " + B + " o " + D + "]"
}

