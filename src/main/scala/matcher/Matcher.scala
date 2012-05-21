package org.bigraph.bigmc.matcher

import org.bigraph.bigmc._

class Matcher (B : Bigraph, R : Bigraph) {
    /* Given a node of R, which nodes in B match? */
    def candidates(n:Node) : Set[Node] = {
        B.V.filter(v => B.ctrl(v) == R.ctrl(n))
    }

    def all : Set[Match] = Set()
}


