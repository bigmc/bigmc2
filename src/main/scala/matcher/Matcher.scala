package org.bigraph.bigmc.matcher

import org.bigraph.bigmc._

class NoMatch() extends Exception {

}

class Matcher (B : Bigraph, redex : Bigraph) {
    /* Given a node of the redex, which nodes in B match? */
    def candidates(n:Node) : Set[Node] = {
        B.V.filter(v => B.ctrl(v) == redex.ctrl(n))
    }

    def matchOnce(n:Node,r:Node) : Boolean = {
        if(B.ctrl(n) != redex.ctrl(r)) {
            return false
        }
        
        val nc = B.children(n)
        val rc = redex.children(r)

        if(nc.size == 0 && rc.size == 0) {
            true
        } else {
            // For every child c of the redex there must exist a child n of nc for which matchOnce(n,c) */
            rc.forall(c => {
                nc.exists(n => matchOnce(n,c))
            })
        }
    }

    def all : Set[Match] = {
        val roots = redex.regions

        val b = roots.forall(r => 
            redex.children(r).forall(c => {
                    B.V.exists(v => {
                        val m = matchOnce(v,c)
                        println("match " + v + " against " + c + ": " + m)
                        m
                    })
                }
            )
        )

        println("Matches: " + b + " for " + B + " with " + redex)

        Set()
    }
}


