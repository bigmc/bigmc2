package org.bigraph.bigmc.matcher

import org.bigraph.bigmc._

class NoMatch() extends Exception {

}

class Matcher (B : Bigraph, redex : Bigraph) {
    /* Given a node of the redex, which nodes in B match? */
    def candidates(n:Node) : Set[Node] = {
        B.V.filter(v => B.ctrl(v) == redex.ctrl(n))
    }

    def placeMatch(agent : Place, pattern : Place, ma : Match) : Match = (agent,pattern) match {
        case (n : Node, m : Node) => {
            if (B.ctrl(n) == redex.ctrl(m)) {
                ma.addMapping(m,n)
                if(ma.root == null) ma.root = n
            } else {
                ma.failure
            }

            ma
        }
        case (n : Node, m : Hole) => {
            ma.addMapping(m,n)
            if(ma.root == null) ma.root = n
            ma
        }
        case (n : Region, m : Hole) => {
            ma.addMapping(m,n)
            if(ma.root == null) ma.root = n
            ma
        }
        case (n : Region, m : Node) => {
            ma.failure
            ma
        }
        case (n, m : Region) => {
            ma.addMapping(m,n)
            if(ma.root == null) ma.root = n
            ma
        }
        case _ => {
            ma.failure
            ma
        }
    }

    def matchProduct (prefixes : Set[Match], suffixes : Set[Match]) : Set[Match] = {
        for (p <- prefixes; s <- suffixes; if(p.isCompatible(s))) yield p.merge(s)
    }

    def find(haystack : Set[Place], needle : Set[Place], m : Match) : Set[Match] = {
        // Nothing to match.  We're done!
        if(needle.size == 0 && haystack.size == 0) {
            m.success
            return Set(m)
        }

        // We need to continue trying to match the same redex if this is unrooted.
        val altMatches = if(m.root == null) {
            haystack.map(h => {
                find(B.children(h),needle,m.dup)
            }).flatten
        } else {
            Set()
        }

        // Single prefix match
        if(needle.size == 1) {
            val n = needle.head

            val matches : Set[Match] = haystack.map(h => {
                val pm = placeMatch(h,n,m.dup)
                if(!pm.failed) {
                    find(B.children(h),redex.children(n),pm)
                } else {
                    Set[Match]()
                }
            }).flatten

            return matches ++ altMatches 
        }

        // Matching a parallel redex against a single prefix
        if(needle.size > 1 && haystack.size <= 1) {
            m.failure
            return Set() ++ altMatches
        }

        // Matching parallel elements at the top level
        if(needle.size > 1 && m.root == null) {
            return Set() ++ altMatches
        }

        // Matching parallel elements nested under a prefix.
        if(needle.size > 1 && m.root != null) {
            var kmap : Map[Place,Set[Match]] = Map()

            for(n <- needle; h <- haystack) yield {
                if(kmap contains n) {
                    kmap = kmap + (n -> (find(Set(h),Set(n),m.dup) ++ kmap(n)))
                } else {
                    kmap = kmap + (n -> find(Set(h),Set(n),m.dup))
                }
            }

            println("Kmap: " + kmap)

            var cand : Set[Match] = kmap.head._2

            for(m <- kmap.tail) yield {
                cand = matchProduct(cand,m._2)
                if(cand.size == 0) {
                    return Set() ++ altMatches
                }
            }

            if(cand.size == 0) {
                m.failure
                return Set() ++ altMatches
            }

            println("kmap cand: " + B + "\n" + redex + "\n" + cand)

            return cand ++ altMatches
        }

        Set() ++ altMatches
    }

    def all : Set[Match] = {
        val places = B.regions

        val m = (for(p <- places) yield {
            val ma = new Match(B,redex)
            find(Set(p),redex.regions.toSet,ma)
        }).flatten
   
        m.toSet
    }
}


