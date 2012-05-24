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
        case (n,m) => {
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

        // Single hole
        if(needle.size == 1 && needle.head.isHole) {
            val c : Set[Place] = haystack ++ haystack.map(x => B.descendants(x)).flatten.toSet
            m.addMapping(needle.head, new Parameter(c))
            m.success
            return Set(m)
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
        if(needle.filter(x => !x.isHole).size > 1 && haystack.size <= 1) {
            m.failure
            return Set() ++ altMatches
        }

        // Matching parallel elements nested under a prefix.
        // We disallow redexes like x.($0 | $1), so we need not worry that these are all holes.
        if(needle.size > 1 && m.root != null) {
            var kmap : Map[Place,Set[Match]] = Map()

            for(n <- needle.filter(x => !x.isHole); h <- haystack) yield {
                if(kmap contains n) {
                    kmap = kmap + (n -> (find(Set(h),Set(n),m.dup) ++ kmap(n)))
                } else {
                    kmap = kmap + (n -> find(Set(h),Set(n),m.dup))
                }
            }

            val holes = needle.filter(x => x.isHole)

            if(holes.size > 1) {
                throw new IllegalArgumentException("Term contains parallel holes: " + holes)
            }


            if(kmap.size == 0) {
                return Set() ++ altMatches
            }

            val candX : Set[Match] = kmap.head._2

            def candFold (k : Map[Place,Set[Match]],res : Set[Match]) : Set[Match] = {
                if(k.size == 0) {
                    res
                } else {
                    candFold(k.tail, matchProduct(res,k.head._2))
                }
            }

            var cand = candFold(kmap.tail, candX)
            
            if(holes.size == 1) {
                val hole = holes.head

                cand = for(c <- cand) yield {
                    // Find the set of nodes in haystack not covered by this match, push them into the hole.
                    val cmp = haystack -- c.matchedPlaces

                    val cmpFlat = cmp ++ cmp.map(x => B.descendants(x)).flatten

                    c.addMapping(hole,new Parameter(cmpFlat))
                    println("Filling hole: haystack: " + haystack + " / matched: " + c.matchedPlaces + " c: " + cmpFlat)
                    c
                }
            }

            if(cand.size == 0) {
                m.failure
                return Set() ++ altMatches
            }

            val cleanCand = if(needle.forall(x => redex.prnt(x).isRegion)) {
                    println("No Filter for: " + redex.toNiceString + " / " + B.toNiceString + ":")
                cand
            } else {
                cand.filter(a => {
                    val k = haystack subsetOf a.matchedPlaces
                    println("Filter for: " + redex.toNiceString + " / " + B.toNiceString + ":")
                    println("haystack: " + haystack + " / matched: " + a.matchedPlaces + " res: " + k)
                    k
                })
            }

            return cleanCand ++ altMatches
        }
        
        m.failure
        Set() ++ altMatches
    }

    def all : Set[Match] = {
        val places = B.regions

        val m = (for(p <- places) yield {
            val ma = new Match(B,redex)
            find(Set(p),redex.regions.toSet,ma)
        }).flatten
  
        println("Matches for: " + redex.toNiceString + " / " + B.toNiceString + ":")
        println("Redex: " + redex)
        println("Agent: " + B)
        println("Matches: " + m.map(x => x.toString + " with " + x.matchedPlaces).mkString("\n") + "\n")

        m.toSet
    }
}


