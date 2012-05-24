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
            println("MATCH FAILURE: " + n + " against " + m)
            ma.failure
            ma
        }
    }

    def matchProduct (prefixes : Set[Match], suffixes : Set[Match]) : Set[Match] = {
        for (p <- prefixes; s <- suffixes; if(p.isCompatible(s))) yield p.merge(s)
    }

    def find(haystack : Set[Place], needle : Set[Place], m : Match) : Set[Match] = {
        //println(" * find:\nRedex: " + redex.toNiceString + "\nAgent: " + B.toNiceString + "\nNeedle: " + needle + "\nHaystack: " + haystack + "\n" + m + "\n")

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
            val c : Set[Place] = haystack.map(x => B.descendants(x)).flatten.toSet
            m.addMapping(needle.head, new Parameter(c))
            m.success
            //println("SINGLE HOLE: " + needle.head + " = " + c + "\n" + m)
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
            //println("kmap cand: B: " + B.toNiceString + "\n Redex: " + redex.toNiceString + "\nReason: needle.size > 1 && haystack <= 1\snNeedle: " + needle + "\nHaystack: " + haystack + "\n" + m + "\n")
            //println("Par-redex: " + redex.toNiceString + " / " + B.toNiceString + "\n" + needle + "\n" + haystack + "\n")
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
                //println("0-kmap: " + redex.toNiceString + " / " + B.toNiceString + "\n")
                return Set() ++ altMatches
            }

            val candX : Set[Match] = kmap.head._2

            println("START: " + redex.toNiceString + " / " + B.toNiceString + "\n")
            println("PRE-CAND: " + candX)

            def candFold (k : Map[Place,Set[Match]],res : Set[Match]) : Set[Match] = {
                println("CAND RES: " + res)
                if(k.size == 0) {
                    res
                } else {
                    println("candFold: " + res + " with " + k.head._2)
                    candFold(k.tail, matchProduct(res,k.head._2))
                }
            }


            var cand = candFold(kmap.tail, candX)
            
            println("CAND: " + cand)

            //for(m <- kmap.tail) yield {
            //    cand = matchProduct(cand,m._2)
            //    println("CAND: " + cand + " kmap: " + kmap)
                //if(cand.size == 0) {
                //    println("0-cand: " + redex.toNiceString + " / " + B.toNiceString + "\n" + m)
                //    return Set() ++ altMatches
                //}
            //}

            if(holes.size == 1) {
                val hole = holes.head

                println("HOLES: " + holes + " HAYSTACK: " + haystack)
                cand = for(c <- cand) yield {
                    // Find the set of nodes in haystack not covered by this match, push them into the hole.
                    val cmp = haystack -- c.matchedPlaces
                    c.addMapping(hole,new Parameter(cmp))
                    c
                }
            }

            if(cand.size == 0) {
                m.failure
                println("0-cand2: " + redex.toNiceString + " / " + B.toNiceString + "\n")
                return Set() ++ altMatches
            }



            val cleanCand = if(haystack.forall(x => B.prnt(x).isRegion)) {
                println("NOT Filtering: " + redex.toNiceString + " / " + B.toNiceString + "\n")
                cand
            } else {
                //println("FILTER: kmap cand: B:\n" + B.toNiceString + "\n Redex: " + redex.toNiceString + "\nSize: " + cand.size + "\n")
                println("Filtering: " + redex.toNiceString + " / " + B.toNiceString + "\n")
                cand.filter(a => {

                    val k = haystack subsetOf a.matchedPlaces
                    //println("CAND Filter (" + k + ") " + a + "\nHaystack: " + haystack + "\nMatched Places: " + a.matchedPlaces + "\n")
                    k
                })
            }

            //println("kmap cand: B: " + B.toNiceString + "\n Redex: " + redex.toNiceString + "\nSize: " + cleanCand.size + "\n")

            return cleanCand ++ altMatches
        }
        
        if(m.root == null) {
            println("Null root: " + redex.toNiceString + " / " + B.toNiceString + "\n")
            //println("kmap cand: B: " + B.toNiceString + "\n Redex: " + redex.toNiceString + "\nReason: null root in: " + m + "\n")
        } else {
            println("Not Null root: " + redex.toNiceString + " / " + B.toNiceString + "\n")
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
   
        m.toSet
    }
}


