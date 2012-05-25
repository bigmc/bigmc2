package org.bigraph.bigmc.matcher

import org.bigraph.bigmc._

import scala.collection.immutable.Map
import scala.collection.immutable.Set

class NoMatch() extends Exception {

}

class Matcher (B : Bigraph, redex : Bigraph) {
    /* Given a node of the redex, which nodes in B match? */
    def candidates(n:Node) : Set[Node] = {
        B.V.filter(v => B.ctrl(v) == redex.ctrl(n))
    }

    /* Attempt to take a place graph match and extend it to include
       a link match, or otherwise have it fail. */
    def linkMatch(link : Map[Link,Link], ma : Set[Match]) : Set[Match] = {
        var matches = ma

        //println("LinkMatchPrePre: " + matches + " link: " + link)

        if(link.size == 0) return ma
        
        val inner = link.head._1
        val outer = link.head._2

        println("LinkMatchPre: " + matches)
        
        matches = (for(m <- matches) yield {

            // We have already matched this before. Subsequent matches should still agree.
            if(m.linkMap contains outer) {
                inner match {
                    case p : Port => {
                        val other : Port = m.mapping(p.node) match {
                            case x : Node => new Port(x,p.id)
                            case _ => throw new IllegalArgumentException("Invalid port: " + p)
                        }

                        if(!(B.link contains other) || B.link(other) != m.linkMap(outer)) {
                            // Invalid match.  Discard it.
                            return Set()
                        } else {
                            // This match is still fine.  Preserve it.
                            return Set(m)
                        }
                    }
                    case n : Name => {
                        // I simply don't know what to do here right now.  Throw exception.
                        throw new IllegalArgumentException("case n : Name not implemented in linkMatch")
                    }
                }
            } else {
                // We have not seen this before.  Bind it all possible ways.
                outer match {
                    case e : Edge => {
                        for(f <- B.E) yield {
                            val nm = m.dup
                            nm.addLink(e,f)
                            nm
                        }
                    }
                    case n : Name => {
                        for(bo <- B.outer.names) yield {
                            val nm = m.dup
                            nm.addLink(n,bo)
                            nm
                        }
                    }
                }
            }
        }).toSeq.flatten.toSet

        println("LinkMatch: " + matches)

        linkMatch(link.tail, matches)
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

            var cand = Match.candFold(kmap.tail, candX, false)
            
            if(holes.size == 1) {
                val hole = holes.head

                cand = for(c <- cand) yield {
                    // Find the set of nodes in haystack not covered by this match, push them into the hole.
                    val cmp = haystack -- c.matchedPlaces

                    val cmpFlat = cmp ++ cmp.map(x => B.descendants(x)).flatten

                    c.addMapping(hole,new Parameter(cmpFlat))
                    c
                }
            }

            if(cand.size == 0) {
                m.failure
                return Set() ++ altMatches
            }

            val cleanCand = if(needle.forall(x => redex.prnt(x).isRegion)) {
                cand
            } else {
                cand.filter(a => {
                    val k = haystack subsetOf a.matchedPlaces
                    k
                })
            }

            return cleanCand ++ altMatches
        }
        
        m.failure
        Set() ++ altMatches
    }

    def all : Set[Match] = {
        val places = B.regions.toSet
        val red = redex.regions.toSet

        var kmap : Map[Place,Set[Match]] = Map()

        val m = for(n <- red; h <- places) yield {
            if(kmap contains n) {
                kmap = kmap + (n -> (find(Set(h),Set(n),new Match(B,redex)) ++ kmap(n)))
            } else {
                kmap = kmap + (n -> find(Set(h),Set(n),new Match(B,redex)))
            }
        }

 
        if(kmap.size == 0) {
            return Set()
        }

        val candX : Set[Match] = kmap.head._2

        if(kmap.size == 1) return linkMatch(redex.link, candX)

        if(redex.link.size > 0) {
            println("REDEX HAS LINKS! " + redex.link)
        } else {
        }

        var cand = Match.candFold(kmap.tail, candX, true)

        val lMatch = linkMatch(redex.link, cand)

        lMatch
    }
}


