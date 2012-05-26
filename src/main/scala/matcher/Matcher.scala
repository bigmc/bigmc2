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

    def matchedPort(m : Match, p : Link) : Port = {
        p match {
            case p : Port => {
                val n : Node = m.mapping(p.node) match {
                    case n : Node => n
                    case _ => throw new IllegalArgumentException("Non-node in port")
                }
                new Port(n, p.id)
            }
            case _ => throw new IllegalArgumentException("Non-port passed to matchedPort")
        }
    }

    def linkMatchOnce(m : Match) : Set[Match] = {
        val places = m.matchedPlaces

        val ports = redex.link.filter(l => l._1 match {
            case p : Port => true
            case _ => false
        })

        val innerNames = redex.link.filter(l => l._1 match {
            case n : Name => true
            case _ => false
        })


        if(ports.size == 0 && innerNames.size == 0) {
            return Set(m)
        }
        
        for(pa <- ports) yield {
            val p = pa._1
            val a = pa._2

            val op = matchedPort(m,p)
            // For a given port (p) and assignment (a), find the
            // other port (op) and ensure it is assigned to something
            // compatible according to m.linkMap, otherwise assign it.

            // Fail if we can't find this port in the link map for B
            if(!(B.link contains op)) {
                m.failure
                println("Couldn't find: " + op)
                return Set()
            }

            val oa = B.link(op)

            // This is already matched in the link map, so oa and linkMap(a) must agree.
            if(m.linkMap contains a) {
                if(oa != m.linkMap(a)) {
                    m.failure
                    return Set()
                }
            } else {
                // Doesn't exist in the link map, so we add it.
                m.addLink(a,oa)
            }
        }

        val params = m.parameters

        for(na <- innerNames) yield {
            val n = na._1
            val a = na._2

            val portset = (for(p <- params) yield {
                B.link.filter(x => x._1 match {
                    case x : Port => {
                        x.node == p
                    }
                    case _ => false
                }).map(x => x._1)
            }).toSet.flatten

            if(portset.size == 0) {
                m.failure
                return Set()
            }

            var matchedName = false
            for(p <- portset) yield {
                if(B.link(p) == m.linkMap(redex.link(n))) {
                    // Success
                    matchedName = true
                } else {
                    ()
                }
            }

            if(matchedName) {
                ()
            } else {
                return Set()
            }
        }

        Set(m)
    }

    /* Attempt to take a place graph match and extend it to include
       a link match, or otherwise have it fail. */
    def linkMatch(ma : Set[Match]) : Set[Match] = {
        var matches = ma
        
        (for(m <- ma) yield {
            linkMatchOnce(m)
        }).flatten.toSet
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

        if(kmap.size == 1) return linkMatch(candX)

        var cand = Match.candFold(kmap.tail, candX, true)

        val lMatch = linkMatch(cand)

        lMatch
    }
}


