package org.bigraph.bigmc.matcher

import org.bigraph.bigmc._

import scala.collection.immutable.Map
import scala.collection.immutable.Set

class Match (val B : Bigraph, 
             val redex : Bigraph) {

    var successful = false
    var failed = false
    var mapping : Map[Place,Place] = Map()
    var root : Place = null

    var linkMap : Map[Link,Link] = Map()
    var nameMap : Map[Link,Link] = Map()

    def success = successful = true
    def failure = {
        successful = false
        failed = true
    }

    def addMapping (p : Place, q : Place) = {
        if(!p.isRegion) 
            mapping += p -> q
    }

    def addLink (l : Link, r : Link) = {
        linkMap += l -> r
    }

    /* Maps inner names of the redex to outer names in parameters */
    def addName (l : Link, r : Link) = {
        nameMap += l -> r
    }


    override def toString = 
        "Match[\n" +
        "  mapping: " + mapping + "\n" +
        "  linking: " + linkMap + "\n" +
        "  names: " + nameMap + "\n" +
        "  root: " + root + "\n" +
        "  successful: " + successful + "\n" +
        "  failed: " + failed +
        "]"

    def dup : Match = {
        val m = new Match(B,redex)
        m.successful = successful
        m.failed = failed
        m.mapping = m.mapping ++ mapping
        m.root = root
        m
    }

    def isWideCompatible(m : Match) : Boolean = {
        (matchedPlaces & m.matchedPlaces).size == 0

        // Two keys aren't mapped to different things.
 //       if(!mapping.forall(k => if(m.mapping contains k._1) (m.mapping(k._1) == k._2) else true)) return false

        // Relax the restriction to let regions disagree on the RHS.
        //mapping.forall(k => !m.matchedPlaces.exists(j => k._2 == j))
   //     true
    }

    def isCompatible(m : Match) : Boolean = {
        m.root == root && 
           mapping.forall(k => !m.mapping.exists(j => (k._2 == j._2 && k._1 != j._1))) &&
            mapping.forall(k => if(m.mapping contains k._1) m.mapping(k._1) == k._2 else true)
    }

    def merge(m : Match) : Match = {
        val k = new Match(B,redex)
        k.mapping = mapping ++ m.mapping
        k.linkMap = linkMap ++ m.linkMap
        k.root = root
        k.successful = successful
        k.failed = failed

        k
    }
    
    def matchedPlaces : Set[Place] = Set() ++ mapping.values.map(x => x match {
        case p : Parameter => p.contents
        case x => Set(x)
    }).flatten

    def getParam (id : Int) : Set[Place] = mapping.getOrElse(new Hole(id), new Parameter(Set())) match {
        case p : Parameter => p.contents
        case _ => Set()
    }

    def parameters : Set[Place] = mapping.values.map(x => x match {
        case p : Parameter => p.contents
        case _ => Set()
    }).toSet.flatten

    /** Turn a match into three bigraphs - the context, the redex, and the parameters. **/
    def toContext : (Bigraph,Bigraph,Bigraph) = {
        /*
            We basically take the original and subtract away
            everything that was matched to obtain a context,
            then we need to add the holes to the context.
        */

        val Vi : Set[Place] = Set() ++ B.V

        val Vp = Vi -- matchedPlaces

        val prnt = for(p <- B.prnt if ((Vp contains p._2) || p._2.isRegion)) yield {
            // The prnt link is entirely within the context, include it directly.
            if(Vp contains p._1) {
                p
            } else {
                // The parent is in the context, but the child is not.
                // So we have to figure out what the hole index of this should be.
                // We look for the matching node in the redex (i.e. mapping contains
                // an entry for this, mapped to something in the redex)
                val n = mapping.find(k => k._2 == p._1) match {
                    case None => throw new IllegalArgumentException("Malformed match in toContext")
                    case Some(x) => x
                }

                redex.prnt(n._1) match {
                    case r : Region => new Hole(r.id) -> B.prnt(n._2)
                    case _ => throw new IllegalArgumentException("Malformed match in toContext")
                }
            }
        }

        val ctrl = B.ctrl.filter(p => Vp contains p._1)

        val V : Set[Node] = Vp.map(x => x match { 
            case x : Node => x
            case _ => throw new IllegalArgumentException("Malformed node set in toContext")
        })

        val C = new Bigraph(V,Set(),ctrl,prnt,Map(), new Face(redex.outer.width,Set()), B.outer)

        val params = mapping.filter(x => x._1.isHole)

        println("Params: " + params)

        val dV = (for(p <- params) yield {
            p._2 match {
                case h : Parameter => h.contents
                case x => Set(x)
            }
        }).flatten.toSet

        val dprnt : Map[Place,Place] = (for(p <- params) yield {
            val hole = p._1 match {
                case h : Hole => h
                case _ => throw new IllegalArgumentException("Parameter key is invalid")
            }
            p._2 match {
                case h : Parameter => h.contents.map(x => x -> new Region(hole.id))
                case _ => throw new IllegalArgumentException("Non-parameter value found in site")
            }
        }).flatten.toMap ++ B.prnt.filter(x => dV contains x._2)


        val dVn : Set[Node] = dV.map(x => x match { 
            case x : Node => x
            case _ => throw new IllegalArgumentException("Malformed node set in toContext")
        })

        val dctrl = B.ctrl.filter(p => dV contains p._1)

        val D = new Bigraph(dVn,Set(),dctrl,dprnt,Map(),new Face(0,Set()), new Face(params.size,Set()))

        (C,redex,D)
    }
}

object Match {
    def matchProduct (prefixes : Set[Match], suffixes : Set[Match]) : Set[Match] = {
        for (p <- prefixes; s <- suffixes; if(p.isCompatible(s))) yield p.merge(s)
    }

    def matchWideProduct (prefixes : Set[Match], suffixes : Set[Match]) : Set[Match] = {
        for (p <- prefixes; s <- suffixes; if(p.isWideCompatible(s))) yield p.merge(s)
    }

    def candFold (k : Map[Place,Set[Match]],res : Set[Match], wide : Boolean) : Set[Match] = {
        if(k.size == 0) {
             //println("RES FINAL: " + res)
            res
        } else {
            //println("RES: " + res)
            if(!wide) {
                candFold(k.tail, matchProduct(res,k.head._2),wide)
            } else {
                candFold(k.tail, matchWideProduct(res,k.head._2),wide)
            }
        }
    }

}
