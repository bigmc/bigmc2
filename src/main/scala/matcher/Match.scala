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

    override def toString = 
        "Match[\n" +
        "  mapping: " + mapping + "\n" +
        "  linking: " + linkMap + "\n" +
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
