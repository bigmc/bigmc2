package org.bigraph.bigmc.matcher

import org.bigraph.bigmc._

class Match (val B : Bigraph, 
             val redex : Bigraph) {

    var successful = false
    var failed = false
    var mapping : Map[Place,Place] = Map()
    var root : Place = null

    def success = successful = true
    def failure = {
        successful = false
        failed = true
    }

    def addMapping (p : Place, q : Place) = mapping += p -> q

    override def toString = 
        "Match[\n" +
        "  mapping: " + mapping + "\n" +
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

    def isCompatible(m : Match) : Boolean = {
        if(m.root != root) return false

        // Two keys aren't mapped to different things.
        if(!mapping.forall(k => if(m.mapping contains k._1) m.mapping(k._1) == k._2 else true)) return false


        // The same rhs isn't used twice for different things.
        mapping.forall(k => !m.mapping.exists(j => k._2 == j._2 && k._1 != j._1))
    }

    def merge(m : Match) : Match = {
        val k = new Match(B,redex)
        k.mapping = mapping ++ m.mapping
        k.root = root
        k.successful = successful
        k.failed = failed

        k
    }

    def matchedPlaces : Set[Place] = Set() ++ mapping.values
}

