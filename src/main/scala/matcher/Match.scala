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
}

