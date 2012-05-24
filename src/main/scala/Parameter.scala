package org.bigraph.bigmc

class Parameter(val contents : Set[Place]) extends Place {
	override def toString = "#[" + contents + "]"
	override def isRegion = false
	override def isHole = false

    override def equals(other : Any) : Boolean = other match {
        case _ => false
    }
}



