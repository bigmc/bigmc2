package org.bigraph.bigmc

class Control(val name : String) {
    override def equals(other : Any) : Boolean = other match {
        case that : Control => that.name == name
        case _ => false
    }

	override def toString = name 
}


