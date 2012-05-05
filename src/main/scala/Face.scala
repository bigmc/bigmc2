package org.bigraph.bigmc

class Face(val width : Int, val names : Set[Name]) extends Link {

	override def toString = "<" + width + ", {" + names + "}>"
}


