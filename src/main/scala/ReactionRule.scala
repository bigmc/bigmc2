package org.bigraph.bigmc

class ReactionRule(val redex : Bigraph, val reactum : Bigraph) {
	override def toString = redex.toNiceString + " -> " + reactum.toNiceString
}



