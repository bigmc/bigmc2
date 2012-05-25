package org.bigraph.bigmc

trait Place {
	def toString: String
	def isRegion: Boolean
    def isHole: Boolean
    
    override def equals(other : Any) : Boolean = {
        println("Calling Place equality!")

        other match {
            case that : Region => that == this
            case that : Hole => that == this
            case that : Node => that == this
            case _ => false
        }
    }
}

