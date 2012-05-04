package org.bigraph.bigmc

/** The core representation of a static bigraph. */
class Bigraph(val V : Set[Node],
              val E : Set[Edge], 
              val ctrl : Map[Node,Control], 
              val prnt : Map[Place,Place], 
              val link : Map[Link,Link],
              val m : Int,
              val X : Set[Link],
              val n : Int,
              val Y : Set[Link]
              ) {

    override def toString = "("+V+","+E+","+ctrl+","+prnt+","+link+"+) : <"+m+","+X+"> -> <"+n+","+Y+">"

    def compose(other : Bigraph) : Bigraph = {
        if(other.n != m) {
            throw new IllegalArgumentException("Incompatible interface widths in composition")
        }

        if(Y != other.X) {
            throw new IllegalArgumentException("Incompatible names in interface composition")
        }

        val nV = V ++ other.V
        val nE = E ++ other.E
        val nctrl = ctrl ++ other.ctrl
        var nprnt1 = prnt
        val nprnt2 = for((a,b) <- other.prnt) yield {
            if(b.isRegion) {
                nprnt1 = nprnt1 - b
                a -> prnt(b)
            } else {
                a -> b
            }
        }
        val nprnt = nprnt1 ++ nprnt2
        val nlink = link ++ other.link

        return new Bigraph(nV,nE,nctrl,nprnt,nlink,other.m,other.X,n,Y)
    }

    
}


