package org.bigraph.bigmc;

class Bigraph(V : Set[Node],
              E : Set[Edge], 
              ctrl : Map[Node,Control], 
              prnt : Map[Place,Place], 
              link : Map[Link,Link]) {

    def toString = "("+V+",E,ctrl,prnt,link) : <m,X> -> <n,Y>"
}


