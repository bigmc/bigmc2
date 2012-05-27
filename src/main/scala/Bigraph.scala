package org.bigraph.bigmc

import org.bigraph.bigmc.matcher._

import scala.collection.immutable.Map
import scala.collection.immutable.Set

/** The core representation of a static bigraph. */
class Bigraph(val V : Set[Node],
              val E : Set[Edge], 
              val ctrl : Map[Node,Control], 
              val prnt : Map[Place,Place], 
              val link : Map[Link,Link],
              val inner : Face,
              val outer : Face
              ) {

    override def toString = "({"+V.mkString(",")+"},{"+E.mkString(",")+"},{"+ctrl.mkString(",")+"},{"+prnt.mkString(",")+"},{"+link.mkString(",")+"}) : "+inner+" → " + outer 

    def toMetaCalcString(p : Place) : String = {
        p match {
            case h : Hole => "$" + h.id
            case n : Node => {
                val c = children(n).toList

                val pa = link.filter(x => x._1 match {
                    case p : Port => p.node == n
                    case _ => false
                }).map(x => x._2.toString).mkString(",")
                
                val p = if(pa != "") "[" + pa + "]" else ""

                if(c.size == 0) {
                    ctrl(n) + p + ".nil"
                } else if(c.size == 1) {
                    ctrl(n) + p + "." + toMetaCalcString(c.head)
                } else {
                    ctrl(n) + p + ".(" + c.map(x => toMetaCalcString(x)).mkString(" | ") + ")"
                }
            }
            case r : Region => {
                 val c = children(r).toList

                 c.map(x => toMetaCalcString(x)).mkString(" | ") 
            }
        }
    }

    def toNiceString : String = {
        regions.map(toMetaCalcString).mkString(" || ")
    }

    def compose(other : Bigraph) : Bigraph = {
        if(other.outer.width != inner.width) {
            throw new IllegalArgumentException("Incompatible interface widths in composition")
        }

        if(other.outer.names != inner.names) {
            throw new IllegalArgumentException("Incompatible names in interface composition: " + inner + " != " + other.outer)
        }

        val nV = V ++ other.V
        val nE = E ++ other.E
        val nctrl = ctrl ++ other.ctrl
        var nprnt1 = prnt
        val nprnt2 = for((a,b) <- other.prnt) yield {
            b match {
                case r : Region => {
                    nprnt1 = nprnt1 - (new Hole(r.id))
                    a -> prnt(new Hole(r.id))
                }
                case _ => a -> b
            }
        }
        var nprnt = nprnt1 ++ nprnt2
        val nlinkf = for((x,y) <- other.link) yield {
            y match {
                case z : Name => if(link contains z) (x,link(z)) else (x,z)
                case z => (x,y)
            }
        }

        val nlinkg = link.filter (p => p._1 match {
                case z : Name => false
                case w => true
            })

        if(other.V.size == 0 && other.E.size == 0) {
            nprnt = nprnt.filter(x => !x._1.isHole)
        }
        
        return new Bigraph(nV,nE,nctrl,nprnt,nlinkf ++ nlinkg,other.inner,outer)
    }

    def tensor(other : Bigraph) : Bigraph = {
        val nctrl = ctrl ++ other.ctrl
        val nV = V ++ other.V
        val nE = E ++ other.E

        val nprnt = prnt ++ (for((a,b) <- other.prnt) yield {
            (a match {
                case ap : Hole => new Hole(ap.id + inner.width)
                case _ => a
             },
             b match {
                case bp : Region => new Region(bp.id + outer.width)
                case _ => b
             }
            )
        })

        var nlink = link ++ other.link

        var ninner = new Face(inner.width+other.inner.width, inner.names ++ other.inner.names)
        var nouter = new Face(outer.width+other.outer.width, outer.names ++ other.outer.names)

        return new Bigraph(nV,nE,nctrl,nprnt,nlink,ninner,nouter)
    }

    def children(place : Place) : Set[Place] = {
        prnt.filter(m => m._2 == place).map(m => m._1).toSet
    }

    def regions : List[Place] = for(r <- List.range(0,outer.width)) yield (new Region(r))
    
    def holes : List[Place] = for(r <- List.range(0,inner.width)) yield (new Hole(r))

    def places : Set[Place] = V ++ holes ++ regions

    def descendants (place : Place) : Set[Place] = place match {
        case h : Hole => Set(h)
        case r : Region => {
            val ch = children(r)
            ch ++ ch.map(c => descendants(c)).flatten
        }
        case r : Node => {
            val ch = children(r)
            ch ++ ch.map(c => descendants(c)).flatten
        }
        case _ => Set()
    }

    def ports (n : Node) : Set[Link] = link.filter(m => m match {
        case (p : Port, _) => true
        case _ => false
    }).map(x => x._1).toSet

    def instantiate(m : Match, D : Bigraph) : Bigraph = {
        var Vmap : Map[Place,Node] = V.map(x => x -> new Node(Node.newId)).toMap

        var Nctrl = ctrl.map(x => Vmap(x._1) -> x._2)

        val (prntmap : Map[Place,Place],inst : Map[Place,Place]) = prnt.map(x => {
            val lhs = if (Vmap contains (x._1)) Vmap(x._1) else x._1
            val rhs = if (Vmap contains (x._2)) Vmap(x._2) else x._2
            lhs -> rhs
        }).partition(x => !x._1.isHole)

        var Nlink = link.map(x => if(m.linkMap contains x._2) x._1 -> m.linkMap(x._2) else x)

        val Nprnt : Map[Place,Place] = prntmap ++ (inst.map(x => {
            x._1 match {
                case h : Hole => {
                    val c = m.getParam(h.id)

                    for(ch <- c) yield {
                        Vmap += ch -> new Node(Node.newId)
                    }

                    Nctrl = Nctrl ++ D.ctrl.filter(t => c contains t._1).map(x => Vmap(x._1) -> x._2)

                    Nlink = Nlink ++ D.link.filter(t => {
                        t._1 match {
                            case p : Port => c contains p.node
                            case _ => true
                        }
                    }).map(t => t._1 match {
                        case p : Port => new Port(Vmap(p.node), p.id) -> t._2
                        case _ => t
                    })

                    D.prnt.filter(d => c contains d._1).map(d => {
                        d._2 match {
                            case r : Region => Vmap(d._1) -> x._2
                            case _ => Vmap(d._1) -> Vmap(d._2)
                        }
                    }).toMap
                }
                case _ => throw new IllegalArgumentException("Non-hole in left hand side of instantiation")
            }
        }).flatten.toMap)


        new Bigraph(Vmap.values.toSet,E ++ D.E,Nctrl,Nprnt,Nlink,new Face(0,Set()),outer)
    }

    def apply (m : Match, reactum : Bigraph) : Bigraph = {
        val B = m.toContext

        val C = B._1
        val D = B._3

        val ireactum = reactum.instantiate(m,D)

        C compose ireactum
    }

    def inbound(e : Edge) : Set[Link] = link.filter(x => x._2 == e).values.toSet

    def connectedNodes(e : Edge) : Set[Place] = inbound(e).filter(x => x match {
        case p : Port => true
        case _ => false
    }).map(x => x match {
        case p : Port => p.node
    })
}


