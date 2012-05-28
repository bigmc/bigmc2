package org.bigraph.bigmc

import org.bigraph.bigmc.matcher._
import org.bigraph.bigmc.parser._

import scala.collection.immutable.Map
import scala.collection.immutable.Set

class VarAssignment(V : Set[Node], vars : List[String]) extends Iterator[Map[String,Node]] {
    var l : List[Set[Node]] = vars.map(x => V)

    var done = false

    def hasNext = {
        !done
    }

    /** Generation the next assignment set. **/
    def genNext (l : List[Set[Node]]) : List[Set[Node]] = {
        if(l.size == 0)
            return Nil

        val h = l.head

        if(h.size == 1) {
            V :: genNext(l.tail)
        } else {
            (h.tail) :: l.tail
        }
    }

    def next = {
        val r = (for(x <- List.range(0,vars.size)) yield {
            vars(x) -> l(x).head
        }).toMap
        
        done = l.forall(x => x.size == 1)

        l = genNext(l)

        r
    }
}

/** A sorting that can be applied to a bigraph. */
class Sorting(term : LTerm) {
    
    def eval (b : Bigraph, vars : Map[String,Node]) : Boolean = {
        println("Evaluating: " + vars)
        
        true
    }

    def check(b : Bigraph) : Boolean = {
        term match {
            case LPredicate(vars, body) => {
                val va = new VarAssignment(b.V, vars)

                println("VarAss: " + b.V)

                for(v <- va) {
                    if(!eval(b,v)) return false
                }

                return true
            }
            case _ => throw new IllegalArgumentException("Malformed predicate: '" + term + "'")
        }
    }
}

object Sorting {
    def fromString (s : String) : Sorting = {
        new Sorting(LogicParser.apply(s))
    }
}

