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

/** 
case class ParentOf extends LOper { }
case class ChildOf extends LOper { }
case class AncestorOf extends LOper { }
case class DescendantOf extends LOper { }
case class NotLinked extends LOper { }
case class Linked extends LOper { }
case class NotEqual extends LOper { }
case class Equal extends LOper { }
case class LAnd extends LOper { }
case class LOr extends LOper { }
case class CtrlEq extends LOper { }
case class Implies extends LOper { }

sealed abstract class LTerm {

}

case class LBinOp(lhs:LTerm, pred: LOper, rhs:LTerm) extends LTerm { }
case class LIdent(id : String) extends LTerm { }
case class LPort(lhs:LTerm, id : Int) extends LTerm { }
case class LPredicate(vars: List[String], body:LTerm) extends LTerm { }

**/

/** A sorting that can be applied to a bigraph. */
class Sorting(term : LTerm) {
    
    def eval (t : LTerm, b : Bigraph, env : Map[String,Node]) : Boolean = {
        val r = t match {
            case LBinOp(LIdent(l),ParentOf(),LIdent(r)) => b.prnt(env(l)) == env(r)
            case LBinOp(LIdent(l),ChildOf(),LIdent(r)) => b.prnt(env(r)) == env(l)
            case LBinOp(LIdent(x),AncestorOf(),LIdent(y)) => b.descendants(env(y)) contains env(x)
            case LBinOp(LIdent(x),DescendantOf(),LIdent(y)) => b.descendants(env(x)) contains env(y)
            case LBinOp(LIdent(x),NotEqual(),LIdent(y)) => env(x) != env(y)
            case LBinOp(LIdent(x),Equal(),LIdent(y)) => env(x) == env(y)
            case LBinOp(LIdent(x),CtrlEq(),LIdent(y)) => b.ctrl(env(x)).name == y
            case LBinOp(LPort(LIdent(x),i),Linked(),LPort(LIdent(y),j)) => {
                val p1 = new Port(env(x),i)
                val p2 = new Port(env(y),j)

                if(p1 == p2) false
                else if(!(b.link contains p1) || !(b.link contains p2)) false
                else b.link(p1) == b.link(p2)
            }
            case LBinOp(LPort(LIdent(x),i),NotLinked(),LPort(LIdent(y),j)) => {
                val p1 = new Port(env(x),i)
                val p2 = new Port(env(y),j)

                if(p1 == p2) true
                else if(!(b.link contains p1) || !(b.link contains p2)) true
                else b.link(p1) != b.link(p2)
            }
            case LBinOp(lhs,LAnd(),rhs) => eval(lhs,b,env) && eval(rhs,b,env)
            case LBinOp(lhs,LOr(),rhs) => eval(lhs,b,env) || eval(rhs,b,env)
            case LBinOp(lhs,Implies(),rhs) => !(eval(lhs,b,env)) || eval(rhs,b,env)
            case _ => throw new IllegalArgumentException("Malformed sorting term: " + term + " near: " + t)
        }
        println("Eval: " + t + " on: " + env + " res: " + r)
        r
    }

    def check(b : Bigraph) : Boolean = {
        term match {
            case LPredicate(vars, body) => {
                val va = new VarAssignment(b.V, vars)

                println("VarAss: " + b.V + " for " + term)

                for(v <- va) {
                    if(!eval(body,b,v)) return false
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

