package org.bigraph.bigmc

import org.bigraph.bigmc.matcher._

import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge
import scalax.collection.edge.Implicits._

import scala.collection.immutable.Set
import scala.collection.immutable.List

trait ExecutionStrategy {
    def step() : Unit
    var done : Boolean
    def report() : String
}

class BigraphicalReactiveSystem(agent : Bigraph, signature : Set[Control], rules : Set[ReactionRule], sorting : Sorting) extends ExecutionStrategy {
    val reactionGraph : Graph[Bigraph,LDiEdge] = Graph()

    var done = false

    var workQueue : List[Bigraph] = List(agent)

    def isComplete = done

    def step() : Unit = {
        println("Step: " + workQueue)
        if(workQueue.size == 0) {
            done = true
            return ()
        }

        val w = workQueue.head

        workQueue = workQueue.tail

        for(r <- rules) yield {
            println("Rule: " + r + " on " + w)
            val cand = r.apply(w).filter(c => sorting == null || sorting.check(c))


            cand.foreach(c => {
                if(!(workQueue contains c)) {
                    workQueue = workQueue :+ c
                }

                println(c)

                reactionGraph += (w ~+> c)(r)
            })
        }

        ()
    }

    def behave() : Unit = {
        while(!done) {
            println("Behave()")
            step()
        }
    }

    def report() : String = {
        "[WQ: " + workQueue.size + "]"
    }
}

class StochasticReactiveSystem(agent : Bigraph, signature : Set[Control], rules : Set[ReactionRule], sorting : Sorting) extends ExecutionStrategy {
    val reactionGraph : Graph[Bigraph,LDiEdge] = Graph()

    var done = false

    var workQueue : List[Bigraph] = List(agent)

    def isComplete = done

    def step() : Unit = {
        if(workQueue.size == 0) {
            done = true
            return ()
        }

        val w = workQueue.head

        workQueue = workQueue.tail

        for(r <- rules) yield {
            val cand = r.apply(w).filter(c => sorting == null || sorting.check(c))

            cand.foreach(c => {
                if(!(workQueue contains c)) {
                    workQueue = workQueue :+ c
                }

                reactionGraph += (w ~+> c)(r)
            })
        }

        ()
    }

    def behave() : Unit = {
        while(!done) {
            step()
        }
    }

    def report() : String = {
        "[WQ: " + workQueue.size + "]"
    }
}


