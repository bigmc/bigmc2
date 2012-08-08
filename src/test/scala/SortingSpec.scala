import org.specs2.mutable._
import org.specs2.specification.Scope

import org.junit.runner._
import org.specs2.runner._

import org.bigraph.bigmc._

import org.bigraph.bigmc.parser._

import scala.collection.immutable.Map
import scala.collection.immutable.Set

//List("(",")","↖", "↘", "⇱","⇲","≁","∧","∨","∀",":",",","-" )
/*
@RunWith(classOf[JUnitRunner])
class SortingSpecTest extends SpecificationWithJUnit {

    "The sorting '∀x,y : x != y => x ↘ y ∨ y ↘ x'" should {
        "find 'a.b.nil' well-sorted" in {
            val b = MetaCalcParser.toBigraph("a.b.nil")
            //Sorting.fromString("∀x,y : x != y =>  x ↘ y ∨ y ↘ x").check(b)
        }
        "find 'a.nil | b.c.nil' not well-sorted" in {
            val b = MetaCalcParser.toBigraph("a.nil | b.c.nil")
            !Sorting.fromString("∀x,y : x != y => x ↘ y ∨ y ↘ x").check(b)
        }
    }
    "The sorting '∀x,y : (ctrl(x) = recv ∧ x[1] -- y[0] ∧ (ctrl(y) = recv ∨ ctrl(y) = send)) => x ⇲ y'" should {
        "find 'recv[q,r].send[r,s].nil' well-sorted" in {
            val s = "∀x,y : (ctrl(x) = recv ∧ x[1] -- y[0] ∧ (ctrl(y) = recv ∨ ctrl(y) = send)) => x ⇲ y"
            val b = MetaCalcParser.toBigraph("recv[q,r].send[r,s].nil")
            Sorting.fromString(s).check(b)
        }
        "find 'recv[q,r].nil | send[r,s].nil' not well-sorted" in {
            val s = "∀x,y : (ctrl(x) = recv ∧ x[1] -- y[0] ∧ (ctrl(y) = recv ∨ ctrl(y) = send)) => x ⇲ y"
            val b = MetaCalcParser.toBigraph("recv[q,r].nil | send[r,s].nil")
            !Sorting.fromString(s).check(b)
        }

    }
}
*/
