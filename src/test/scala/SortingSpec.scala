import org.specs2.mutable._
import org.specs2.specification.Scope

import org.junit.runner._
import org.specs2.runner._

import org.bigraph.bigmc._

import org.bigraph.bigmc.parser._

import scala.collection.immutable.Map
import scala.collection.immutable.Set

//List("(",")","↖", "↘", "⇱","⇲","≁","∧","∨","∀",":",",","-" )

@RunWith(classOf[JUnitRunner])
class SortingSpecTest extends SpecificationWithJUnit {

    "The sorting '∀x,y : x ↘ y ∨ y ↘ x'" should {
        "find 'a.b.nil' well-sorted" in {
            val b = MetaCalcParser.toBigraph("a.b.nil")
            Sorting.fromString("∀x,y : x ↘ y ∨ y ↘ x").check(b)
        }
        "find 'a.nil | b.c.nil' not well-sorted" in {
            val b = MetaCalcParser.toBigraph("a.nil | b.c.nil")
            !Sorting.fromString("∀x,y : x ↘ y ∨ y ↘ x").check(b)
        }
    }
}

