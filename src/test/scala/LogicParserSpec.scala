import org.specs2.mutable._
import org.specs2.specification.Scope

import org.junit.runner._
import org.specs2.runner._

import org.bigraph.bigmc._

import org.bigraph.bigmc.parser._

import scala.collection.immutable.Map
import scala.collection.immutable.Set

//List("(",")","↖", "↘", "⇱","⇲","≁","∧","∨","∀",":","," )

@RunWith(classOf[JUnitRunner])
class LogicParserSpecTest extends SpecificationWithJUnit {

	"The LogicParser" should {
		"parse '∀x,y : x ↘ y ∨ y ↘ x'" in {
		    LogicParser.test("∀x,y : x ↘ y ∨ y ↘ x")
        }
    }
}

