import org.specs2.mutable._
import org.specs2.specification.Scope

import org.junit.runner._
import org.specs2.runner._

import org.bigraph.bigmc._

import org.bigraph.bigmc.parser._

import scala.collection.immutable.Map
import scala.collection.immutable.Set

@RunWith(classOf[JUnitRunner])
class LogicParserSpecTest extends SpecificationWithJUnit {

	"The LogicParser" should {
		"parse 'a'" in {
		    LogicParser.test("a")
        }
    }
}

