import org.specs2.mutable._
import org.specs2.specification.Scope

import org.junit.runner._
import org.specs2.runner._

import org.bigraph.bigmc._

import org.bigraph.parser._

import scala.collection.immutable.Map

@RunWith(classOf[JUnitRunner])
class MetaCalcParserSpecTest extends SpecificationWithJUnit {

	"The MetaCalcParser" should {
		"parse '$0'" in {
		    MetaCalcParser.test("$0")
        }
        "parse '$1'" in {
            MetaCalcParser.test("$1")
        }
	}
}



