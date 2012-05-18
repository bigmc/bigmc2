import org.specs2.mutable._
import org.specs2.specification.Scope

import org.junit.runner._
import org.specs2.runner._

import org.bigraph.bigmc._

import org.bigraph.bigmc.parser._

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
        "parse 'nil'" in {
            MetaCalcParser.test("nil")
        }
        "parse '0'" in {
            MetaCalcParser.test("0")
        }
        "parse 'nil | nil'" in {
            MetaCalcParser.test("nil | nil")
        }
        "parse '(nil)'" in {
            MetaCalcParser.test("(nil)")
        }
        "parse '0 || 0'" in {
            MetaCalcParser.test("0 || 0")
        }
        "parse '(0 || 0)'" in {
            MetaCalcParser.test("(0 || 0)")
        }
        "parse '$0 || 0'" in {
            MetaCalcParser.test("$0 || 0")
        }
        "parse 'a.nil'" in {
            MetaCalcParser.test("a.nil")
        }
        "parse 'a.b.nil'" in {
            MetaCalcParser.test("a.b.nil")
        }
        "parse 'a[x].nil'" in {
            MetaCalcParser.test("a[x].nil")
        }
        "parse 'a[x,y].nil'" in {
            MetaCalcParser.test("a[x,y].nil")
        }
        "parse '(νx)nil'" in {
            MetaCalcParser.test("(νx)nil")
        }
        "parse '(νx) nil'" in {
            MetaCalcParser.test("(νx) nil")
        }
        "parse '(νx) a[x].nil'" in {
            MetaCalcParser.test("(νx) a[x].nil")
        }
        "parse '(νx) a[x].nil | (νy) a[y].nil'" in {
            MetaCalcParser.test("(νx) a[x].nil | (νy) a[y].nil")
        }
        "parse 'a.nil | b.nil'" in {
            MetaCalcParser.test("a.nil | b.nil")
        }
	}
}



