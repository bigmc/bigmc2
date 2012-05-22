import org.specs2.mutable._
import org.specs2.specification.Scope

import org.junit.runner._
import org.specs2.runner._

import org.bigraph.bigmc._
import org.bigraph.bigmc.matcher._
import org.bigraph.bigmc.parser._

import scala.collection.immutable.Map

@RunWith(classOf[JUnitRunner])
class MatchSpecTest extends SpecificationWithJUnit {
    "Candidate sets for 'a.$0' in 'a.b.nil'" should {
        "be of size 1" in new matchtest {
            matcher.candidates(n).size mustEqual 1
        }
    }
    "Matching 'a.$0' in 'a.b.nil'" should {
        "find 1 occurence" in new matchtest {
            matches.size mustEqual 1
        }
    }
    "Matching 'a.(b | $0)' in 'a.b.nil'" should {
        "find 1 occurence" in {
            val b1 = MetaCalcParser.toBigraph("a.b.nil")
            val b2 = MetaCalcParser.toBigraph("a.(b | $0)")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
    }
    "Matching 'a.b.(b | $0)' in 'a.b.b.nil'" should {
        "find 1 occurence" in {
            val b1 = MetaCalcParser.toBigraph("a.b.b.nil")
            val b2 = MetaCalcParser.toBigraph("a.b.(b | $0)")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
    }
    "Matching 'a.b.(b | $0)' in 'b.b.b.nil'" should {
        "find 0 occurences" in {
            val b1 = MetaCalcParser.toBigraph("b.b.b.nil")
            val b2 = MetaCalcParser.toBigraph("a.b.(b | $0)")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 0
        }
    }
    "Matching 'a.$0 || b.$0' in 'a.b.nil | b.a.nil'" should {
        "find 1 occurences" in {
            val b1 = MetaCalcParser.toBigraph("a.b.nil | b.a.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0 || b.$0")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1 
        }
        "recompose correctly" in {
            val b1 = MetaCalcParser.toBigraph("a.b.nil | b.a.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0 || b.$0")
            val m = new Matcher(b1,b2)
            val m1 = m.all.head
            val c1 = m1.C.compose(m1.B.compose(m1.D))

            c1.toString mustEqual b1.toString
        }
    }


}

trait matchtest extends Scope {
    val b1 = MetaCalcParser.toBigraph("a.b.nil")
    val b2 = MetaCalcParser.toBigraph("a.$0")

    val n = b2.V.head

    val matcher = new Matcher(b1,b2)
    val matches = matcher.all
}


