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
    "Matching ground redexes" should {
        "find 1 occurence of 'a.nil' in 'a.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil")
            val b2 = MetaCalcParser.toBigraph("a.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 1 occurence of 'a.nil' in 'b.a.nil'" in {
            val b1 = MetaCalcParser.toBigraph("b.a.nil")
            val b2 = MetaCalcParser.toBigraph("a.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 2 occurences of 'a.nil' in 'b.a.nil | b.a.nil'" in {
            val b1 = MetaCalcParser.toBigraph("b.a.nil | b.a.nil")
            val b2 = MetaCalcParser.toBigraph("a.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2
        }
        "find 1 occurence of 'a.b.nil' in 'a.b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.b.nil")
            val b2 = MetaCalcParser.toBigraph("a.b.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 1 occurence of 'a.b.nil' in 'a.a.b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.a.b.nil")
            val b2 = MetaCalcParser.toBigraph("a.b.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 2 occurences of 'a.b.nil' in 'a.b.nil | a.b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.b.nil | a.b.nil")
            val b2 = MetaCalcParser.toBigraph("a.b.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2
        }
        "find 2 occurences of 'a.b.nil' in 'a.a.b.nil | a.a.b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.a.b.nil | a.a.b.nil")
            val b2 = MetaCalcParser.toBigraph("a.b.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2
        }
        "find 0 occurences of 'a.b.nil' in 'a.b.c.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.b.c.nil")
            val b2 = MetaCalcParser.toBigraph("a.b.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 0
        }
        "find 1 occurences of 'a.nil | b.nil' in 'a.nil | b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil | b.nil")
            val b2 = MetaCalcParser.toBigraph("a.nil | b.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 2 occurences of 'a.nil | a.nil' in 'a.nil | a.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil | a.nil")
            val b2 = MetaCalcParser.toBigraph("a.nil | a.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2
        }
        "find 6 occurences of 'a.nil | a.nil' in 'a.nil | a.nil | a.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil | a.nil | a.nil")
            val b2 = MetaCalcParser.toBigraph("a.nil | a.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 6
        }
        "find 2 occurences of 'a.nil | b.nil' in 'a.nil | b.nil | b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil | b.nil | b.nil")
            val b2 = MetaCalcParser.toBigraph("a.nil | b.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2 
        }
        "find 0 occurences of 'a.nil | a.nil' in 'a.b.nil | a.b.nil | a.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.b.nil | a.b.nil | a.nil")
            val b2 = MetaCalcParser.toBigraph("a.nil | a.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 0
        }
        "find 2 occurences of 'a.nil | a.nil' in 'x.(a.nil | a.nil)'" in {
            val b1 = MetaCalcParser.toBigraph("x.(a.nil | a.nil)")
            val b2 = MetaCalcParser.toBigraph("a.nil | a.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2
        }
        "find 2 occurences of 'x.(a.nil | a.nil)' in 'x.(a.nil | a.nil)'" in {
            val b1 = MetaCalcParser.toBigraph("x.(a.nil | a.nil)")
            val b2 = MetaCalcParser.toBigraph("x.(a.nil | a.nil)")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2
        }
        "find 0 occurences of 'x.(a.nil | a.nil)' in 'x.(a.nil | a.nil | a.nil)'" in {
            val b1 = MetaCalcParser.toBigraph("x.(a.nil | a.nil | a.nil)")
            val b2 = MetaCalcParser.toBigraph("x.(a.nil | a.nil)")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 0
        }
        "find 1 occurence of 'a.nil | b.nil' in 'a.nil | b.nil | c.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil | b.nil | c.nil")
            val b2 = MetaCalcParser.toBigraph("a.nil | b.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
    }
    "Parametric redexes" should {
        "find 1 occurence of 'a.$0' in 'a.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 1 occurencs of 'a.$0' in 'a.b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.b.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 1 occurence of 'a.$0' in 'a.b.c.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.b.c.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 2 occurences of 'a.$0' in 'a.a.b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.a.b.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2
        }
        "find 1 occurence of 'a.(b.nil | $0)' in 'a.b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.b.nil")
            val b2 = MetaCalcParser.toBigraph("a.(b.nil | $0)")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 2 occurences of 'a.(b.nil | $0)' in 'a.(b.nil | b.nil)'" in {
            val b1 = MetaCalcParser.toBigraph("a.(b.nil | b.nil)")
            val b2 = MetaCalcParser.toBigraph("a.(b.nil | $0)")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2 
        }
        "find 1 occurence of 'a.b.(b.nil | $0)' in 'a.b.b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.b.b.nil")
            val b2 = MetaCalcParser.toBigraph("a.b.(b.nil | $0)")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 0 occurences of 'a.b.(b.nil | $0)' in 'a.b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.b.nil")
            val b2 = MetaCalcParser.toBigraph("a.b.(b.nil | $0)")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 0
        }
        "find 0 occurences of 'a.b.(b.nil | $0)' in 'a.b.b.b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.b.b.b.nil")
            val b2 = MetaCalcParser.toBigraph("a.b.(b.nil | $0)")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 0
        }
        "find 1 occurence of 'a.$0 | b.$1' in 'a.nil | b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil | b.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0 | b.$1")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 1 occurence of 'a.$0 | b.$1' in 'a.c.nil | b.d.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.c.nil | b.d.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0 | b.$1")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 2 occurences of 'a.$0 | b.$1' in 'a.nil | b.nil | b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil | b.nil | b.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0 | b.$1")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2 
        }
        "find 2 occurences of 'a.$0 | b.$1' in 'a.nil | b.c.nil | b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil | b.c.nil | b.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0 | b.$1")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2 
        }


    }
    "Wide parametric matching" should {
        "find 1 occurence of 'a.$0 || b.$0' in 'a.b.nil | b.c.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.b.nil | b.c.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0 || b.$0")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
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


