import org.specs2.mutable._
import org.specs2.specification.Scope

import org.junit.runner._
import org.specs2.runner._

import org.bigraph.bigmc._
import org.bigraph.bigmc.matcher._
import org.bigraph.bigmc.parser._

import scala.collection.immutable.Map
import scala.collection.immutable.Set

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
    "Wide redex matching" should {
        "find 1 occurence of 'a.nil || b.nil' in 'a.nil | b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil | b.nil")
            val b2 = MetaCalcParser.toBigraph("a.nil || b.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 1 occurence of 'a.$0 || b.$0' in 'a.b.nil | b.c.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.b.nil | b.c.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0 || b.$1")
            val m = new Matcher(b1,b2)


            m.all.size mustEqual 1
        }
        "find 2 occurences of 'a.$0 || a.$1' in 'a.nil | a.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil | a.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0 || a.$1")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2
        }
        "find 2 occurences of 'a.$0 || b.$1' in 'a.b.nil | b.a.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.b.nil | b.a.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0 || b.$1")
            val m = new Matcher(b1,b2)
            m.all.size mustEqual 2
        }
        "find 4 occurences of 'a.$0 || b.$1' in 'a.a.nil | a.a.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.a.nil | a.a.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0 || a.$1")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 8
        }

        "find 2 occurences of 'a.$0 || b.(c | $1)' in 'a.nil | a.nil | b.(c.nil | a.nil | a.b.nil)'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil | a.nil | b.(c.nil | a.nil | a.b.nil)")
            val b2 = MetaCalcParser.toBigraph("a.$0 || b.(c | $1)")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2
        }

        "match 3 nodes in $1 for 'a.$0 || b.(c | $1)' in 'a.nil | a.nil | b.(c.nil | a.nil | a.b.nil)'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil | a.nil | b.(c.nil | a.nil | a.b.nil)")
            val b2 = MetaCalcParser.toBigraph("a.$0 || b.(c | $1)")
            val m = new Matcher(b1,b2)
            val k = m.all

            k.head.getParam(1).size mustEqual 3
        }

    }

    "Wide agent matching" should {
        "find 1 occurence of 'a.nil || b.nil' in 'a.nil || b.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil || b.nil")
            val b2 = MetaCalcParser.toBigraph("a.nil || b.nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 1 occurence of 'a.$0 || b.$0' in 'a.b.nil || b.c.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.b.nil || b.c.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0 || b.$1")
            val m = new Matcher(b1,b2)


            m.all.size mustEqual 1
        }
        "find 2 occurences of 'a.$0 || a.$1' in 'a.nil || a.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil || a.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0 || a.$1")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2
        }
        "find 2 occurences of 'a.$0 || b.$1' in 'a.b.nil || b.a.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.b.nil || b.a.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0 || b.$1")
            val m = new Matcher(b1,b2)
            m.all.size mustEqual 2
        }
        "find 4 occurences of 'a.$0 || b.$1' in 'a.a.nil || a.a.nil'" in {
            val b1 = MetaCalcParser.toBigraph("a.a.nil || a.a.nil")
            val b2 = MetaCalcParser.toBigraph("a.$0 || a.$1")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 8
        }

        "find 2 occurences of 'a.$0 || b.(c | $1)' in 'a.nil || a.nil || b.(c.nil | a.nil | a.b.nil)'" in {
            val b1 = MetaCalcParser.toBigraph("a.nil || a.nil || b.(c.nil | a.nil | a.b.nil)")
            val b2 = MetaCalcParser.toBigraph("a.$0 || b.(c | $1)")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2
        }
    }

    "Link graph matching" should {
        "find 1 occurence of 'a[x].nil in 'a[z].nil'" in {
            val b1 = MetaCalcParser.toBigraph("a[x].nil")
            val b2 = MetaCalcParser.toBigraph("a[z].nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 0 occurences of 'a[x,y].nil' in 'a[z].nil'" in {
            val b1 = MetaCalcParser.toBigraph("a[z].nil")
            val b2 = MetaCalcParser.toBigraph("a[x,y].nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 0
        }
        "find 0 occurences of 'a[x].nil' in 'a[y,z].nil'" in {
            val b1 = MetaCalcParser.toBigraph("a[y,z].nil")
            val b2 = MetaCalcParser.toBigraph("a[x].nil")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 0
        }
        "find 2 occurences of '(νx) (a[x].nil | a[x].nil)' in '(νy) (a[y].nil | a[y].nil)'" in {
            val b1 = MetaCalcParser.toBigraph("(νy) (a[y].nil | a[y].nil)")
            val b2 = MetaCalcParser.toBigraph("(νx) (a[x].nil | a[x].nil)")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2
        }
        "find 0 occurences of '(νx) (a[x].nil | a[x].nil)' in '(νy) a[y].nil | (νx) a[x].nil'" in {
            val b1 = MetaCalcParser.toBigraph("(νy) a[y].nil | (νx) a[x].nil")
            val b2 = MetaCalcParser.toBigraph("(νx) (a[x].nil | a[x].nil)")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 0
        }
        "find 1 occurence of '(νe) ((a[e].nil | $0) || [x |-> e])' in '(νx) (a[x].nil | c.b[x].nil)'" in {
            val b1 = MetaCalcParser.toBigraph("(νx) (a[x].nil | c.b[x].nil)")
            val b2 = MetaCalcParser.toBigraph("(νe) ((a[e].nil | $0) || [x |-> e])")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 0 occurences of '(νe) ((a[e].nil | $0) || [x |-> e])' in '(νx) (a[x].nil | c.b.nil)'" in {
            val b1 = MetaCalcParser.toBigraph("(νx) (a[x].nil | c.b.nil)")
            val b2 = MetaCalcParser.toBigraph("(νe) ((a[e].nil | $0) || [x |-> e])")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 0
        }
        "find 0 occurences of '(νe) ((a[e].nil | $0) || [x |-> e])' in '(νx) (a[x].nil | c.b[y].nil)'" in {
            val b1 = MetaCalcParser.toBigraph("(νx) (a[x].nil | c.b[y].nil)")
            val b2 = MetaCalcParser.toBigraph("(νe) ((a[e].nil | $0) || [x |-> e])")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 0
        }
        "find 1 occurences of '(νe) ((a[e,q].nil | $0) || [x |-> e])' in '(νx) (a[x,p].nil | c.b[x].nil)'" in {
            val b1 = MetaCalcParser.toBigraph("(νx) (a[x,p].nil | c.b[x].nil)")
            val b2 = MetaCalcParser.toBigraph("(νe) ((a[e,q].nil | $0) || [x |-> e])")
            val m = new Matcher(b1,b2)


            m.all.size mustEqual 1
        }
        "find 2 occurences of 'a[x] | b[x]' in 'a[k] | b[k] | b[j] | b[k]'" in {
            val b1 = MetaCalcParser.toBigraph("a[k] | b[k] | b[j] | b[k]")
            val b2 = MetaCalcParser.toBigraph("a[x] | b[x]")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2
        }
        "find 4 occurences of 'a[x] | b[x]' in 'a[k] | b[k] | b[k] | a[k]'" in {
            val b1 = MetaCalcParser.toBigraph("a[k] | b[k] | b[k] | a[k]")
            val b2 = MetaCalcParser.toBigraph("a[x] | b[x]")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 4
        }
        "find 1 occurence of '(νe) ((a[e].nil | $0) || [x |-> e])' in '(νx) (a[x].nil | c.b[x,x].nil)'" in {
            val b1 = MetaCalcParser.toBigraph("(νx) (a[x].nil | c.b[x,x].nil)")
            val b2 = MetaCalcParser.toBigraph("(νe) ((a[e].nil | $0) || [x |-> e])")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 1
        }
        "find 2 occurence of '(νe) ((a[e].nil | c.$0 | c.$1) || [x |-> e])' in '(νx) (a[x].nil | c.b[x].nil | c.d[x].nil)'" in {
            val b1 = MetaCalcParser.toBigraph("(νx) (a[x].nil | c.b[x].nil | c.d[x].nil)")
            val b2 = MetaCalcParser.toBigraph("(νe) ((a[e].nil | c.$0 | c.$1) || [x |-> e])")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 2
        }
        "find 4 occurences of '(νe)(νf)((a[e].nil | c.$0 | a[f].nil | c.$1) || [x |-> e] || [y |-> f])' in '(νx)(νy) (a[x].nil | c.b[x].nil | a[y].nil | c.d[y].nil)'" in {
            val b1 = MetaCalcParser.toBigraph("(νx)(νy) (a[x].nil | c.b[x].nil | a[y].nil | c.d[y].nil)")
            val b2 = MetaCalcParser.toBigraph("(νe)(νf)((a[e].nil | c.$0 | a[f].nil | c.$1) || [x |-> e] || [y |-> f])")
            val m = new Matcher(b1,b2)

            m.all.size mustEqual 4
        }

    }

    "Parameter set" should {
        "be of size 2 for '(νe) (a[e].nil | $0)' in '(νx) (a[x].nil | c.b[y].nil)" in {
            val b1 = MetaCalcParser.toBigraph("(νx) (a[x].nil | c.b[y].nil)")
            val b2 = MetaCalcParser.toBigraph("(νe) (a[e].nil | $0)")
            val m = new Matcher(b1,b2)

            val ma = m.all

            println("MA: " + ma.head.parameters)
            println("b1: " + b1)

            ma.head.parameters.size mustEqual 2
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


