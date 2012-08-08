import org.specs2.mutable._
import org.specs2.specification.Scope

import org.junit.runner._
import org.specs2.runner._

import org.bigraph.bigmc._
import org.bigraph.bigmc.parser._

import scala.collection.immutable.Map
import scala.collection.immutable.Set

@RunWith(classOf[JUnitRunner])
class BigraphSpecTest extends SpecificationWithJUnit {
    "Bigraph equality" should {
        "find 'a.nil' == 'a.nil'" in {
            val a = MetaCalcParser.toBigraph("a.nil")
            val b = MetaCalcParser.toBigraph("a.nil")

            a == b
        }
        "find 'a.nil' != 'b.nil'" in {
            val a = MetaCalcParser.toBigraph("a.nil")
            val b = MetaCalcParser.toBigraph("b.nil")

            a != b
        }
        "find 'a.b.nil' == 'a.b.nil'" in {
            val a = MetaCalcParser.toBigraph("a.b.nil")
            val b = MetaCalcParser.toBigraph("a.b.nil")

            a == b
        }
        "find 'a.b.nil' != 'b.a.nil'" in {
            val a = MetaCalcParser.toBigraph("a.b.nil")
            val b = MetaCalcParser.toBigraph("b.a.nil")

            a != b
        }
        "find 'a.(b | c)' == 'a.(b | c).nil'" in {
            val a = MetaCalcParser.toBigraph("a.(b | c)")
            val b = MetaCalcParser.toBigraph("a.(b | c)")

            a == b
        }
        "find 'b.(a | c)' != 'a.(b | c).nil'" in {
            val a = MetaCalcParser.toBigraph("b.(a | c)")
            val b = MetaCalcParser.toBigraph("a.(b | c)")

            a != b
        }
        "find 'b || a' != 'a || b'" in {
            val a = MetaCalcParser.toBigraph("b || a")
            val b = MetaCalcParser.toBigraph("a || b")

            a != b
        }
         "find 'a || b' == 'a || b'" in {
            val a = MetaCalcParser.toBigraph("a || b")
            val b = MetaCalcParser.toBigraph("a || b")

            a == b
        }

        "find 'a.(b.c | d.e)' == 'a.(d.e | b.c)'" in {
            val a = MetaCalcParser.toBigraph("a.(b.c | d.e)")
            val b = MetaCalcParser.toBigraph("a.(d.e | b.c)")

            a == b
        }
        "find 'a.(b.c | d.e)' != 'a.(d.f | b.c)'" in {
            val a = MetaCalcParser.toBigraph("a.(b.c | d.e)")
            val b = MetaCalcParser.toBigraph("a.(d.f | b.c)")

            a != b
        }
        "find '(νe) a[e]' == '(νe) a[e]'" in {
            val a = MetaCalcParser.toBigraph("(νe) a[e]")
            val b = MetaCalcParser.toBigraph("(νe) a[e]")

            a == b
        }
        "find '(νe) a[e]' == '(νf) a[f]'" in {
            val a = MetaCalcParser.toBigraph("(νe) a[e]")
            val b = MetaCalcParser.toBigraph("(νf) a[f]")

            a == b
        }
        "find '(νe)(νf) a[e]' == '(νf)(νe) a[f]'" in {
            val a = MetaCalcParser.toBigraph("(νe)(νf) a[e]")
            val b = MetaCalcParser.toBigraph("(νf)(νe) a[f]")

            a == b
        }
        "find '(νe)(νf) a[e].b[f]' == '(νf)(νe) a[f].b[e]'" in {
            val a = MetaCalcParser.toBigraph("(νe)(νf) a[e].b[f]")
            val b = MetaCalcParser.toBigraph("(νf)(νe) a[f].b[e]")

            a == b
        }
        "find '(νe)(νf) a[e].b[e]' != '(νf)(νe) a[f].b[e]'" in {
            val a = MetaCalcParser.toBigraph("(νe)(νf) a[e].b[e]")
            val b = MetaCalcParser.toBigraph("(νf)(νe) a[f].b[e]")

            a != b
        }

        "find 'a[e] || [x |-> e]' == 'a[e] || [x |-> e]'" in {
            val a = MetaCalcParser.toBigraph("a[e] || [x |-> e]")
            val b = MetaCalcParser.toBigraph("a[e] || [x |-> e]")

            a == b
        }
        "find 'a[e] || [y |-> e]' != 'a[e] || [x |-> e]'" in {
            val a = MetaCalcParser.toBigraph("a[e] || [y |-> e]")
            val b = MetaCalcParser.toBigraph("a[e] || [x |-> e]")

            a != b
        }
    }

    "Bigraph hashCode" should {
        "find 'a.nil' == 'a.nil'" in {
            val a = MetaCalcParser.toBigraph("a.nil").hashCode
            val b = MetaCalcParser.toBigraph("a.nil").hashCode

            a == b
        }
        "find 'a.b.nil' == 'a.b.nil'" in {
            val a = MetaCalcParser.toBigraph("a.b.nil").hashCode
            val b = MetaCalcParser.toBigraph("a.b.nil").hashCode

            a == b
        }
        "find 'a.(b | c)' == 'a.(b | c).nil'" in {
            val a = MetaCalcParser.toBigraph("a.(b | c)").hashCode
            val b = MetaCalcParser.toBigraph("a.(b | c)").hashCode

            a == b
        }
         "find 'a || b' == 'a || b'" in {
            val a = MetaCalcParser.toBigraph("a || b").hashCode
            val b = MetaCalcParser.toBigraph("a || b").hashCode

            a == b
        }

        "find 'a.(b.c | d.e)' == 'a.(d.e | b.c)'" in {
            val a = MetaCalcParser.toBigraph("a.(b.c | d.e)").hashCode
            val b = MetaCalcParser.toBigraph("a.(d.e | b.c)").hashCode

            a == b
        }
        "find '(νe) a[e]' == '(νe) a[e]'" in {
            val a = MetaCalcParser.toBigraph("(νe) a[e]").hashCode
            val b = MetaCalcParser.toBigraph("(νe) a[e]").hashCode

            a == b
        }
        "find '(νe) a[e]' == '(νf) a[f]'" in {
            val a = MetaCalcParser.toBigraph("(νe) a[e]").hashCode
            val b = MetaCalcParser.toBigraph("(νf) a[f]").hashCode

            a == b
        }
        "find '(νe)(νf) a[e]' == '(νf)(νe) a[f]'" in {
            val a = MetaCalcParser.toBigraph("(νe)(νf) a[e]").hashCode
            val b = MetaCalcParser.toBigraph("(νf)(νe) a[f]").hashCode

            a == b
        }
        "find '(νe)(νf) a[e].b[f]' == '(νf)(νe) a[f].b[e]'" in {
            val a = MetaCalcParser.toBigraph("(νe)(νf) a[e].b[f]").hashCode
            val b = MetaCalcParser.toBigraph("(νf)(νe) a[f].b[e]").hashCode

            a == b
        }
        "find 'a[e] || [x |-> e]' == 'a[e] || [x |-> e]'" in {
            val a = MetaCalcParser.toBigraph("a[e] || [x |-> e]").hashCode
            val b = MetaCalcParser.toBigraph("a[e] || [x |-> e]").hashCode

            a == b
        }
    }
}

