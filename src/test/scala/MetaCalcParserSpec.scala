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
            MetaCalcParser.test("zero")
        }
        "parse 'nil | nil'" in {
            MetaCalcParser.test("nil | nil")
        }
        "parse '(nil)'" in {
            MetaCalcParser.test("(nil)")
        }
        "parse '0 || 0'" in {
            MetaCalcParser.test("zero || zero")
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
        "parse '[x |-> y]'" in {
            MetaCalcParser.test("[x |-> y]")
        }
        "parse '[x |-> y] || 0'" in {
            MetaCalcParser.test("[x |-> y] || 0")
        }
        "parse 'a.(x | y | $0)'" in {
            MetaCalcParser.test("a.(x | y | $0)")
        }
        "parse 'a.(x | y | $0) || a.b.c'" in {
            MetaCalcParser.test("a.(x | y | $0) || a.b.c")
        }
        "parse 'a.(x | y | $0) || a.(b | c | $1)'" in {
            MetaCalcParser.test("a.(x | y | $0) || a.(b | c | $1)")
        }
        "parse 'a[t].(x[u] | y[v] | $0) || a[w].(b | c[z] | $1)'" in {
            MetaCalcParser.test("a[t].(x[u] | y[v] | $0) || a[w].(b | c[z] | $1)")
        }
        "parse 'send[c,x].$0 || recv[c,z].$1 || [x |-> z]'" in {
            MetaCalcParser.test("send[c,x].$0 || recv[c,z].$1 || [x |-> z]")
        }
        "parse '(νx)(a[x].nil || b[x].nil)'" in {
            MetaCalcParser.test("(νx)(a[x].nil || b[x].nil)")
        }
        "parse 'a[x].(νx)b[x].c[x].nil'" in {
            MetaCalcParser.test("a[x].(νx)b[x].c[x].nil")
        }
	}
    "a BigraphTranslator" should {
        "have one node for 'a.nil'" in {
            MetaCalcParser.toBigraph("a.nil").V.size mustEqual 1
        }
        "have two nodes for 'a.nil | b.nil'" in {
            MetaCalcParser.toBigraph("a.nil | b.nil").V.size mustEqual 2
        }
        "have outer width 2 for 'a.nil || b.nil'" in {
            MetaCalcParser.toBigraph("a.nil || b.nil").outer.width mustEqual 2
        }
        "have inner width 2 for 'a.$0 | a.$1'" in {
            MetaCalcParser.toBigraph("a.$0 | a.$1").inner.width mustEqual 2
        }
        "have parent '2' for $1 in 'a.$0 || $1'" in {
            MetaCalcParser.toBigraph("a.$0 || $1").prnt(new Hole(1)) mustEqual (new Region(2))
        }
        "have parent 'a' for 'b' in 'a.b.nil'" in {
            val b = MetaCalcParser.toBigraph("a.b.nil")
           
            val n0 = b.V.head
            val n1 = b.V.tail.head

            val na = if (b.ctrl(n0).toString == "a") { n0 } else { n1 }
            val nb = if (b.ctrl(n0).toString == "b") { n0 } else { n1 }

            b.prnt(nb) mustEqual na 
        }
    }
    "freeName" should {
        "report 'x,y,z' for 'a[x,y,z].nil'" in {
            MetaCalcParser.apply("a[x,y,z].nil").freeNames(Set()) mustEqual Set(new Ident("x"),new Ident("y"),new Ident("z"))
        }
        "report 'x,z' for '(νy) a[x,y,z].nil'" in {
            MetaCalcParser.apply("(νy) a[x,y,z].nil").freeNames(Set()) mustEqual Set(new Ident("x"),new Ident("z"))	
        }
        "report 'x,y,z' for '(νy) a[x,y,z].nil || b[y].nil'" in {
            MetaCalcParser.apply("(νy) a[x,y,z].nil || b[y].nil").freeNames(Set()) mustEqual Set(new Ident("x"),new Ident("y"),new Ident("z"))	
        }
        "report 'x' for 'a[x].nil | (νx) b[x].nil'" in {
            MetaCalcParser.apply("a[x].nil | (νx) b[x].nil").freeNames(Set()) mustEqual Set(new Ident("x"))	
        }
        "report '' for '(νx) a[x].nil | (νx) b[x].nil | (νy) c[y].nil'" in {
            MetaCalcParser.apply("(νx) a[x].nil | (νx) b[x].nil | (νy) c[y].nil").freeNames(Set()).size mustEqual 0	
        }
    }
    "substitution" should {
        "turn 'a[x].nil' into 'a[y].nil'" in {
            MetaCalcParser.apply("a[x].nil").substitute(new Ident("x"),new Ident("y")).toString mustEqual "a[y].nil"
        }
        "turn 'a[x].nil | b[x].nil | c[z].nil' into 'a[y].nil | b[y].nil | c[z].nil'" in {
            MetaCalcParser.apply("a[x].nil | b[x].nil | c[z].nil").substitute(new Ident("x"),new Ident("y")).toString mustEqual "(a[y].nil | (b[y].nil | c[z].nil))"
        }
        "turn 'a[x].nil || b[x].nil' into 'a[y].nil || b[y].nil'" in {
            MetaCalcParser.apply("a[x].nil || b[x].nil").substitute(new Ident("x"),new Ident("y")).toString mustEqual "(a[y].nil || b[y].nil)"
        }
        "turn 'a[x].nil | (νx) a[x].nil' into a different binding'" in {
            MetaCalcParser.apply("a[x].nil | (νx) a[x].nil").substitute(new Ident("x"),new Ident("y")).toString mustNotEqual "a[y].nil | (νx) a[y].nil"
        }
        "turn 'a[x,y,z].nil' into 'a[y,y,z].nil'" in {
             MetaCalcParser.apply("a[x,y,z].nil").substitute(new Ident("x"),new Ident("y")).toString mustEqual "a[y,y,z].nil"
        }
        "turn 'a[x].b[z].c[x].nil' into 'a[y].b[z].c[y]'" in {
            MetaCalcParser.apply("a[x].b[z].c[x].nil").substitute(new Ident("x"),new Ident("y")).toString mustEqual "a[y].b[z].c[y].nil"
        }
        "turn '[x |-> y]' into '[y |-> y]'" in {
            MetaCalcParser.apply("[x |-> y]").substitute(new Ident("x"),new Ident("y")).toString mustEqual "[y |-> y]"
        }
        "not alter '(νx)(a[x].nil || b[x].nil)'" in {
            MetaCalcParser.apply("(νx)(a[x].nil || b[x].nil)").substitute(new Ident("x"),new Ident("y")).toString mustEqual "(νx)(a[x].nil || b[x].nil)"
        }

    }
    "normalisation" should {
        "Lift binding to the top-level for 'a[x].nil || (νx) b[x].nil'" in {
            println("File Encoding: " + System.getProperty("file.encoding"))

            val t =  MetaCalcParser.apply("a[x].nil || (νx) b[x].nil")
            val nt = t.normalise(t)

            println("Normalise: " + nt)

            nt match {
                case TNew(n,b) => { n.toString != "x" }
                case x => { false }
                }
        }
        "Lift binding to the top level for 'a[x].(νx)b[x].c[x].nil'" in {
            val t =  MetaCalcParser.apply("a[x].(νx)b[x].c[x].nil")
            val nt = t.normalise(t)

            println("Normalise: " + nt)

            nt match {
                case TNew(n,b) => { n.toString != "x" }
                case x => { false }
                }

        }
        "Lift binding to the top level for 'a[x].b[x].(νx)c[x].nil'" in {
            val t =  MetaCalcParser.apply("a[x].b[x].(νx)c[x].nil")
            val nt = t.normalise(t)

            println("Normalise: " + nt)

            nt match {
                case TNew(n,b) => { n.toString != "x" }
                case x => { false }
                }

        }
    }

}



