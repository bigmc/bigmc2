import org.specs2.mutable._
import org.specs2.specification.Scope

import org.junit.runner._
import org.specs2.runner._

import org.bigraph.bigmc._
import org.bigraph.bigmc.parser._

import scala.collection.immutable.Map
import scala.collection.immutable.Set

@RunWith(classOf[JUnitRunner])
class ReactionSpecTest extends SpecificationWithJUnit {
    "Rewriting" should {

        "produce 'b' for 'a -> b' on 'a'" in {
            val a = MetaCalcParser.toBigraph("a.nil")

            val redex = MetaCalcParser.toBigraph("a.nil")
            val reactum = MetaCalcParser.toBigraph("b.nil")
            val rule = new ReactionRule(redex,reactum)
            
            val res = rule.apply(a)

            res.head.toNiceString mustEqual "b.nil"
        }
        "produce 'b.b' for 'a -> b' on 'b.a'" in {
            val a = MetaCalcParser.toBigraph("b.a.nil",activity=Set("a","b"))

            val redex = MetaCalcParser.toBigraph("a.nil")
            val reactum = MetaCalcParser.toBigraph("b.nil")
            val rule = new ReactionRule(redex,reactum)
            
            val res = rule.apply(a)

            res.head.toNiceString mustEqual "b.b.nil"
        }
        "produce 'c.b' for 'a.$0 -> c.$0' on 'a.b'" in {
            val a = MetaCalcParser.toBigraph("a.b.nil")

            val redex = MetaCalcParser.toBigraph("a.$0")
            val reactum = MetaCalcParser.toBigraph("c.$0")
            val rule = new ReactionRule(redex,reactum)
            
            val res = rule.apply(a)

            res.head.toNiceString mustEqual "c.b.nil"
        }
        "produce 'b | b' for 'a -> b' on 'b | a'" in {
            val a = MetaCalcParser.toBigraph("b | a")

            val redex = MetaCalcParser.toBigraph("a.nil")
            val reactum = MetaCalcParser.toBigraph("b.nil")
            val rule = new ReactionRule(redex,reactum)
            
            val res = rule.apply(a)

            res.head.toNiceString mustEqual "b.nil | b.nil"
        }

        "produce 'nil' for 'send[x].$0 | recv[x].$1 -> $0 | $1' on 'send[a].nil | recv[a].nil'" in {
            val a = MetaCalcParser.toBigraph("send[a].nil | recv[a].nil")

            val redex = MetaCalcParser.toBigraph("send[x].$0 | recv[x].$1")
            val reactum = MetaCalcParser.toBigraph("$0 | $1")
            val rule = new ReactionRule(redex,reactum)
            
            val res = rule.apply(a)

            res.head.toNiceString mustEqual "nil"
        }

        "produce 'recv[b].nil | send[b].nil' for 'send[x].$0 | recv[x].$1 -> $0 | $1' on 'send[a].recv[b].nil | recv[a].send[b].nil'" in {
            val a = MetaCalcParser.toBigraph("send[a].recv[b].nil | recv[a].send[b].nil")

            val redex = MetaCalcParser.toBigraph("send[x].$0 | recv[x].$1")
            val reactum = MetaCalcParser.toBigraph("$0 | $1")
            val rule = new ReactionRule(redex,reactum)
            
            val res = rule.apply(a)
            
            res.head.toNiceString mustEqual "recv[b].nil | send[b].nil"
        }
    }

    "activity" should {
        "Inhibit matching 'b -> c' in a.b.nil" in {
            val a = MetaCalcParser.toBigraph("a.b.nil")

            val redex = MetaCalcParser.toBigraph("b.nil")
            val reactum = MetaCalcParser.toBigraph("c.nil")
            val rule = new ReactionRule(redex,reactum)
            
            val res = rule.apply(a)
            
            res.size mustEqual 0

        }
        "Inhibit matching 'b -> c' in a.b.b.nil with active a" in {
            val a = MetaCalcParser.toBigraph("a.b.b.nil",activity=Set("a"))

            val redex = MetaCalcParser.toBigraph("b.nil")
            val reactum = MetaCalcParser.toBigraph("c.nil")
            val rule = new ReactionRule(redex,reactum)
            
            val res = rule.apply(a)
            
            res.size mustEqual 0
        }
        "Limit matching 'b -> c' in 'a.b | b.b' with active a" in {
            val a = MetaCalcParser.toBigraph("a.b | b.b",activity=Set("a"))

            val redex = MetaCalcParser.toBigraph("b.nil")
            val reactum = MetaCalcParser.toBigraph("c.nil")
            val rule = new ReactionRule(redex,reactum)
            
            val res = rule.apply(a)
            
            res.size mustEqual 1
        }
    }

    "ReactiveSystem reactionGraph" should {
        "Have 4 nodes for 'a.$0 -> $0' on 'a.a.a.nil'" in {
            val a = MetaCalcParser.toBigraph("a.a.a.nil")
            val r = MetaCalcParser.toBigraph("a.$0")
            val rp = MetaCalcParser.toBigraph("$0")
            val rr = new ReactionRule(r,rp)
            val rs = new BigraphicalReactiveSystem(a,Set(),Set(rr),null)

            rs.behave()

            println("RS: " + rs.reactionGraph)

            rs.reactionGraph.order mustEqual 4
        }
        "Have 3 nodes for 'a.b.$0 -> $0' on 'a.b.a.b.nil' with active a" in {
            val a = MetaCalcParser.toBigraph("a.b.a.b.nil",activity=Set("a"))
            val r = MetaCalcParser.toBigraph("a.b.$0")
            val rp = MetaCalcParser.toBigraph("$0")
            val rr = new ReactionRule(r,rp)
            val rs = new BigraphicalReactiveSystem(a,Set(),Set(rr),null)

            rs.behave()

            println("RS: " + rs.reactionGraph)

            rs.reactionGraph.order mustEqual 3
        }
        "Have 3 nodes for 'a.b.$0 -> $0' on 'a.b.a.b.nil' with active a,b" in {
            val a = MetaCalcParser.toBigraph("a.b.a.b.nil",activity=Set("a","b"))
            val r = MetaCalcParser.toBigraph("a.b.$0")
            val rp = MetaCalcParser.toBigraph("$0")
            val rr = new ReactionRule(r,rp)
            val rs = new BigraphicalReactiveSystem(a,Set(),Set(rr),null)

            rs.behave()

            println("RS: " + rs.reactionGraph)

            rs.reactionGraph.order mustEqual 3
        }
    }
}


