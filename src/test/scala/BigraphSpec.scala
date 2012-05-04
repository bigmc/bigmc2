import org.specs2.mutable._
import org.specs2.specification.Scope

import org.junit.runner._
import org.specs2.runner._

import org.bigraph.bigmc._

import scala.collection.immutable.Map

@RunWith(classOf[JUnitRunner])
class BigraphSpecTest extends SpecificationWithJUnit {

	"A composite bigraph object b1 o b2" should {
		"compose the node set" in new bgtest {
			println(b1b2)
			b1b2.V.size mustEqual 4
		}
		"compose the edge set" in new bgtest {
			b1b2.E.size mustEqual 1
		}
		"compose the ctrl map" in new bgtest {
			b1b2.ctrl.size mustEqual 4
		}
		"compose the parent map for hole 0" in new bgtest {
			b1b2.prnt(x) mustEqual a
		}
		"compose the parent map for hole 1" in new bgtest {
			b1b2.prnt(y) mustEqual b
		}
		"compose the link graph" in new bgtest {
			b1b2.link.size mustEqual 2
		}
		"include a link from (a,0) to e1" in new bgtest {
			b1b2.link(new Port(a,0)) == edge1
		}
		"include a link from (x,0) to e1" in new bgtest {
			b1b2.link(new Port(x,0)) == edge1
		}
	}
}

trait bgtest extends Scope {
	val a = new Node(1)
	val b = new Node(2)
	val x = new Node(3)
	val y = new Node(4)

	val v1 = Set(a,b)
	val v2 = Set(x,y)

	val edge1 = new Edge(1)

	val e1 : Set[Edge] = Set(edge1)
	val e2 : Set[Edge] = Set()

	val ns : Set[Link] = Set()

	val ctrla = new Control("a")
	val ctrlb = new Control("b")
	val ctrlx = new Control("x")
	val ctrly = new Control("y")

	val ctrl1 = Map(a -> ctrla, b -> ctrlb)
	val ctrl2 = Map(x -> ctrlx, y -> ctrly)

	val prnt1 : Map[Place,Place] = Map(new Hole(0) -> a, new Hole(1) -> b, a -> new Region(0), b -> new Region(1))
	val prnt2 : Map[Place,Place] = Map(x -> new Region(0), y -> new Region(1))

	val link1 : Map[Link,Link] = Map(new Name("p") -> edge1, new Port(a,0) -> edge1)
	val link2 : Map[Link,Link] = Map(new Port(x,0) -> new Name("p"))

	val b1 = new Bigraph(v1,e1,ctrl1,prnt1,link1,2,ns,2,ns)
	val b2 = new Bigraph(v2,e2,ctrl2,prnt2,link2,0,ns,2,ns)

	val b1b2 = b1.compose(b2)
}

