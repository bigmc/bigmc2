import org.specs2.mutable._
import org.specs2.specification.Scope

import org.junit.runner._
import org.specs2.runner._

import org.bigraph.bigmc._

import scala.collection.immutable.Map

@RunWith(classOf[JUnitRunner])
class BigraphSpecTest extends SpecificationWithJUnit {

	"A bigraph object" should {
		"compose the node set" in new bgtest {
			val b1b2 = b1.compose(b2)
			println(b1b2)
			b1b2.V.size mustEqual 4
		}
		"start with 'Hello'" in {
			"Hello world" must startWith("Hello")
		}
		"end with 'world'" in {
			"Hello world" must endWith("world")
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

	val e1 : Set[Edge] = Set()
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

	val link1 : Map[Link,Link] = Map()
	val link2 : Map[Link,Link] = Map()

	val b1 = new Bigraph(v1,e1,ctrl1,prnt1,link1,2,ns,2,ns)
	val b2 = new Bigraph(v2,e2,ctrl2,prnt2,link2,0,ns,2,ns)
}

