import org.specs2.mutable._
import org.specs2.specification.Scope

import org.junit.runner._
import org.specs2.runner._

import org.bigraph.bigmc._
import org.bigraph.bigmc.matcher._

import scala.collection.immutable.Map

@RunWith(classOf[JUnitRunner])
class MatchSpecTest extends SpecificationWithJUnit {

	"Matching G = C o R o D" should {
		"find 2 occurences" in new matchtest {
			matches.size mustEqual 2
		}
	}
}

trait matchtest extends Scope {
	val a = new Node(1)
	val b = new Node(2)
	val x = new Node(3)
	val y = new Node(4)

	val v1 = Set(a,b)
	val v2 = Set(x,y)

	val edge1 = new Edge("1")

	val e1 : Set[Edge] = Set(edge1)
	val e2 : Set[Edge] = Set()

	val ns : Set[Name] = Set()

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

	val inner1 = new Face(2,Set(new Name("p")))
	val inner2 = new Face(0,ns)
	val outer1 = new Face(2,ns)
	val outer2 = new Face(2,Set(new Name("p")))

	val b1 = new Bigraph(v1,e1,ctrl1,prnt1,link1,inner1,outer1)
	val b2 = new Bigraph(v2,e2,ctrl2,prnt2,link2,inner2,outer2)

    val matcher = new Matcher(b1,b2)
    val matches = matcher.all
}


