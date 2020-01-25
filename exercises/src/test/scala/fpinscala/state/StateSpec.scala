package fpinscala.state

import fpinscala.state.RNG.Simple
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateSpec extends AnyFlatSpec with Matchers {

  private val intState: State[RNG, Int] = State(RNG.int)
  private val seed = 42
  private val rng = Simple(seed)

  "unit" should "return 1" in {
    val expected = 1
    val actual = State.unit(expected).run(rng)._1

    actual shouldBe expected
  }

  "map" should "increment the int result" in {
    val expected = intState.run(rng)._1 + 1
    val actual = intState.map(_ + 1).run(rng)._1

    actual shouldBe expected
  }

  "map2" should "increment the int and double results" in {
    val doubleState: State[RNG, Double] = State(RNG.double)
    val (intToIncrement, nextState) = intState.run(rng)
    val expectedInt = intToIncrement + 1
    val expectedDouble = doubleState.run(nextState)._1 + 1
    val (actualInt, actualDouble) = intState.map2(doubleState) { (a, b) =>
      (a + 1, b + 1)
    }.run(rng)._1

    actualInt shouldBe expectedInt
    actualDouble shouldBe expectedDouble
  }

  "flatMap" should "increment the result" in {
    val expected = intState.run(rng)._1
    val actual = intState.flatMap(x => State.unit(x + 1)).run(rng)._1

    actual shouldBe (expected + 1)
  }

}
