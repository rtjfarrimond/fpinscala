package fpinscala.state

import fpinscala.state.RNG.Simple
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class RNGSpec extends AnyFlatSpec with Matchers with BeforeAndAfter {

  private val seed = -42
  private val rng = Simple(seed)

  "nonNegativeInt" should "return a non-negative integer and an RNG" in {
    val (negativeRandomInt, nextStateRng) = RNG.int(rng)
    negativeRandomInt should be < 0

    val (actualInt, actualRng) = RNG.nonNegativeInt(rng)
    actualInt shouldBe math.abs(negativeRandomInt)
    nextStateRng shouldBe actualRng
  }

  it should "always return the same int when called on the same RNG" in {
    val (firstInt, firstRng) = RNG.nonNegativeInt(rng)
    val (secondInt, secondRng) = RNG.nonNegativeInt(rng)

    firstInt shouldBe secondInt
    firstRng shouldBe secondRng
  }

  it should "return a new random int when called on a returned RNG" in {
    val (firstInt, firstRng) = RNG.nonNegativeInt(rng)
    val (secondInt, secondRng) = RNG.nonNegativeInt(firstRng)

    firstInt should not be secondInt
    firstRng should not be secondRng
  }

  "double" should "generate a random double between 0 and 1 exclusive" in {
    val actual = RNG.double(rng)._1
    actual should be > 0.0
    actual should be < 1.0
  }

  "intDouble" should "return the same int and the same double as if called independently on rng" in {
    val (expectedInt, firstRng) = RNG.int(rng)
    val (expectedDouble, expectedRng) = RNG.double(firstRng)

    val actual = RNG.intDouble(rng)
    actual._1 shouldBe (expectedInt, expectedDouble)
    actual._2 shouldBe expectedRng
  }

  "doubleInt" should "return the same double and the same int as if called independently on rng" in {
    val (expectedDouble, firstRng) = RNG.double(rng)
    val (expectedInt, expectedRng) = RNG.int(firstRng)

    val actual = RNG.doubleInt(rng)
    actual._1 shouldBe (expectedDouble, expectedInt)
    actual._2 shouldBe expectedRng
  }

  "double3" should "return a tuple2 of a tuple3 of the first three doubles and the fourth rng state" in {
    val (double1, rng1) = RNG.double(rng)
    val (double2, rng2) = RNG.double(rng1)
    val (double3, rng3) = RNG.double(rng2)
    val expected = ((double1, double2, double3), rng3)
    val actual = RNG.double3(rng)

    actual shouldBe expected
  }

  "ints" should "return a list containing the first three ints and the fourth rng state" in {
    val (int1, rng1) = RNG.int(rng)
    val (int2, rng2) = RNG.int(rng1)
    val (int3, rng3) = RNG.int(rng2)
    val expected = (List(int1, int2, int3), rng3)
    val actual = RNG.ints(3)(rng)

    actual shouldBe expected
  }

  "randIntDouble" should "yield the same values as intDouble" in {
    val expected = RNG.intDouble(rng)
    val actual = RNG.randIntDouble(rng)

    actual shouldBe expected
  }

  "randDoubleInt" should "yield the same values as doubleInt" in {
    val expected = RNG.doubleInt(rng)
    val actual = RNG.randDoubleInt(rng)

    actual shouldBe expected
  }

  "sequence" should "apply the functions in the given list" in {
    import RNG.unit
    val expected = List(1, 2, 3)
    val actual = RNG.sequence(List(unit(1), unit(2), unit(3)))(rng)._1

    actual shouldBe expected
  }

  "_ints" should "return the same value as ints" in {
    val expected = RNG.ints(3)(rng)
    val actual = RNG._ints(3)(rng)

    actual shouldBe expected
  }

  "nonNegativeLessThan" should "return a value between 0 and 5 in" in {
    val lt5s = RNG.sequence(List.fill(100)(RNG.nonNegativeLessThan(5)))(rng)._1
    lt5s.forall(_ >= 0) shouldBe true
    lt5s.forall(_ < 5) shouldBe true
  }

}