package fpinscala.state

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MachineSpec extends AnyFlatSpec with Matchers {

  private val machine = Machine(locked = true, candies = 5, coins = 10)

  "simulateMachine" should "complete 4 transactions" in {
    val inputs: List[Input] = List.fill(4)((Coin, Turn)).flatMap(t => Seq(t._1, t._2))

    val ((actualCoins, actualCandies), actualMachine) = Machine.simulateMachine(inputs).run(machine)

    (actualCoins, actualCandies) shouldBe ((14, 1))
    actualMachine shouldBe Machine(locked = true, actualCandies, actualCoins)
  }

}
