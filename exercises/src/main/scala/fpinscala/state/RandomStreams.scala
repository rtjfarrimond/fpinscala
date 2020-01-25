package fpinscala.state

import fpinscala.laziness.{Cons, Stream}
import Stream.constant
import fpinscala.state.RNG.{Rand, Simple}

object RandomStreams {
  def apply(seed: Long): RandomStreams = RandomStreams(Simple(seed))
}

case class RandomStreams(rng: RNG) {

  def randomStream[A](f: Rand[A]): Stream[A] = Stream.unfold(constant(rng)) {
    case Cons(h, _) =>
      val (int, rng) = f(h())
      Some((int, constant(rng)))
    case _ => None
  }

  val randomIntStream: Stream[Int] = randomStream(RNG.int)
  val randomNonNegativeIntStream: Stream[Int] = randomStream(RNG.nonNegativeInt)
  val randomDoubleStream: Stream[Double] = randomStream(RNG.double)
}
