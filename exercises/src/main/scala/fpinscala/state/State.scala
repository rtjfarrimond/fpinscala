package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInteger, nextRng) = rng.nextInt
    nextInteger match {
      case Int.MinValue => nonNegativeInt(nextRng)
      case i => (math.abs(i), nextRng)
    }
  }

  val double: Rand[Double] = RNG.map(nonNegativeInt) { int =>
    int / (Int.MaxValue.toDouble + 1)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, firstRng) = RNG.int(rng)
    val (double, secondRng) = RNG.double(firstRng)
    ((int, double), secondRng)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (double, firstRng) = RNG.double(rng)
    val (int, secondRng) = RNG.int(firstRng)
    ((double, int), secondRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1, rng1) = RNG.double(rng)
    val (double2, rng2) = RNG.double(rng1)
    val (double3, rng3) = RNG.double(rng2)
    ((double1, double2, double3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = count match {
      case i if i > 0 =>
        val next = RNG.int(rng)
        loop(count - 1, next._2, acc :+ next._1)
      case _ => (acc, rng)
    }
    loop(count, rng, List())
  }

  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) {
      a => map(rb)(b => f(a, b))
    }

  def flatMap[A,B](s: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (nextA, nextRng) = s(rng)
      f(nextA)(nextRng)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)
  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]())) {
      (f, acc) => map2(f, acc)(_ :: _)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1)- mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
}

case class State[S,+A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (a, nextS) = run(s)
        f(a).run(nextS)
      }
    )

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  import State._

  def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, candies, _)) if candies == 0 => s
      case (Coin, s) => s.copy (locked = false)
      case (Turn, s) if s.locked => s
      case (_, s) => s.copy (locked = true, candies = s.candies - 1, coins = s.coins + 1)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def get[S]: State[S,S] = State(s => (s, s))

  def set[S](s: S): State[S,Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

}