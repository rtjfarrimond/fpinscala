package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, _), 1) => Some(( h(), (empty, 0) ))
      case (Cons(h, t), i) if i > 1 => Some(( h(), (t(), i - 1) ))
      case _ => None
    }
  }

  @tailrec
  final def drop(n: Int): Stream[A] = {
    if (n <= 0) this
    else this match {
      case Cons(_, t) => t().drop(n-1)
      case _ => Empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true) {
      (a, b) => p(a) && b
    }

  def headOption: Option[A] =
    foldRight(None: Option[A]) {
      (a, _) => Some(a)
    }

  def map[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some( (f(h()), t()) )
      case _ => None
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A]) { (a, b) =>
      if (p(a)) Stream.cons(a, b)
      else b
    }

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => Stream.cons(a, b))

  def appendOne[B>:A](b: B): Stream[B] =
    append(cons(b, empty))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]) { (a, b) =>
      f(a) append b
    }

  def zipWith[B](bs: Stream[B])(f: (A, B) => B): Stream[B] =
    unfold((this, bs)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(( f(h1(), h2()), (t1(), t2()) ))
      case _ => None
    }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, bs)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
      case _ => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    this.zipAll(s).takeWhile(_._2.isDefined).forAll {
      case (h1, h2) => h1 == h2
    }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    }.append(Stream(Empty))
  }

  override def toString: String = toList.toString.replace("List", "Stream")

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Cons[A]]

  override def equals(that: Any): Boolean = {
    that match {
      case that: Cons[A] => {
        that.canEqual(this) &&
          that.h() == this.h() &&
          that.t() == this.t()
      }
      case _ => false
    }
  }
}

object Stream {

  private val Phi = (math.sqrt(5) + 1) / 2

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  val ones: Stream[Int] = constant(1)

  val as: Stream[Char] = constant('a')

  def from(n: Int): Stream[Int] =
    unfold(n - 1)(i => Some(i + 1, i + 1))

  // http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibFormula.html#section4
  private def nthFib(n: Int): Option[(Int, Int)] = {
    val PhiToTheN = math.pow(Phi, n)
    val numerator = PhiToTheN - (math.pow(-1, n) / PhiToTheN)
    val fibN = (numerator / math.sqrt(5)).toInt
    Some(fibN, n + 1)
  }

  def fibs(n: Int): Stream[Int] = {
    unfold(0)(nthFib).take(n)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map { case (a, s) =>
      cons(a, unfold(s)(f))
    }.getOrElse(Empty)
}
