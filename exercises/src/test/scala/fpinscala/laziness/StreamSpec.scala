package fpinscala.laziness

import org.scalatest._
import org.scalatest.matchers.should._

class StreamSpec extends FlatSpec with Matchers {

  "take" should "take the first n elements" in {
    val stream = Stream[Int](1, 2, 3, 4, 5)
    val expected = Stream[Int](1, 2, 3)
    val actual = stream.take(3)

    expected shouldEqual actual
  }

  it should "return the empty stream if n == 0" in {
    val stream = Stream[Int](1, 2, 3, 4, 5)
    val expected = Empty
    val actual = stream.take(0)

    expected shouldEqual actual
  }

  it should "return the empty stream if n < 0" in {
    val stream = Stream[Int](1, 2, 3, 4, 5)
    val expected = Empty
    val actual = stream.take(-1)

    expected shouldEqual actual
  }

  it should "return the unmodified stream if n > stream length" in {
    val stream = Stream[Int](1, 2, 3, 4, 5)
    val actual = stream.take(100)

    stream shouldEqual actual
  }

  it should "return the unmodified stream if n == stream length" in {
    val stream = Stream[Int](1, 2, 3, 4, 5)
    val actual = stream.take(5)

    stream shouldEqual actual
  }

  "drop" should "remove the first n elements" in {
    val stream = Stream[Int](1, 2, 3, 4, 5)
    val expected = Stream[Int](4, 5)
    val actual = stream.drop(3)

    expected shouldEqual actual
  }

  it should "return the unmodified stream when n is 0" in {
    val stream = Stream[Int](1, 2, 3, 4, 5)
    val actual = stream.drop(0)

    stream shouldEqual actual
  }

  it should "return the unmodified stream when n < 0" in {
    val stream = Stream[Int](1, 2, 3, 4, 5)
    val actual = stream.drop(-1)

    stream shouldEqual actual
  }

  it should "return the empty stream when n == stream length" in {
    val stream = Stream[Int](1, 2, 3, 4, 5)
    val actual = stream.drop(5)

    Empty shouldBe actual
  }

  it should "return the empty stream when n > stream length" in {
    val stream = Stream[Int](1, 2, 3, 4, 5)
    val actual = stream.drop(100)

    Empty shouldBe actual
  }

  "takeWhile" should "take the first three elements" in {
    val p = (i: Int) => i % 2 == 0
    val stream = Stream[Int](2, 4, 6, 3, 8)
    val expected = Stream[Int](2, 4, 6)
    val actual = stream.takeWhile(p)

    expected shouldEqual actual
  }

  it should "take no elements" in {
    val p = (i: Int) => i % 2 == 0
    val stream = Stream[Int](3, 8)
    val expected = Empty
    val actual = stream.takeWhile(p)

    expected shouldEqual actual
  }

  it should "take all elements" in {
    val p = (i: Int) => i % 2 == 0
    val stream = Stream[Int](2, 4, 6, 8)
    val actual = stream.takeWhile(p)

    stream shouldEqual actual
  }

  it should "return the empty stream" in {
    val p = (i: Int) => i % 2 == 0
    val stream = Empty
    val actual = stream.takeWhile(p)

    stream shouldEqual actual
  }

  "forAll" should "return true when all elements pass the predicate" in {
    val p = (i: Int) => i % 2 == 0
    val stream = Stream[Int](2, 4, 6)
    val actual = stream.forAll(p)

    true shouldBe actual
  }

  it should "return false when at least one element does not pass the predicate" in {
    val p = (i: Int) => i % 2 == 0
    val stream = Stream[Int](2, 1, 6)
    val actual = stream.forAll(p)

    false shouldBe actual
  }

  it should "return true for the empty stream" in {
    val p = (i: Int) => i % 2 == 0
    val stream = Empty
    val actual = stream.forAll(p)

    true shouldBe actual
  }

  "headOption" should "return None for the empty stream" in {
    val stream = Empty
    val actual = stream.headOption

    None shouldBe actual
  }

  it should "return Some(1) for single element stream" in {
    val stream = Stream[Int](1)
    val expected = Some(1)
    val actual = stream.headOption

    expected shouldEqual actual
  }

  it should "return Some(1) for multi-element stream" in {
    val stream = Stream[Int](1, 2, 3)
    val expected = Some(1)
    val actual = stream.headOption

    expected shouldEqual actual
  }

  "map" should "increment and convert each Int element to Double" in {
    val p = (i: Int) => (i + 1).toDouble
    val stream = Stream[Int](1, 2, 3)
    val expected = Stream[Double](2.0, 3.0, 4.0)
    val actual = stream.map(p)

    expected shouldEqual actual
  }

  it should "return the empty stream when called on the empty stream" in {
    val p = (i: Int) => i.toChar
    val stream = Empty
    val actual = stream.map(p)

    stream shouldEqual actual
  }

  "filter" should "return the unmodified stream" in {
    val p = (i: Int) => i < 100
    val stream = Stream[Int](1, 2, 3, 4, 5, 6)
    val actual = stream.filter(p)

    stream shouldEqual actual
  }

  it should "return the empty stream" in {
    val p = (i: Int) => i > 100
    val stream = Stream[Int](1, 2, 3, 4, 5, 6)
    val expected = Empty
    val actual = stream.filter(p)

    expected shouldEqual actual
  }

  it should "return odd numbers" in {
    val p = (i: Int) => i % 2 == 1
    val stream = Stream[Int](1, 2, 3, 4, 5, 6)
    val expected = Stream[Int](1, 3, 5)
    val actual = stream.filter(p)

    expected shouldEqual actual
  }

  it should "return even numbers" in {
    val p = (i: Int) => i % 2 == 0
    val stream = Stream[Int](1, 2, 3, 4, 5, 6)
    val expected = Stream[Int](2, 4, 6)
    val actual = stream.filter(p)

    expected shouldEqual actual
  }

  "append" should "return the appended stream unmodified" in {
    val stream = Empty
    val toAppend = Stream[Int](1,2,3)
    val actual = stream.append(toAppend)

    toAppend shouldEqual actual
  }

  it should "return the original stream unmodified" in {
    val stream = Stream[Int](1,2,3)
    val toAppend = Empty
    val actual = stream.append(toAppend)

    stream shouldEqual actual
  }

  it should "append the second stream" in {
    val stream = Stream[Int](1,2,3)
    val toAppend = Stream[Int](4, 5, 6)
    val expected = Stream[Int](1, 2, 3, 4, 5, 6)
    val actual = stream.append(toAppend)

    expected shouldEqual actual
  }

  "appendOne" should "append 5 to the end of stream" in {
    val stream = Stream[Int](1, 2, 3)
    val expected = Stream[Int](1, 2, 3, 4)
    val actual = stream.appendOne(4)

    expected shouldEqual actual
  }

  "ones" should "return a stream of 5 ones" in {
    val expected = Stream[Int](1, 1, 1, 1, 1)
    val actual = Stream.ones.take(5)

    expected shouldEqual actual
  }

  "as" should "return a stream of 5 'a's" in {
    val expected = Stream[Char]('a', 'a', 'a', 'a', 'a')
    val actual = Stream.as.take(5)

    expected shouldEqual actual
  }

  "from" should "return -2 to 2" in {
    val expected = Stream[Int](-2, -1, 0, 1, 2)
    val actual = Stream.from(-2).take(5)

    expected shouldEqual actual
  }

  "fibs" should "return the empty stream when n == 0" in {
    val expected = Empty
    val actual = Stream.fibs(0)

    expected shouldEqual actual
  }

  it should "return the empty stream when n < 0" in {
    val expected = Empty
    val actual = Stream.fibs(-1)

    expected shouldEqual actual
  }

  it should "return lone element stream of 0 when n == 1" in {
    val expected = Stream[Int](0)
    val actual = Stream.fibs(1)

    expected shouldEqual actual
  }

  it should "return stream(0, 1) when n == 2" in {
    val expected = Stream[Int](0, 1)
    val actual = Stream.fibs(2)

    expected shouldEqual actual
  }

  it should "return the first 8 Fibonacci numbers when n == 8" in {
    val expected = Stream[Int](0, 1, 1, 2, 3, 5, 8, 13)
    val actual = Stream.fibs(8)

    expected shouldEqual actual
  }

  "zipWith" should "return the empty stream when both are empty" in {
    val left: Stream[Int] = Empty
    val right: Stream[Int] = Empty
    val expected: Stream[Int] = Empty
    val actual = left.zipWith(right)((a, b) => a + b)

    expected shouldBe actual
  }

  it should "return the empty stream when left is empty" in {
    val left: Stream[Int] = Empty
    val right: Stream[Int] = Stream(1, 2, 3)
    val expected: Stream[Int] = Empty
    val actual = left.zipWith(right)((a, b) => a + b)

    expected shouldBe actual
  }

  it should "return the empty stream when right is empty" in {
    val left: Stream[Int] = Stream(1, 2, 3)
    val right: Stream[Int] = Empty
    val expected: Stream[Int] = Empty
    val actual = left.zipWith(right)((a, b) => a + b)

    expected shouldBe actual
  }

  it should "return new stream with elements from left and right summed" in {
    val left: Stream[Int] = Stream(1, 2, 3)
    val right: Stream[Int] = Stream(1, 2, 3)
    val expected: Stream[Int] = Stream(2, 4, 6)
    val actual = left.zipWith(right)((a, b) => a + b)

    expected shouldBe actual
  }

  "zipAll" should "return the empty stream when both streams are empty" in {
    val left = Empty
    val right = Empty
    val expected = Empty
    val actual = left.zipAll(right)

    expected shouldBe actual
  }

  it should "have None as _._2 for each tuple when the right Stream is empty" in {
    val left = Stream[Int](1, 2, 3)
    val right = Empty
    val expected = Stream((Some(1), None), (Some(2), None), (Some(3), None))
    val actual = left.zipAll(right)

    expected shouldBe actual
  }

  it should "have None as _._1 for each tuple when the left Stream is empty" in {
    val left = Empty
    val right = Stream[Int](1, 2, 3)
    val expected = Stream((None, Some(1)), (None, Some(2)), (None, Some(3)))
    val actual = left.zipAll(right)

    expected shouldBe actual
  }

  it should "never have None in any element tuple when both Streams are the same length" in {
    val left = Stream[Char]('1', '2', '3')
    val right = Stream[Int](1, 2, 3)
    val expected = Stream((Some('1'), Some(1)), (Some('2'), Some(2)), (Some('3'), Some(3)))
    val actual = left.zipAll(right)

    expected shouldBe actual
  }

  it should "have None in _._2 for each tuple after the length of the right Stream when the left is longer" in {
    val left = Stream[Char]('1', '2', '3')
    val right = Stream[Int](1)
    val expected = Stream((Some('1'), Some(1)), (Some('2'), None), (Some('3'), None))
    val actual = left.zipAll(right)

    expected shouldBe actual
  }

  it should "have None in _._1 for each tuple after the length of the left Stream when the right is longer" in {
    val left = Stream[Char]('1')
    val right = Stream[Int](1, 2, 3)
    val expected = Stream((Some('1'), Some(1)), (None, Some(2)), (None, Some(3)))
    val actual = left.zipAll(right)

    expected shouldBe actual
  }

  "startsWith" should "be true for s with 1 element" in {
    val stream = Stream[Char]('a', 'b', 'c')
    val s = Stream[Char]('a')
    val actual = stream.startsWith(s)

    true shouldBe actual
  }

  it should "be false for s with 1 element" in {
    val stream = Stream[Char]('a', 'b', 'c')
    val s = Stream[Char]('b')
    val actual = stream.startsWith(s)

    false shouldBe actual
  }

  it should "be true for s with > 1 element" in {
    val stream = Stream[Char]('a', 'b', 'c')
    val s = Stream[Char]('a', 'b')
    val actual = stream.startsWith(s)

    true shouldBe actual
  }

  it should "be false for s with > 1 element" in {
    val stream = Stream[Char]('a', 'b', 'c')
    val s = Stream[Char]('a', 'c')
    val actual = stream.startsWith(s)

    false shouldBe actual
  }

  it should "be false for empty first list" in {
    val stream = Empty
    val s = Stream[Char]('a', 'c')
    val actual = stream.startsWith(s)

    false shouldBe actual
  }

  it should "be false for empty second list" in {
    val stream = Stream[Char]('a', 'b', 'c')
    val s = Empty
    val actual = stream.startsWith(s)

    true shouldBe actual
  }

  it should "be true for both empty lists" in {
    val stream = Empty
    val s = Empty
    val actual = stream.startsWith(s)

    true shouldBe actual
  }

  it should "be false when s contains more elements" in {
    val stream = Stream[Char]('a', 'b', 'c')
    val s = Stream[Char]('a', 'b', 'c', 'd')
    val actual = stream.startsWith(s)

    false shouldBe actual
  }

  "tails" should "return the stream of suffices of the input stream" in {
    val stream = Stream[Int](1, 2, 3)
    val expected = Stream[Stream[Int]](stream, stream.drop(1), stream.drop(2), Empty)
    val actual = stream.tails

    expected shouldBe actual
  }

  it should "return a stream containing the empty stream when called on the empty stream" in {
    val stream = Empty
    val expected = Stream(Empty)
    val actual = stream.tails

    expected shouldBe actual
  }

}
