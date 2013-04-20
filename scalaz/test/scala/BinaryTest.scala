package shapeless.contrib.scalaz

import shapeless.Iso
import shapeless.contrib.scalacheck._

import scalaz._

import scalaz.scalacheck.ScalazArbitrary._

import org.scalacheck._
import org.scalacheck.Arbitrary._

import org.specs2.matcher.ScalaCheckMatchers
import org.specs2.mutable.Specification

class BinaryTest extends Specification with ScalaCheckMatchers {

  import scalaz.std.AllInstances._

  implicit val ByteVectorArbitrary = Arbitrary(arbitrary[List[Byte]] map { _.toVector })

  def binaryLaws[A : Binary : Equal : Arbitrary](name: String) =
    name ! prop { (a: A, rest: Vector[Byte]) =>
      val encoded = Binary[A] encode a
      val decoded = Binary[A] decode (encoded ++ rest)
      Equal[Option[(A, Vector[Byte])]].equal(decoded, Some((a, rest)))
    }

  "simple instances" should {
    binaryLaws[Int]("Int")
    binaryLaws[(Int, Int)]("(Int, Int)")
    binaryLaws[Int \/ Long]("Int \\/ Long")
    binaryLaws[List[Int]]("List[Int]")
    binaryLaws[String]("String")
  }

  "case class instances" should {
    case class OneElem(n: Int)
    implicit def OneIso = Iso.hlist(OneElem.apply _, OneElem.unapply _)

    binaryLaws[OneElem]("OneElem")

    case class TwoElem(n: Int, x: String)
    implicit def TwoIso = Iso.hlist(TwoElem.apply _, TwoElem.unapply _)

    binaryLaws[TwoElem]("TwoElem")

    case class Complex(n: Int, x: TwoElem \/ String, z: List[OneElem])
    implicit def ComplexIso = Iso.hlist(Complex.apply _, Complex.unapply _)

    binaryLaws[Complex]("Complex")

    binaryLaws[Complex]("Complex + checksum")(Binary[Complex].withChecksum(new java.util.zip.CRC32), implicitly, implicitly)
  }

  "checksum" should {
    "complain when broken" ! prop { (n: Long) =>
      val binary = Binary[Long].withChecksum(new java.util.zip.CRC32)
      val encoded = binary encode n
      // let's manipulate the last byte of the checksum
      val manipulated = encoded.init :+ (encoded.last + 1).toByte
      binary decode manipulated must beNone
    }
  }

}

// vim: expandtab:ts=2:sw=2
