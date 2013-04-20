package shapeless.contrib.scalaz

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.collection.generic.CanBuildFrom

import java.util.zip.Checksum
import java.nio.ByteBuffer

import shapeless._

import scalaz._
import scalaz.std.tuple._
import scalaz.syntax.bifunctor._
import scalaz.syntax.id._
import scalaz.syntax.std.boolean._

trait Binary[A] { outer =>

  def encode(a: A): Vector[Byte]

  def decode(bytes: Vector[Byte]): Option[(A, Vector[Byte])]

  final def derive[B](iso: Iso[B, A]): Binary[B] = new Binary[B] {
    def encode(a: B) = outer encode (iso to a)
    def decode(bytes: Vector[Byte]) = outer decode bytes map { _ leftMap iso.from }
  }

  final def repeatSeq[C[X] <: Seq[X]](implicit cbf: CanBuildFrom[Nothing, A, C[A]]) = repeatColl[C[A]](identity, cbf)

  final def repeatColl[Coll](implicit f: Coll => Seq[A], cbf: CanBuildFrom[Nothing, A, Coll]): Binary[Coll] = new Binary[Coll] {

    def encode(coll: Coll) =
      (Binary[Int] encode coll.length) ++
      coll.flatMap(outer.encode)(breakOut)

    def decode(bytes: Vector[Byte]) = {
      val builder = cbf()

      @tailrec def consume(count: Int, rest: Vector[Byte]): Option[Vector[Byte]] =
        if (count > 0)
          outer decode rest match {
            case Some((a, rest)) =>
              builder += a
              consume(count - 1, rest)
            case None =>
              None
          }
        else
          Some(rest)

      Binary[Int] decode bytes flatMap { case (length, rest) =>
        if (length < 0) {
          None
        }
        else {
          builder.sizeHint(length)
          consume(length, rest) map { (builder.result, _) }
        }
      }
    }

  }

  final def withChecksum(summer: => Checksum): Binary[A] = new Binary[A] {

    private def hash(v: Vector[Byte]) = {
      val summer0 = summer
      v.foreach(b => summer0.update(b: Int))
      summer0.getValue()
    }

    private val BinaryByteVec = Binary[Byte].repeatSeq[Vector]

    def encode(a: A) = {
      val encoded = outer encode a
      val header  = Binary[Long]  encode hash(encoded)
      val payload = BinaryByteVec encode encoded
      header ++ payload
    }

    def decode(bytes: Vector[Byte]) = {
      def reconstruct(header: Long, payload: Vector[Byte]) =
        if (header == hash(payload))
          outer decode payload
        else
          None

      for {
        (header,  r0) <- Binary[Long]  decode bytes
        (payload, r1) <- BinaryByteVec decode r0
        // We discard the rest here, because there shouldn't be any
        (result,  _)  <- reconstruct(header, payload)
      } yield (result, r1)
    }

  }

}

object Binary {

  @inline def apply[A](implicit A: Binary[A]) = A

  // Ops

  case class BinaryEncodeOps[A : Binary](a: A) {
    def encode = Binary[A] encode a
  }

  case class BinaryDecodeOps(bytes: Vector[Byte]) {
    def decode[A : Binary] = Binary[A] decode bytes
    def decodeOnly[A : Binary] = Binary[A] decode bytes map { _._1 }
  }

  // Binary is a TypeClass

  implicit val BinaryInstance: TypeClass[Binary] = new TypeClass[Binary] {

    def emptyProduct = new Binary[HNil] {
      def encode(hnil: HNil) = Vector()
      def decode(bytes: Vector[Byte]) = Some(HNil, bytes)
    }

    def derive[F, G](instance: Binary[G], iso: Iso[F, G]) =
      instance derive iso

    def product[H, T <: HList](BHead: Binary[H], BTail: Binary[T]) = new Binary[H :: T] {
      def encode(hcons: H :: T) =
        (BHead encode hcons.head) ++ (BTail encode hcons.tail)
      def decode(r0: Vector[Byte]) =
        for {
          (h, r1) <- BHead decode r0
          (t, r2) <- BTail decode r1
        } yield (h :: t, r2)
    }

  }

  // Boilerplate

  implicit def deriveBinary[F, G <: HList](implicit iso: Iso[F, G], hlistInst: TypeClass.HListInstance[Binary, G]): Binary[F] =
    TypeClass.deriveFromIso[Binary, F, G]

  // Instances for data types

  private trait ByteBufferBinary[A <: AnyVal] extends Binary[A] {
    def put(a: A, buf: ByteBuffer): ByteBuffer
    def get(buf: ByteBuffer): A
    val length: Int

    def encode(a: A) =
      put(a, ByteBuffer allocate length).array().toVector

    def decode(bytes: Vector[Byte]) = {
      val (xs, rest) = bytes splitAt length
      if (xs.length < length)
        None
      else
        Some((get(ByteBuffer wrap xs.toArray), rest))
    }
  }

  implicit def ByteBinary: Binary[Byte] = new Binary[Byte] {
    def encode(byte: Byte) = Vector(byte)
    def decode(bytes: Vector[Byte]) = bytes match {
      case byte +: rest => Some((byte, rest))
      case _ => None
    }
  }

  implicit def IntBinary: Binary[Int] = new ByteBufferBinary[Int] {
    def put(n: Int, buf: ByteBuffer) = buf.putInt(n)
    def get(buf: ByteBuffer) = buf.getInt()
    val length = 4
  }

  implicit def LongBinary: Binary[Long] = new ByteBufferBinary[Long] {
    def put(n: Long, buf: ByteBuffer) = buf.putLong(n)
    def get(buf: ByteBuffer) = buf.getLong()
    val length = 8
  }

  implicit def StringBinary: Binary[String] =
    Binary[Byte].repeatColl[Array[Byte]].derive(new Iso[String, Array[Byte]] {
      def to(str: String) = str.getBytes
      def from(bytes: Array[Byte]) = new String(bytes)
    })

  implicit def ListBinary[A : Binary]: Binary[List[A]] =
    Binary[A].repeatSeq[List]

  implicit def PairBinary[A : Binary, B : Binary]: Binary[(A, B)] = new Binary[(A, B)] {

    def encode(ab: (A, B)) =
      (Binary[A] encode ab._1) ++
      (Binary[B] encode ab._2)

    def decode(r0: Vector[Byte]) =
      for {
        (a, r1) <- Binary[A] decode r0
        (b, r2) <- Binary[B] decode r1
      } yield ((a, b), r2)

  }

  implicit def EitherBinary[A : Binary, B : Binary]: Binary[A \/ B] = new Binary[A \/ B] {

    def encode(ab: A \/ B) =
      ab.fold(
        a => 0.toByte +: (Binary[A] encode a),
        b => 1.toByte +: (Binary[B] encode b)
      )

    def decode(bytes: Vector[Byte]) = Binary[Byte] decode bytes match {
      case Some((0, rest)) => Binary[A] decode rest map { _ leftMap { _.left  } }
      case Some((1, rest)) => Binary[B] decode rest map { _ leftMap { _.right } }
      case _ => None
    }

  }

}

trait BinarySyntax {

  import Binary._

  implicit def ToEncodeOps[A : Binary](a: A) = BinaryEncodeOps(a)
  implicit def ToDecodeOps(bytes: Vector[Byte]) = BinaryDecodeOps(bytes)

}

// vim: expandtab:ts=2:sw=2
