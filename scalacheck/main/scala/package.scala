package shapeless.contrib

import shapeless._

import scalaz._
import scalaz.syntax.apply._
import scalaz.scalacheck.ScalaCheckBinding._

import org.scalacheck.{Gen, Arbitrary}

package object scalacheck {

  implicit def ArbitraryI: ProductTypeClass[Arbitrary] = new ProductTypeClass[Arbitrary] {

    def emptyProduct = Arbitrary(Gen.value(HNil))

    def product[F, T <: HList](f: Arbitrary[F], t: Arbitrary[T]) =
      (f |@| t) { _ :: _ }

    def project[A, B](b: => Arbitrary[B], ab: A => B, ba: B => A) =
      b.map(ba)

  }

  implicit def deriveArbitrary[T] = macro TypeClass.derive_impl[Arbitrary, T]

  implicit val arbHNil: Arbitrary[HNil] = Arbitrary(Gen.value(HNil))

  implicit def arbHCons[H, T <: HList]
    (implicit h: Arbitrary[H], t: Arbitrary[T]): Arbitrary[H :: T] =
    ^(h, t)(_ :: _)

}

// vim: expandtab:ts=2:sw=2
