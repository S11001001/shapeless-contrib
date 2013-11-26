package shapeless.contrib

import shapeless._

import org.scalacheck.{Gen, Arbitrary}

package object scalacheck {

  implicit def ArbitraryI: TypeClass[Arbitrary] = new TypeClass[Arbitrary] {

    def emptyProduct = Arbitrary(Gen.value(HNil))

    def product[H, T <: HList](h: Arbitrary[H], t: Arbitrary[T]) =
      Arbitrary(Gen.sized { size =>
        if (size == 0)
          Gen.fail
        else {
          val half = size.abs/2
          val resizedH = Gen.resize(half, h.arbitrary)
          val resizedT = Gen.resize(half, t.arbitrary)
          for { h <- resizedH; t <- resizedT }
            yield h :: t
        }})

    def coproduct[L, R <: Coproduct](l: => Arbitrary[L], r: => Arbitrary[R]) = {
      lazy val mappedL = l.arbitrary.map(Inl(_): L :+: R)
      lazy val mappedR = r.arbitrary.map(Inr(_): L :+: R)
      Arbitrary(for {
        which <- Gen.oneOf(false, true)
        result <- if (which) mappedL else mappedR
      } yield result)
    }

    def project[A, B](b: => Arbitrary[B], ab: A => B, ba: B => A) =
      Arbitrary(b.arbitrary.map(ba))

  }

  implicit def deriveArbitrary[T] = macro TypeClass.derive_impl[Arbitrary, T]

  implicit val arbHNil: Arbitrary[HNil] = ArbitraryI.emptyProduct

  implicit def arbHCons[H, T <: HList]
    (implicit h: Arbitrary[H], t: Arbitrary[T]): Arbitrary[H :: T] =
    ArbitraryI.product(h, t)

}

// vim: expandtab:ts=2:sw=2
