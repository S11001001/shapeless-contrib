package shapeless.contrib.scalaz

import shapeless.{::, HNil, Lens, Witness}
import shapeless.Lens._
import shapeless.Nat._
import shapeless.record.FieldType
import shapeless.syntax.singleton._
import shapeless.test.illTyped
import shapeless.contrib.scalacheck._

import org.specs2.scalaz.Spec

import scalaz.{Equal, Order} // XXX needed by derived Equal
import scalaz.scalacheck.ScalazProperties._

class LensTest extends Spec {

  import scalaz.std.anyVal._
  import scalaz.std.list._
  import scalaz.std.string._

  case class TwoElem(n: Int, x: String)

  val nLens = Lens[TwoElem] >> _0
  val xLens = Lens[TwoElem] >> _1

  checkAll("case class >>  _0", lens.laws(nLens.asScalaz))
  checkAll("case class >>  _1", lens.laws(xLens.asScalaz))

  case class Nested(a: Int, b: TwoElem)

  val bnLens = Lens[Nested] >> _1 >> _0
  val bxLens = Lens[Nested] >> _1 >> _1

  checkAll("nested case class >> _0", lens.laws(bnLens.asScalaz))
  checkAll("nested case class >> _1", lens.laws(bxLens.asScalaz))

  type SIL[A] = String :: Int :: List[A] :: HNil
  def silString[A] = selectLensFamily[SIL[A], SIL[A], String, String]
  def silA[A, B] = selectLensFamily[SIL[A], SIL[B], List[A], List[B]]

  checkAll("head select lens", lens.laws(silString[Int]))
  checkAll("3rd select lens", lens.laws(silA[Int, Int]))

  illTyped("selectLensFamily[SIL[Int], SIL[Int], Long, Long]")

  val (strT, numT, laT) = (Witness("str"), Witness("num"), Witness("la"))
  type RSIL[A] = ((strT.T FieldType String) :: (numT.T FieldType Int)
                  :: (laT.T FieldType List[A]) :: HNil)
/*
  def rsilString[A, B] = recordLensFamily[RSIL[A], B]("str")
  def rsilA[A, B] = recordLensFamily[RSIL[A], List[B]]("la")

  rsilString[Int, Int]
  rsilA[Int, String]

  checkAll("head record lens", lens.laws(rsilString[Int, String]))
  checkAll("3rd record lens", lens.laws(rsilA[Int, Int]))
*/

  illTyped("""recordLensFamily[RSIL[A], B]("st")""")
}

// vim: expandtab:ts=2:sw=2
