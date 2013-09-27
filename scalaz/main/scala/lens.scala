package shapeless.contrib.scalaz

import scalaz.LensFamily
import shapeless.HList
import shapeless.ops.hlist._

trait LensOps[A, B] {

  def asScalaz: scalaz.Lens[A, B] =
    LensFamily.lensg(asShapeless.set, asShapeless.get)

  def asShapeless: shapeless.Lens[A, B] =
    new shapeless.Lens[A, B] {
      def get(a: A): B = asScalaz.get(a)
      def set(a: A)(b: B): A = asScalaz.set(a, b)
    }

}

trait Lenses {

  implicit def scalazLensOps[A, B](l: scalaz.Lens[A, B]) = new LensOps[A, B] {
    override val asScalaz = l
  }

  implicit def shapelessLensOps[A, B](l: shapeless.Lens[A, B]) = new LensOps[A, B] {
    override val asShapeless = l
  }

  /** A variant of [shapeless.Lens]`.hlistSelectLens` that produces a
    * lens family instead.
    */
  def selectLensFamily[S <: HList, T, A, B]
      (implicit selector: Selector[S, A],
       replacer: Replacer.Aux[S, A, B, (_, T)]): LensFamily[S, T, A, B] =
    LensFamily lensFamilyu ((s, b) => replacer(s, b)._2,
                            s => selector(s))

}

// vim: expandtab:ts=2:sw=2
