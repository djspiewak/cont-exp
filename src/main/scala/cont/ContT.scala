package cont

import scalaz.{Bind => SBind}
import scalaz.syntax.all._

sealed trait ContT[F[_], R, A] {
  import ContT._

  def map[B](f: A => B): ContT[F, R, B] = flatMap(a => ContT.pure(f(a)))

  def flatMap[B](f: A => ContT[F, R, B]): ContT[F, R, B] = Bind(this, f)

  def mapCont(f: F[R] => F[R]): ContT[F, R, A] = this match {
    case CC(k) => CC(k.andThen(f))
    case Bind(target, f2) => Bind(target, f2.andThen(_.mapCont(f)))
  }

  def withCont[B](f: (B => F[R]) => A => F[R]): ContT[F, R, B] = this match {
    case CC(k) => CC(cb => k(f(cb)))
    case Bind(target, f2) => Bind(target, f2.andThen(_.withCont(f)))
  }

  def run(f: A => F[R])(implicit F: SBind[F]): F[R] = this match {
    case CC(k) => k(f)
    case Bind(target, f2) => target.run(f2(_).run(f))
  }
}

object ContT {

  def apply[F[_], R, A](k: (A => F[R]) => F[R]): ContT[F, R, A] = CC(k)

  def pure[F[_], R, A](a: A): ContT[F, R, A] = apply(_(a))

  // this is basically the cleverest thing ever; thanks, @puffnfresh!
  def liftM[F[_]: SBind, R, A](fa: F[A]): ContT[F, R, A] = apply(fa.flatMap)

  final case class CC[F[_], R, A](k: (A => F[R]) => F[R]) extends ContT[F, R, A]
  final case class Bind[F[_], R, E, A](target: ContT[F, R, E], f: E => ContT[F, R, A]) extends ContT[F, R, A]
}
