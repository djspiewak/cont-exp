package cont

sealed trait Cont[R, A] {
  import Cont._

  def map[B](f: A => B): Cont[R, B] = flatMap(a => Cont.pure(f(a)))

  def flatMap[B](f: A => Cont[R, B]): Cont[R, B] = Bind(this, f)

  def mapCont(f: R => R): Cont[R, A] = this match {
    case CC(k) => CC(k.andThen(f))
    case Bind(target, f2) => Bind(target, f2.andThen(_.mapCont(f)))
  }

  def withCont[B](f: (B => R) => A => R): Cont[R, B] = this match {
    case CC(k) => CC(cb => k(f(cb)))
    case Bind(target, f2) => Bind(target, f2.andThen(_.withCont(f)))
  }

  def run(f: A => R): R = this match {
    case CC(k) => k(f)
    case Bind(target, f2) => target.run(f2(_).run(f))
  }
}

object Cont {

  def apply[R, A](k: (A => R) => R): Cont[R, A] = CC(k)

  def pure[R, A](a: A): Cont[R, A] = apply(_(a))

  final case class CC[R, A](k: (A => R) => R) extends Cont[R, A]
  final case class Bind[R, E, A](target: Cont[R, E], f: E => Cont[R, A]) extends Cont[R, A]
}
