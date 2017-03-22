package cont

import scalaz.effect.IO
import scalaz.syntax.monad._

import java.util.concurrent.ExecutorService

// TODO exceptions???
object Task {

  def now[A](a: A): Task[A] = Cont.pure(a)

  def delay[A](body: => A): Task[A] = Cont(_(body))

  def async[A](f: (A => IO[Unit]) => IO[Unit]): Task[A] = Cont(f)

  def flatMap[A, B](t: Task[A])(f: A => Task[B]): Task[B] = t.flatMap(f)

  def pzip[A, B](left: Task[A], right: Task[B])(implicit E: ExecutorService): Task[(A, B)] = {
    Cont { cb =>
      IO {
        @volatile var lres: Option[A] = None
        @volatile var rres: Option[B] = None

        val notify = IO {
          val zipped = for {
            l <- lres
            r <- rres
          } yield (l, r)

          zipped.map(cb).getOrElse(IO(()))
        } join

        val leftAct = left.run(a => IO { lres = Some(a) } >> notify)
        val rightAct = right.run(b => IO { rres = Some(b) } >> notify)

        E(leftAct) >> E(rightAct)
      }
    }
  }
}
