package cont

import scalaz.{-\/, \/-, \/}
import scalaz.effect.IO
import scalaz.syntax.monad._

import java.util.concurrent.ExecutorService

object Task {

  def fail[A](t: Throwable): Task[A] = ContT.pure(-\/(t))

  def now[A](a: A): Task[A] = ContT.pure(\/-(a))

  def delay[A](body: => A): Task[A] = liftIO(IO(body))

  def async[A](f: (Throwable \/ A => IO[Unit]) => IO[Unit]): Task[A] = ContT(f)

  def liftIO[A](ioa: IO[A]): Task[A] = ContT.liftM(ioa.catchLeft)

  def both[A, B](left: Task[A], right: Task[B])(implicit E: ExecutorService): Task[(A, B)] = {
    ContT { cb =>
      IO {
        @volatile var failure: Option[Throwable] = None
        @volatile var lres: Option[A] = None
        @volatile var rres: Option[B] = None

        val notify = IO {
          val zipped = for {
            l <- lres
            r <- rres
          } yield \/-((l, r))

          zipped.orElse(failure.map(-\/(_))).map(cb).getOrElse(IO(()))
        } join

        val leftAct = left.run(ea => IO { ea.fold(t => failure = Some(t), a => lres = Some(a)) } >> notify)
        val rightAct = right.run(eb => IO { eb.fold(t => failure = Some(t), b => rres = Some(b)) } >> notify)

        E(leftAct) >> E(rightAct)
      }
    }
  }
}
