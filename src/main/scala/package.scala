import scalaz.effect.IO

import java.util.concurrent.ExecutorService

package object cont {
  type Task[A] = Cont[IO[Unit], A]

  implicit final class TaskSyntax[A](val self: Task[A]) extends AnyVal {
    def flatMap[B](f: A => Task[B]): Task[B] = Task.flatMap(self)(f)
    def both[B](that: Task[B])(implicit E: ExecutorService): Task[(A, B)] = Task.both(self, that)
  }

  implicit final class ESSyntax(val executor: ExecutorService) extends AnyVal {

    def apply(effect: IO[Unit]): IO[Unit] = IO {
      executor.execute(new Runnable {
        def run() = effect.unsafePerformIO()
      })
    }
  }
}
