import scala.collection.parallel.Task

def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
  val tB: Task[B] = task {
    taskB()
  }

  val tA: A = taskA

  (tA, tB.join)
}