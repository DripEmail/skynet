package skynet

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
@Warmup(iterations = 4)
@Measurement(iterations = 4)
@Fork(2)
class Skynet {

  def skynetAsync1(num: Int, size: Int, div: Int): Future[Long] =
    if (size == 1) Future.successful(num)
    else Future.sequence {
      (0 until div) map (i => skynetAsync1(num + i * size / div, size / div, div))
    } map (_.sum)

  def skynetAsync2(numAndSize: Future[(Int,Int)], div: Int): Future[Long] =
    numAndSize flatMap { case (num,size) =>
      if (size == 1) Future.successful(num)
      else Future.sequence {
        (0 until div) map (i => skynetAsync2(
          numAndSize map { case (num,size) =>
            val f = size / div
            (num + i * f, f)
          }, div
        ))
      } map (_.sum)
    }

  def skynetSync(num: Int, size: Int, div: Int): Long = {
    var sum: Long = 0
    if (size > 1) {
      (0 until div).foreach(i =>
        sum = sum + skynetSync(num + i * size / div, size / div, div)
      )
      sum
    } else {
      num
    }
  }

  val initialSize = 1000000

  @Benchmark
  def skynetAsync1Bench(): Long = {
    Await.result(skynetAsync1(0, initialSize, 10), Duration.Inf)
  }

  @Benchmark
  def skynetAsync2Bench(): Long = {
    Await.result(skynetAsync2(Future.successful(0, initialSize), 10), Duration.Inf)
  }

  @Benchmark
  def skynetSyncBench(): Long = {
    skynetSync(0, initialSize, 10)
  }
}
