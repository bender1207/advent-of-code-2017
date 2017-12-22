package day20_1

import scala.collection.immutable.IndexedSeq
import scala.io.Source

object Day20_1
{
  case class Vec(x: Int, y: Int, z: Int) {
    def +(v: Vec) : Vec = Vec(this.x + v.x, this.y + v.y, this.z + v.z)
    def lengthManhattan() : Int = math.abs(x) + math.abs(y) + math.abs(z)
  }

  case class Particle(p: Vec, v: Vec, a: Vec) {
    def move() : Particle = copy(v = this.v + this.a, p = this.p + this.v + this.a)
    def distance() : Int = p.lengthManhattan()
  }

  def parseParticle(line: String) : Particle = {
    val particlePattern = """p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>""".r
    line match {
      case particlePattern(px, py, pz, vx, vy, vz, ax, ay, az) => {
        Particle(Vec(px.toInt, py.toInt, pz.toInt), Vec(vx.toInt, vy.toInt, vz.toInt), Vec(ax.toInt, ay.toInt, az.toInt))
      }
    }
  }

  def simulate(particles: IndexedSeq[Particle], counter: Int, stop: Int) : Int = {
    if (counter == stop) particles.indexOf(particles.minBy(_.distance())) else {
      simulate(particles.map(_.move()), counter + 1, stop)
    }
  }

  def computeClosestParticleOverTime(lines: Seq[String]) : Int = {
    val particles = lines.map(parseParticle).toIndexedSeq
    val minAcceleration = particles.minBy(p => p.a.lengthManhattan()).a.lengthManhattan()
    val minAccelerationParticles = particles.filter(_.a.lengthManhattan() == minAcceleration)
    val maxDistance = minAccelerationParticles.maxBy(_.p.lengthManhattan()).p.lengthManhattan()
    val index = simulate(minAccelerationParticles, 0, maxDistance)
    particles.indexOf(minAccelerationParticles(index))
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(parseParticle("p=<-10,8,12>, v=<-1212,10,4>, a=<8,0,-1>") == Particle(Vec(-10, 8, 12), Vec(-1212, 10, 4), Vec(8, 0, -1)))
      println("Tests cleared!")
    }
    //--------------------------

    // Read input
    //
    val lines: Seq[String] = Source.fromResource("day20.txt").getLines.toSeq

    println(computeClosestParticleOverTime(lines))
  }
}
