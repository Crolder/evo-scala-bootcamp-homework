package test.scala.basics

import java.lang.Math.{PI, pow}
import main.scala.basics.ClassesTraits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ClassesTraitsSpec extends AnyFlatSpec {

    val round: (Double, Int) => Double = (number, digits) => (number * Math.pow(10, digits)).round / Math.pow(10, digits)

    "circle area" should "be correct" in {
        Circle(1, 1, 4).area shouldEqual (PI * pow(4, 2))
    }

    "rectangle area" should "be correct" in {
        Rectangle(1, 1, 8, 4).area shouldEqual 32
    }

    "square area" should "be correct" in {
        Square(1, 1, 4).area shouldEqual 16
    }

    "triangle area" should "be correct" in {
        val point1 = Point(1,1)
        val point2 = Point(2,1)
        val point3 = Point(1,2)
        round(Triangle(point1, point2, point3).area, 2) shouldEqual 0.5
    }

    "sphere surface area & volume" should "be correct" in {
        round(Sphere(1,1,1,4).surfaceArea, 2) shouldEqual 201.06
        round(Sphere(1,1,1,4).volume, 2) shouldEqual 268.08
    }

    "cuboid surface area & volume" should "be correct" in {
        round(Cuboid(1, 1, 1, 8, 4, 6).surfaceArea, 2) shouldEqual 208
        round(Cuboid(1, 1, 1, 8, 4, 6).volume, 2) shouldEqual 192
    }

    "cube surface area & volume" should "be correct" in {
        round(Cube(1, 1, 1, 8).surfaceArea, 2) shouldEqual 384
        round(Cube(1, 1, 1, 8).volume, 2) shouldEqual 512
    }

    "cone surface area & volume" should "be correct" in {
        round(Cone(1, 1, 1, 8, 16).surfaceArea, 2) shouldEqual 650.65
        round(Cone(1, 1, 1, 8, 16).volume, 2) shouldEqual 1072.33
    }

    "cylinder surface area & volume" should "be correct" in {
        round(Cylinder(1, 1, 1, 8, 16).surfaceArea, 2) shouldEqual 1206.37
        round(Cylinder(1, 1, 1, 8, 16).volume, 2) shouldEqual 3216.99
    }
}
