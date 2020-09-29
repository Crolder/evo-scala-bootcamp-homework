package main.scala.basics

import java.lang.Math.{PI, abs, pow, sqrt}

object ClassesTraits extends App{
    sealed trait Shape2D[A <: Movable[A]] extends Located with Bounded with Movable[A] {
        def area: Double
    }

    sealed trait Shape3D[A <: Movable3D[A]] extends Located3D with Bounded3D with Movable3D[A] {
        def surfaceArea: Double
        def volume: Double
    }

    sealed trait Movable[A] {
        def move(dx: Double, dy: Double) : A
    }

    sealed trait Movable3D[A] {
        def move(dx: Double, dy: Double, dz: Double): A
    }

    sealed trait Located {
        def x: Double
        def y: Double
    }

    sealed trait Located3D extends Located {
        def z: Double
    }

    sealed trait Bounded {
        def minX: Double
        def maxX: Double

        def minY: Double
        def maxY: Double
    }

    sealed trait Bounded3D extends Bounded {
        def minZ: Double
        def maxZ: Double
    }

    object Origin extends Located {
        override def x: Double = 0
        override def y: Double = 0
    }

    object Origin3D extends Located3D {
        override def x: Double = 0
        override def y: Double = 0
        override def z: Double = 0
    }

    //Start of 2D shapes

    final case class Point(x: Double, y: Double) extends Shape2D[Point] {
        override def minX: Double = x
        override def maxX: Double = x

        override def minY: Double = y
        override def maxY: Double = y

        override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy )

        override def area: Double = 0
    }

    final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape2D[Circle] {
        override def x: Double = centerX
        override def y: Double = centerY

        override def minX: Double = x - radius
        override def maxX: Double = x + radius

        override def minY: Double = y - radius
        override def maxY: Double = y + radius

        override def move(dx: Double, dy: Double): Circle =  Circle(centerX + dx, centerY + dy, radius)

        override def area: Double = PI * pow(radius, 2)
    }

    final case class Rectangle(centerX: Double, centerY: Double, width: Double, height: Double) extends Shape2D[Rectangle] {
        override def x: Double = centerX
        override def y: Double = centerY

        override def minX: Double = x - width/2
        override def maxX: Double = x + width/2

        override def minY: Double = y - height/2
        override def maxY: Double = y + height/2

        override def move(dx: Double, dy: Double): Rectangle = Rectangle(centerX + dx, centerY + dy, width, height)

        override def area: Double = width * height
    }

    final case class Square(centerX: Double, centerY: Double, size: Double ) extends Shape2D[Square] {
        override def x: Double = centerX
        override def y: Double = centerY

        override def minX: Double = x - size/2
        override def maxX: Double = x + size/2

        override def minY: Double = y - size/2
        override def maxY: Double = y + size/2

        override def move(dx: Double, dy: Double): Square = Square(centerX + dx, centerY + dy, size)

        override def area: Double = size * size
    }

    final case class Triangle(point1: Point, point2: Point, point3: Point) extends Shape2D[Triangle] {
        override def x: Double = (point1.x + point2.x + point3.x)/3
        override def y: Double = (point1.y + point2.y + point3.y)/3

        override def minX: Double = List(point1.x, point2.x, point3.x).min
        override def maxX: Double = List(point1.x, point2.x, point3.x).max

        override def minY: Double = List(point1.y, point2.y, point3.y).min
        override def maxY: Double = List(point1.y, point2.y, point3.y).max

        override def move(dx: Double, dy: Double): Triangle =
            Triangle(
                Point(point1.x + dx, point1.y + dy),
                Point(point2.x + dx, point2.y + dy),
                Point(point3.x + dx, point3.y + dy)
            )

        override def area: Double = {
            val a : Double = sqrt(pow(point1.x - point2.x,2) + pow(point1.y - point2.y,2))
            val b : Double = sqrt(pow(point2.x - point3.x,2) + pow(point2.y - point3.y,2))
            val c : Double = sqrt(pow(point3.x - point1.x,2) + pow(point3.y - point1.y,2))
            val s = (a + b + c) / 2
            sqrt(s * (s - a) * (s - b) * (s - c))
        }
    }

    //End of 2D shapes

    //Start of 3D shapes

    final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D[Point3D] {
        override def minX: Double = x
        override def maxX: Double = x

        override def minY: Double = y
        override def maxY: Double = y

        override def minZ: Double = z
        override def maxZ: Double = z

        override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy, z + dz)

        override def surfaceArea: Double = 0

        override def volume: Double = 0

    }

    final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends Shape3D[Sphere] {
        override def x: Double = centerX
        override def y: Double = centerY
        override def z: Double = centerZ

        override def minX: Double = x - radius
        override def maxX: Double = x + radius

        override def minY: Double = y - radius
        override def maxY: Double = y + radius

        override def minZ: Double = z - radius
        override def maxZ: Double = z + radius

        override def move(dx: Double, dy: Double, dz: Double): Sphere =  Sphere(centerX + dx, centerY + dy, centerZ + dz, radius)

        override def surfaceArea: Double = 4 * PI * pow(radius, 2)

        override def volume: Double = PI * pow(radius, 3) * 4/3
    }

    final case class Cuboid(centerX: Double, centerY: Double, centerZ: Double, width: Double, height: Double, depth: Double) extends Shape3D[Cuboid] {
        override def x: Double = centerX
        override def y: Double = centerY
        override def z: Double = centerZ

        override def minX: Double = x - width/2
        override def maxX: Double = x + width/2

        override def minY: Double = y - height/2
        override def maxY: Double = y + height/2

        override def minZ: Double = z - depth/2
        override def maxZ: Double = z + depth/2

        override def move(dx: Double, dy: Double, dz: Double): Cuboid = Cuboid(centerX + dx, centerY + dy,centerZ + dz, width, height, depth)

        override def surfaceArea: Double = 2 * (width * height) + 2 * (height * depth) + 2 * (depth * width)

        override def volume: Double = width * height * depth
    }

    final case class Tetrahedron(point1: Point3D, point2: Point3D, point3: Point3D, point4: Point3D) extends Shape3D[Tetrahedron] {
        override def x: Double = (point1.x + point2.x + point3.x + point4.x)/4
        override def y: Double = (point1.y + point2.y + point3.y + point4.y)/4
        override def z: Double = (point1.z + point2.z + point3.z + point4.z)/4

        override def minX: Double = List(point1.x, point2.x, point3.x, point4.x).min
        override def maxX: Double = List(point1.x, point2.x, point3.x, point4.x).max

        override def minY: Double = List(point1.y, point2.y, point3.y, point4.y).min
        override def maxY: Double = List(point1.y, point2.y, point3.y, point4.y).max

        override def minZ: Double = List(point1.z, point2.z, point3.z, point4.z).min
        override def maxZ: Double = List(point1.z, point2.z, point3.z, point4.z).max

        override def move(dx: Double, dy: Double, dz: Double): Tetrahedron =
            Tetrahedron(
                Point3D(point1.x + dx, point1.y + dy, point1.z + dz),
                Point3D(point2.x + dx, point2.y + dy, point2.z + dz),
                Point3D(point3.x + dx, point3.y + dy, point3.z + dz),
                Point3D(point4.x + dx, point4.y + dy, point4.z + dz)
            )

        override def surfaceArea: Double = {
            val area: (Point3D, Point3D, Point3D) => Double = (firstPoint: Point3D, secondPoint: Point3D, thirdPoint: Point3D) => {
                val a : Double = sqrt(pow(firstPoint.x - secondPoint.x, 2) + pow(firstPoint.y - secondPoint.y,2) + pow(firstPoint.z - secondPoint.z, 2))
                val b : Double = sqrt(pow(secondPoint.x - thirdPoint.x, 2) + pow(secondPoint.y - thirdPoint.y,2) + pow(secondPoint.z - thirdPoint.z, 2))
                val c : Double = sqrt(pow(thirdPoint.x - firstPoint.x, 2) + pow(thirdPoint.y - firstPoint.y,2) + pow(thirdPoint.z - firstPoint.z, 2))
                val s = (a + b + c) / 2
                println(sqrt(s * (s - a) * (s - b) * (s - c)))
                sqrt(s * (s - a) * (s - b) * (s - c))
            }
            area(point1,point2,point3) + area(point2, point3, point4) + area(point3, point4, point1) + area(point2, point4, point1)
        }

        override def volume: Double = 0 // Matrix and determinant need to be used
    }

    final case class Cube(centerX: Double, centerY: Double, centerZ: Double, size: Double ) extends Shape3D[Cube] {
        override def x: Double = centerX
        override def y: Double = centerY
        override def z: Double = centerZ

        override def minX: Double = x - size/2
        override def maxX: Double = x + size/2

        override def minY: Double = y - size/2
        override def maxY: Double = y + size/2

        override def minZ: Double = z - size/2
        override def maxZ: Double = z + size/2

        override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(centerX + dx, centerY + dy, centerZ + dz, size)

        override def surfaceArea: Double = size * size * 6

        override def volume: Double = pow(size,3)
    }

    final case class Cone(centerX: Double, centerY: Double, centerZ: Double, radius: Double, height: Double ) extends Shape3D[Cone] {
        override def x: Double = centerX
        override def y: Double = centerY
        override def z: Double = centerZ

        override def minX: Double = x - radius
        override def maxX: Double = x + radius

        override def minY: Double = y - radius
        override def maxY: Double = y + radius

        override def minZ: Double = List(centerZ, height).min
        override def maxZ: Double = List(centerZ, height).max

        override def move(dx: Double, dy: Double, dz: Double): Cone = Cone(centerX + dx, centerY + dy, centerZ + dz, radius, height)

        override def surfaceArea: Double = {
            val l = sqrt(pow(radius, 2) + pow(height, 2))
            PI * pow(radius, 2) + PI * radius * l
        }

        override def volume: Double = (PI * pow(radius, 2) * abs(height)) / 3
    }

    final case class Cylinder(centerX: Double, centerY: Double, centerZ: Double, radius: Double, height: Double ) extends Shape3D[Cylinder] {
        override def x: Double = centerX
        override def y: Double = centerY
        override def z: Double = centerZ

        override def minX: Double = x - radius
        override def maxX: Double = x + radius

        override def minY: Double = y - radius
        override def maxY: Double = y + radius

        override def minZ: Double = List(centerZ, height).min
        override def maxZ: Double = List(centerZ, height).max

        override def move(dx: Double, dy: Double, dz: Double): Cylinder = Cylinder(centerX + dx, centerY + dy, centerZ + dz, radius, height)

        override def surfaceArea: Double = 2 * PI * pow(radius, 2) + 2 * PI * radius * height

        override def volume: Double = PI * pow(radius, 2) * height
    }

    //End of 3D shapes
}
