package main.scala.basics

object gcd_lcm extends App{
    val gcdTestResult: Int = 9
    val lcmTestResult: Int = 216

    def gcd (a: Int, b: Int): Int = {
        if(b == 0) a else gcd(b, a % b)
    }

    def lcm (a: Int, b: Int): Int = {
        Math.abs(a*b)/gcd(a,b)
    }

    println(s"Result of gcd function: ${gcd(81,18)}, should be: $gcdTestResult")
    println(s"Result of gcd function: ${lcm(54,24)}, should be: $lcmTestResult")
}
