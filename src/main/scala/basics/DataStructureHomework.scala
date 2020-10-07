package main.scala.basics

object DataStructureHomework extends App{
    def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = {
        val values = map.values.toSet
        val result = values.map(value => (map.filter { case (_, v) => v == value}.keySet, value))
        result.toList.sortBy { case (_, value) => value}
    }

    val test = Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2);
    println(sortConsideringEqualValues(test))
}
