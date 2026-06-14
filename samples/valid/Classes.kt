// Class with no body
class Empty

// Class with primary constructor parameters
class Point(val x: Int, val y: Int)

// Class with body: property and method
class Counter(var count: Int) {
    val step: Int = 1

    fun increment() {
        return count
    }
}

// Class with base type
class Dog : Animal

// Interface with a method
interface Printable {
    fun describe() {
    }
}
