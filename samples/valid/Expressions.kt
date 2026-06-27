// Arithmetic expressions
fun arithmetic(x: Int, y: Int): Int {
    val a: Int = x + y
    val b: Int = x - y
    val c: Int = x * y
    val d: Int = x / y
    val e: Int = x % y
    return a + b - c * d / e
}

// Logical and comparison expressions
fun logic(x: Int, y: Int): Boolean {
    val eq: Boolean = x == y
    val neq: Boolean = x != y
    val lt: Boolean = x < y
    val gt: Boolean = x > y
    val and: Boolean = x > 0 && y > 0
    val or: Boolean = x > 0 || y > 0
    return and || or
}

// Unary expressions
fun unary(x: Int): Int {
    val neg: Int = -x
    val notVal: Boolean = !true
    return neg
}

// Nested expressions
fun nested(x: Int): Int {
    return (x + 1) * (x - 1)
}
