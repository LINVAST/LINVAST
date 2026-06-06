// Simple function with no parameters and no return type
fun greet() {
    return
}

// Function with a single parameter and explicit return type
fun square(x: Int): Int {
    return x * x
}

// Function with multiple parameters
fun add(x: Int, y: Int): Int {
    return x + y
}

// Function with local variable declaration
fun compute(x: Int, y: Int): Int {
    val result: Int = x + y * 2
    return result
}
