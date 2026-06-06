// if/else statement
fun max(x: Int, y: Int): Int {
    if (x > y) {
        return x
    } else {
        return y
    }
}

// if without else
fun abs(x: Int): Int {
    if (x < 0) {
        return -x
    }
    return x
}

// while loop
fun countdown(n: Int): Int {
    var x: Int = n
    while (x > 0) {
        x = x - 1
    }
    return x
}

// assignment operators
fun assignments(x: Int): Int {
    var result: Int = x
    result += 1
    result -= 1
    result *= 2
    result /= 2
    return result
}
