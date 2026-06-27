fun printItems(items: List<String>) {
    for (x in items) {
        return x
    }
}

fun printTyped(items: List<Int>) {
    for (x: Int in items) {
        return x
    }
}

fun destructure(pairs: List<Pair<Int, Int>>) {
    for ((a, b) in pairs) {
        return a
    }
}
