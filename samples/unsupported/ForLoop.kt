// NOT SUPPORTED: Kotlin's for loop is a foreach/iterator construct
// and does not map to any existing LINVAST node.
// Parsing this will throw NotImplementedException.

fun printList() {
    for (x in listOf(1, 2, 3)) {
        return x
    }
}
