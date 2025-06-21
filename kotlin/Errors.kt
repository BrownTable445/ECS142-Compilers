package edu.ucdavis.ecs.ecs142

// We don't use the name Error because that
// has meaning in Kotlin's type structure.

enum class CEErrorType {
    UnknownFuction,
    IncorrectArgument,
    IncompatibleTypes,
    UnknownVariable,
}

class CEError ( val type: CEErrorType, val location: ParseLocation) {
    override fun toString() = "Error: $location $type"
}