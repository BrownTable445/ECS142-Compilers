package edu.ucdavis.ecs.ecs142

import edu.ucdavis.ecs.ecs142.antlr.CompilerExercisesBaseListener
import edu.ucdavis.ecs.ecs142.antlr.CompilerExercisesParser

// For functions we can just use the AST node and a map
// but for variable declarations we need to have a separate record
// that we use in numerous contexts, and have a notion of parent scopes.

// To also prevent having to carry around function info we have
// it also has the function itself.
class VariableScope (private val _parent: VariableScope? = null,
                     private val _function: Function? = null) {
    private val data = mutableMapOf<String, VariableInfo>()

    // This will guarantee the function is
    // returned by a non-null assertion.
    fun getFunction() : Function {
        if (_function != null) return _function
        return _parent!!.getFunction()
    }

    operator fun get(id: String) : VariableInfo? {
        if (id in data) {return data[id]}
        return _parent?.get(id)
    }

    operator fun contains(id: String) : Boolean {
        if (data.containsKey(id)) return true
        return _parent?.contains(id) ?: false
    }

    operator fun set(id: String, value: VariableInfo) {
        data[id] = value
    }
}


class VariableInfo(val name: String, val type: TypeDecl,
                   val global: Boolean,
                   val declaredAt: ParseLocation,
                   val register : String = generateReg()) {
    override fun toString()  = "$name: $type ($global, $declaredAt, $register)"
}

var identifiers = mutableSetOf<String>()
var labelCount = 0
/*
 * This is used for the testing infrastructure to
 * reset the set and label count
 */
fun resetIdentifiers() {
    identifiers.clear()
    labelCount = 0
}

fun registerID(id: String) {
    identifiers.add(id)
}

fun generateLabel(base: String = ""): String {
    while (base + "_LABEL_" + labelCount in identifiers) {
        labelCount++
    }
    val retval = base + "_LABEL_" + labelCount
    registerID(retval)
    return retval
}

fun generateReg(base: String = ""): String {
    while (base + "REG_" + labelCount in identifiers) {
        labelCount++
    }
    val retval = base + "REG_" + labelCount
    registerID(retval)
    return retval
}

class CEListener() : CompilerExercisesBaseListener() {
    override fun exitId(ctx: CompilerExercisesParser.IdContext) {
        registerID(ctx.start.text)
    }
}

