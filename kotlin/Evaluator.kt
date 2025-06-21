package edu.ucdavis.ecs.ecs142



/*
* These are classes for values on the evaluator.  We have
* two types of data, integers and arrays, plus an UndefinedValue
* that will throw exceptions if accessed.
*/
open class EvalValue()

class IntValue(var value: Int) : EvalValue() {
    override fun toString() = "$value"
}

class ArrayValue(var size: Int) : EvalValue() {
    val values = MutableList<EvalValue>(size){ UndefinedValue()}
    override fun toString() = "[${values.joinToString(",", limit = 20, truncated = "...")}]"
}

class UndefinedValue() : EvalValue() {
    override fun toString() = "X"
}

/*
 * This is basically a lame interpreter for the control flow graph and
 * instructions in the control flow graph.
 */
class Evaluator(val globalInit: CFGNode, val functionsIn: List<CFGNode> ,
    val traceExecution : Boolean = false) {
    enum class Status {
        OK,
        RETURN,
        TAKEN,
    }


    val functions = mutableMapOf<String, CFGNode>()
    val binaryOperators = mutableMapOf<String, (EvalValue, EvalValue) -> EvalValue>()

    val cycleLimit = 50000
    var cycleCount = 0

    init {
        for (function in functionsIn) {
            functions[function.label] = function
        }

        binaryOperators["<="] = { a: EvalValue, b: EvalValue ->
            if ((a as IntValue).value <= (b as IntValue).value) IntValue(1) else IntValue(0)
        }
        binaryOperators["<"] = { a: EvalValue, b: EvalValue ->
            if ((a as IntValue).value < (b as IntValue).value) IntValue(1) else IntValue(0)
        }
        binaryOperators[">"] = { a: EvalValue, b: EvalValue ->
            if ((a as IntValue).value > (b as IntValue).value) IntValue(1) else IntValue(0)
        }
        binaryOperators[">="] = { a: EvalValue, b: EvalValue ->
            if ((a as IntValue).value >= (b as IntValue).value) IntValue(1) else IntValue(0)
        }
        binaryOperators["=="] = { a: EvalValue, b: EvalValue ->
            if ((a as IntValue).value == (b as IntValue).value) IntValue(1) else IntValue(0)
        }
        binaryOperators["!="] = { a: EvalValue, b: EvalValue ->
            if ((a as IntValue).value != (b as IntValue).value) IntValue(1) else IntValue(0)
        }


        binaryOperators["+"] = { a: EvalValue, b: EvalValue ->
            IntValue((a as IntValue).value + (b as IntValue).value)
        }
        binaryOperators["-"] = { a: EvalValue, b: EvalValue ->
            IntValue((a as IntValue).value - (b as IntValue).value)
        }
        binaryOperators["*"] = { a: EvalValue, b: EvalValue ->
            IntValue((a as IntValue).value * (b as IntValue).value)
        }
        binaryOperators["/"] = { a: EvalValue, b: EvalValue ->
            IntValue((a as IntValue).value / (b as IntValue).value)
        }

    }

    /*
     * This runs main with the specified list of arguments.
     */
    fun run(args: List<Int>): Int {
        val globals = mutableMapOf<String, EvalValue>()
        val mainArgs = ArrayValue(args.size)
        var i = 0
        for (arg in args) {
            mainArgs.values[i++] = IntValue(arg);
        }
        if (traceExecution) println("Beginning execution:")
        evaluate(globalInit, emptyList(), globals)
        val retval = evaluate(functions["main"]!!, listOf(mainArgs), globals)
        return (retval as IntValue).value
    }

/*
* And this evaluates the actual specified function with the specified arguments.
 */
    fun evaluate(node: CFGNode, args: List<EvalValue>, globals : MutableMap<String, EvalValue>): EvalValue {
        var current = node;
        var at = 0;
        val locals = mutableMapOf<String, EvalValue>()

        /*
         * The "Call frame" we use is just a map of 'register name' to value,
         * so we initialize it for the register names for the arguments.
         */
        for (i in 0 until args.size) {
            locals[current.args[i]] = args[i]
        }
        while (true) {
            cycleCount++
            if(cycleCount > cycleLimit) {
                throw Error("Exceeded execution count")
            }
            if (at >= current.ops.size) {
                // Fall off the end of the control flow block.
                // In which case we return undefined value.
                if (current.successors.size == 0) {
                    return UndefinedValue()
                }
                // Otherwise we go onto the next basic block...
                at = 0;
                current = current.successors[0]
                // Necessary because we may have empty blocks so
                // we restart the loop.
                continue
            }
            val op = current.ops[at]
            val ret = evaluate(op, globals, locals)
            if (ret.first == Status.RETURN) {
                return ret.second
            }
            if (ret.first == Status.TAKEN) {
                at = 0;
                current = current.successors[1]
            } else {
                at++
            }
        }
    }
    // This will throw ALL sorts of exceptions/errors if things were not
    // typechecked right, but that is a Feature, not a Bug (tm)
    fun evaluate(
        op: OP,
        globals: MutableMap<String, EvalValue>,
        locals: MutableMap<String, EvalValue>
    ) : Pair<Status, EvalValue> {
        if(traceExecution) println("Operation: $op")
        // Kotlin's when is like a switch statement on Russian grade
        // olympic steriods.  One useful thing it can do is evaluate based
        // on the type
        when (op) {
            is GlobalReadOp -> {
                locals[op.dest.str] = globals[op.src]!!
                if(traceExecution) println("$op ${globals[op.src]!!}")
            }

            is GlobalWriteOp -> {
                globals[op.dest] = locals[op.src.str]!!
                if(traceExecution) println("$op ${locals[op.src.str]!!}")
            }

            is AssignOpConst -> {
                locals[op.dest.str] = IntValue(op.arg1.constant)
                if(traceExecution) println("$op")
            }
            is AssignOpReg -> {
                locals[op.dest.str] = locals[op.arg1.str]!!
                if(traceExecution) println("$op ${locals[op.arg1.str]!!}")
            }

            is ReturnOp -> {
                if(traceExecution) println("$op ${locals[op.arg.str]!!}")
                return Pair(Status.RETURN, locals[op.arg.str]!!)
            }

            is CallOp -> {
                val args = MutableList(op.args.size) { i -> locals[op.args[i].str]!! }
                if(traceExecution) println("$op $op.function(${args.joinToString(", ")})")
                val ret = evaluate(functions[op.function]!!, args, globals)
                locals[op.dest.str] = ret
            }

            is BinaryOp -> {
                if (op.op !in binaryOperators) {
                    TODO("Need to implement binary operator $op")
                }
                if(traceExecution) println("$op  ${locals[op.arg1.str]} ${locals[op.arg2.str]}")
                locals[op.dest.str] = binaryOperators[op.op]!!(locals[op.arg1.str]!!, locals[op.arg2.str]!!)
            }

            is BranchOp -> {
                if(traceExecution) println("$op ${locals[op.arg.str]!!}")
                if((locals[op.arg.str]!! as IntValue).value != 0) {
                    return Pair(Status.TAKEN, locals[op.arg.str]!!)
                }
            }

            is MakeOp -> {
                if (traceExecution) println("make size ${locals[op.size.str]!!}")
                locals[op.dest.str] = ArrayValue((locals[op.size.str]!! as IntValue).value)
            }

            is MemoryWrite -> {
                val loc = locals[op.ptr.str] as ArrayValue
                val index = (locals[op.offset.str] as IntValue).value
                val data = locals[op.source.str]!!
                if (traceExecution) println("$op $loc [$index] <- $data")
                loc.values[index] = data
            }

            is MemoryRead -> {
                val loc = locals[op.ptr.str] as ArrayValue
                val index = (locals[op.offset.str] as IntValue).value
                val data = loc.values[index]
                if (traceExecution) println("$op $data <- $loc [$index]")
                locals[op.dest.str] = data
            }

            is LenOp -> {
                val array = locals[op.src.str] as ArrayValue
                val len = IntValue(array.size)
                if (traceExecution) println("$op $len")
                locals[op.dest.str] = len

            }

            is Spill -> {
                if (traceExecution) println("$op <- ${op.source} ")
                locals[op.dest] = locals[op.source.str]!!
            }
            is Unspill -> {
                if (traceExecution) println("$op -> ${op.dest} ")
                locals[op.dest.str] = locals[op.source]!!
            }

            else -> {
                TODO("Need to implement $op for ${op::class.simpleName}")
            }
        }
        return Pair(Status.OK, UndefinedValue())
    }



}