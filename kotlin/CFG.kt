package edu.ucdavis.ecs.ecs142


/*
 * The class for nodes in the control flow graph.
 * For functions we have them be the entry point,
 * and for globals there is a single CFG node entry point for
 * the global variables.
 *
 * For the entry point for a function it should have the label
 * be the function name itself, as this will be used by the simulation environment.
 * We also have to specify the argument names in the function itself, as those
 * are used for setting up the local scope in the evaluator
 */
class CFGNode(
    val label: String = generateLabel(),
    val isEntryPoint: Boolean = false,
    // For functions used to specify the arguments in order
    val args: MutableList<String> = mutableListOf(),
    val ops: MutableList<OP> = mutableListOf(),
    // This should be 0, 1, or 2; 0 for nodes ending in a
    // return, 1 for nodes that are just a destination, 2 (with the second
    // being the taken case) for branches.
    val successors: MutableList<CFGNode> = mutableListOf(),
    val predecessors: MutableList<CFGNode> = mutableListOf()
) {
    fun flatten(): String {
        return flatten(mutableSetOf<CFGNode>())
    }

    fun flatten(visited: MutableSet<CFGNode>): String {
        if (this in visited) {
            return "";
        }
        visited.add(this)
        var str = ""
        str += "$label:\n"
        ops.forEach { str += "$it\n" }
        if (successors.isNotEmpty() && successors[0] in visited) {
            str += "jump ${successors[0].label}\n"
        }
        successors.forEach { str += it.flatten(visited) }
        return str
    }

    override fun toString() : String {
        return flatten();
    }

    fun makePredecessors(visited: MutableSet<CFGNode> = mutableSetOf())  {
        if (this in visited) { return }
        visited.add(this)
        successors.forEach{
            it.predecessors.add(this)
            it.makePredecessors(visited)
        }
    }
}

// An argument to an instruction, this is subclassed
// to the actual one
abstract class InstArg() {
    open fun constant() = false;
}


class ConstArg(val constant: Int) : InstArg() {
    override fun constant() = true;
    override fun equals(other: Any?) = other is ConstArg && constant == other.constant;
    override fun toString() = "$constant"
}

class RegisterArg(val str: String) : InstArg() {
    override fun equals(other: Any?) = other is RegisterArg && str == other.str;
    override fun toString() = "$str"
}


// Within a basic block is it legal to move
// instructions across this point
abstract class OP(
    val loc: ParseLocation,
    val node: CFGNode,
) {
    // This has to be a var because we will
    // need to insert spills/unspills
    var nodeloc = node.ops.size

    init {
        node.ops.add(this)
    }

    // Can an optimizer relocate instructions across this
    // instruction?
    open fun canRelocateAcross(): Boolean = true

    // Will this operation ensure that temporaries are maintained
    // or are temporaries erased.
    open fun preservesTemporaries(): Boolean = true

    // Can this instruction only be in the last position because
    // it is a control flow operation (jump, branch, return)
    open fun onlyLast(): Boolean = false

    open fun getDead(): Boolean = _dead;
    open fun setDead(dead: Boolean) {
        _dead = dead
    }

    // These we will use to do the liveness analysis
    // for the final part (register allocation).

    val liveBefore = mutableSetOf<RegisterArg>()
    val liveAfter = mutableSetOf<RegisterArg>()

    protected var _dead = false
}

class GlobalReadOp(
    val dest: RegisterArg,
    val src: String, loc: ParseLocation,
    node: CFGNode
) : OP(loc, node) {
    override fun toString() = "$dest <-- $src"
}

// Move constants into registers first...
class GlobalWriteOp(
    val dest: String,
    val src: RegisterArg, loc: ParseLocation,
    node: CFGNode
) : OP(loc, node) {
    override fun toString() = "$src --> $dest"
}

// Calls are allowed in the middle
// of a basic block, since we don't constant propagate
// on memory, but it does disrupt registers
class CallOp(
    val dest : RegisterArg,
    val function: String,
    val args: List<RegisterArg>, loc: ParseLocation,
    node: CFGNode
) : OP(loc, node) {
    override fun canRelocateAcross() = false
    override fun preservesTemporaries() = false
    override fun toString() =
        "call $dest <- $function ${args.joinToString(" ")}"
}

//
// Make gets converted into a call to malloc so it
// does disrupt register values
class MakeOp(
    val dest: RegisterArg, val size: RegisterArg,
    loc: ParseLocation,
    node: CFGNode
) : OP(loc, node) {
    override fun canRelocateAcross() = false
    override fun preservesTemporaries() = false
    override fun toString() = "$dest <- make($size)"
}

/* We do not do constant propagation for arrays so the source
  MUST be a register.
 */
class LenOp(
    val dest: RegisterArg,
    val src: RegisterArg, loc: ParseLocation,
    node: CFGNode
) : OP(loc, node) {
    override fun toString() = "$dest <- len($src)"
}

class BinaryOp(
    val dest: RegisterArg,
    val arg1: RegisterArg,
    val arg2: RegisterArg,
    val op: String,
    loc: ParseLocation,
    node: CFGNode
) : OP(loc, node) {
    override fun toString() = "OP$op $dest $arg1 $arg2"
}

/*
 * For a branch if the result is TRUE we go to the second node in the
 * CFG otherwise we go to the first node.
 */
class BranchOp(
    val arg: RegisterArg,
    val dest: CFGNode,
    loc: ParseLocation,
    node: CFGNode,
) : OP(loc, node) {
    override fun onlyLast() = true
    override fun toString() = "branch $arg ${dest.label}"
}

class ReturnOp(
    val arg: RegisterArg, loc: ParseLocation,
    node: CFGNode
) : OP(loc, node) {
    override fun onlyLast() = true
    override fun toString() = "return $arg"
}

// We have a separate assignment for from
// register (aka copying) and from arg to simplify
// the emulator.
class AssignOpReg(
    val dest: RegisterArg,
    val arg1: RegisterArg, loc: ParseLocation,
    node: CFGNode
) : OP(loc, node) {
    override fun toString() = "$dest <- $arg1"
}

class AssignOpConst(
    val dest: RegisterArg,
    val arg1: ConstArg, loc: ParseLocation,
    node: CFGNode
) : OP(loc, node) {
    override fun toString() = "$dest <- $arg1"
}

/*
 * Offset is the array index, we don't need to scale.
 */
class MemoryRead(
    val dest: RegisterArg,
    val ptr: RegisterArg, val offset: RegisterArg, loc: ParseLocation,
    node: CFGNode
) : OP(loc, node) {
    override fun toString() = "$dest <- *($ptr + $offset)"
}

class MemoryWrite(
    val source: RegisterArg,
    val ptr: RegisterArg, val offset: RegisterArg, loc: ParseLocation,
    node: CFGNode
) : OP(loc, node) {
    override fun toString() = "*($ptr + $offset) <- $source"
}

// For spill and unspill we just use a unique string
// to indicate a unique location in the call frame.
class Spill(
    val source: RegisterArg,
    val dest: String,
    loc: ParseLocation,
    node: CFGNode
) : OP(loc, node) {
    override fun toString() = "SPILL $dest <- $source"

}

class Unspill(
    val source: String,
    val dest: RegisterArg,
    loc: ParseLocation,
    node: CFGNode
) : OP(loc, node) {
    override fun toString() = "UNSPILL $source -> $dest"
}


