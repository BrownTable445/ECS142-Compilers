package edu.ucdavis.ecs.ecs142

import edu.ucdavis.ecs.ecs142.antlr.CompilerExercisesBaseVisitor
import edu.ucdavis.ecs.ecs142.antlr.CompilerExercisesParser
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.Token

/*
 * This is designed to be a far simpler language for a series of
 * back end compiler exercises.
 *
 * The first is AST typechecking.  There are only two
 * types in this language: integers and arrays of [x]
 * (so up to n-dimensional arrays).  In typechecking
 * we ensure that an expression is of a consistent type
 * and that assignment works.  For external linkages only the
 * base type (array or int) is considered, for internal functions
 * it requires an exact match.
 *
 * The second is converting the AST to the basic control flow graph
 * IR.  The CFG IR is in the CFG.kt file
 */

data class ParseLocation(
    val startline: Int, val startchar: Int,
    val endline: Int, val endchar: Int
) {
    override fun toString(): String {
        return "$startline:$startchar->$endline:$endchar"
    }
}

fun loc(ctx: ParserRuleContext): ParseLocation {
    return ParseLocation(
        ctx.start.line,
        ctx.start.charPositionInLine,
        ctx.stop?.line ?: ctx.start.line,
        ctx.stop?.charPositionInLine?.plus(
            (if (ctx.stop.type == Token.EOF) 0 else ctx.stop.text.length)
        ) ?: (ctx.start.charPositionInLine + ctx.start.text.length)
    )
}


abstract class ASTNode(val loc: ParseLocation) {
    /*
     * This is the function we need to override for doing
     * typechecking.  It returns a list of errors or
     * the empty list if there are no errors found.
     */
    open fun typecheck(
        functions: Map<String, Function>,
        scope: VariableScope
    ): List<CEError> {
        TODO("Need to implement for ${this::class.simpleName}")
    }

    /*
     * And this is the function for internal code
     * generation.  It accepts a CFG node which it
     * may add onto and returns a final CFG node (which
     * may be the same node or may be different) that
     * should be the next thing added onto
     */
    open fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): Any {
        TODO("Need to implement")
    }


}

class CEProgram(val topStatements: List<TopLevelStatement>, loc: ParseLocation) : ASTNode(loc) {
    override fun toString() = "${topStatements.joinToString("\n")} /* span $loc */"

    // The top level typechecking function.
    fun typecheck(): List<CEError> {
        val errors = mutableListOf<CEError>()
        /*
         * There needs to be a first pass to put the functions into
         * a function map,
         * a second pass that typechecks the global variable declarations
         * and a third pass that typechecks all the functions.
         */
        val functionMap = mutableMapOf<String, Function>()
        topStatements.forEach {
            if (it is Function) {
                functionMap[it.name] = it
            }
        }
        val globalScope = VariableScope(null)
        topStatements.forEach {
            if (it is GlobalDecl) {
                val globalScopeError = it.typecheck(functionMap, globalScope) //we set the global variables into scope in the typecheck
                errors += globalScopeError
            }
        }
        topStatements.forEach {
            if (it is Function) {
                val functionScope = VariableScope(globalScope)
                errors += it.typecheck(functionMap, functionScope) //not sure if I need to add a functionScope
            }
        }
        return errors
    }

    /*
     * This generates the intermediate representation form for
     * the entire program.  It actually provides both a single
     * CFG node for the global initializers and a list of
     * CFG nodes as the entry point to each function.
     *
     */
    fun cgen(): Pair<CFGNode, List<CFGNode>> {
        val globals = CFGNode(
            generateLabel("Globals"),
            true
        )
        var globalsAt = globals
        val functions = mutableListOf<CFGNode>()
        val globalVarNames = mutableMapOf<String, RegisterArg>()

        // First pass: collect global variable names
        topStatements.forEach {
            if (it is GlobalDecl) {
                globalVarNames[it.id] = RegisterArg(generateReg())
            }
        }

        // Second pass: process statements
        topStatements.forEach {
            if (it is GlobalDecl) {
                globalsAt = it.cgen(globalsAt, globalVarNames, mutableMapOf()) as CFGNode
            }
            if (it is Function) {
                val function = CFGNode(it.name, true)
                functions.add(function)
                it.cgen(function, globalVarNames, mutableMapOf())
            }
        }

        // Set up predecessors for proper CFG structure
        functions.forEach { it.makePredecessors() }
        globals.makePredecessors()

        return Pair(globals, functions)
    }

}

abstract class TopLevelStatement(loc: ParseLocation) : ASTNode(loc) {

}

class Function(
    val retType: TypeDecl, val name: String,
    val args: List<Arg>,
    val stmts: List<Stmt>, loc: ParseLocation
) : TopLevelStatement(loc) {
    override fun toString() =
        "$retType $name (${args.joinToString(", ")}) { ${stmts.joinToString("\n")} }"

    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        val errors = mutableListOf<CEError>()
        val functionScope = VariableScope(scope)
        args.forEach {
            functionScope.set(it.id, VariableInfo(it.id, it.type, false, it.loc))
        }
        stmts.forEach {
            errors += it.typecheck(functions, functionScope)
        }
        return errors
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): Any {
        args.forEach {
            node.args.add(it.id)
            locals[it.id] = RegisterArg(it.id)
        } //assuming that passed in node is an entry node
        var atNode = node
        stmts.forEach {
            val nextNode = it.cgen(atNode, globals, locals) as CFGNode
            if (nextNode != atNode) {
                atNode.successors.add(nextNode)
            }
            atNode = nextNode
        }
        return node
    }
}

/*
 * If there is an error in typechecknig the global variable is not created
 */
class GlobalDecl(
    val id: String, val expression: Expr,
    var inferredType: TypeDecl, loc: ParseLocation
) : TopLevelStatement(loc) {
    override fun toString() = "$id := $expression ; /* $inferredType */ "

    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        val errors = expression.typecheck(functions, scope)
        if (errors.isNotEmpty()) {
            return errors
        }
        inferredType = expression.getType()
        scope.set(id, VariableInfo(id, inferredType, true, loc))
        return mutableListOf()
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): Any {
        val exprReg = expression.cgen(node, globals, locals) as RegisterArg
        GlobalWriteOp(id, exprReg, loc, node)
        return node
    }
}

/*
 * TypeDecls themselves don't have a parse
 * location in order to simplify equality, they have JUST
 * what type it is.
 */
abstract class TypeDecl {
}

class ArrayType(val type: TypeDecl) : TypeDecl() {
    override fun toString() = "array[$type]"
    override fun equals(other: Any?): Boolean {
        if (other !is ArrayType) return false
        return this.type == other.type
    }

    override fun hashCode(): Int {
        return 4 + type.hashCode()
    }
}

class IntType() : TypeDecl() {
    override fun toString() = "int"
    override fun equals(other: Any?): Boolean {
        if (other !is IntType) return false
        return true
    }

    override fun hashCode(): Int {
        return 1
    }
}

// We use this as a placeholder in cases where we don't know the actual type.
class UnknownType() : TypeDecl() {
    override fun toString() = "?"
    override fun equals(other: Any?): Boolean {
        if (other !is UnknownType) return false
        return true
    }

    override fun hashCode(): Int {
        return 2
    }
}

class Arg(val type: TypeDecl, val id: String, loc: ParseLocation) : ASTNode(loc) {
    override fun toString() = "$type $id"
}

abstract class Stmt(loc: ParseLocation) : ASTNode(loc) {

}

class ExprStmt(val expr: Expr, loc: ParseLocation) : Stmt(loc) {
    override fun toString() = "$expr ;"
    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        return expr.typecheck(functions, scope)
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): Any {
        expr.cgen(node, globals, locals)
        return node
    }
}

/*
 * Again, if there is an error in the expression the variable
 * is never created since we don't know the type and it would cause
 * cascading errors anyway
 */
class AssignmentStmt(val assign: LHS, val expr: Expr, loc: ParseLocation) : Stmt(loc) {
    override fun toString() = "$assign := $expr ;\n"

    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        val errors = mutableListOf<CEError>()
        errors += expr.typecheck(functions, scope)
        if (assign is ArrayLHS) {
            errors += assign.typecheck(functions, scope)
        }
        if (errors.isNotEmpty()) {
            return errors
        }
        if (assign is IdLHS) {
            scope.set(assign.id, VariableInfo(assign.id, expr.getType(), false, loc))
        } else if (assign is ArrayLHS && assign.type != expr.getType()) { //guaranteed
            return mutableListOf(CEError(CEErrorType.IncompatibleTypes, loc))
        }
        return mutableListOf()
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): CFGNode {
        val exprReg = expr.cgen(node, globals, locals) as RegisterArg

        if (assign is IdLHS) {
            if (assign.id in globals) {
                globals[assign.id] = exprReg
                GlobalWriteOp(assign.id, exprReg, loc, node)
            } else {
                if (assign.id !in locals) {
                    locals[assign.id] = exprReg
                } else {
                    AssignOpReg(locals[assign.id]!!, exprReg, loc, node)
                }
            }
        } else if (assign is ArrayLHS) {
            val arrayReg = assign.array.cgen(node, globals, locals) as RegisterArg
            val indexReg = assign.index.cgen(node, globals, locals) as RegisterArg
            MemoryWrite(exprReg, arrayReg, indexReg, loc, node)
        }

        return node
    }

}

abstract class LHS(loc: ParseLocation) : ASTNode(loc) {}

class ArrayLHS(
    val array: Expr, val index: Expr, loc: ParseLocation
) : LHS(loc) {
    var type: TypeDecl = UnknownType()
    override fun toString() = "$array[$index] "
    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        val errors = mutableListOf<CEError>()
        errors += array.typecheck(functions, scope)
        errors += index.typecheck(functions, scope)
        if (errors.isNotEmpty()) {
            return errors
        }
        if (array.getType() !is ArrayType) {
            return mutableListOf(CEError(CEErrorType.IncompatibleTypes, loc))
        }
        if (index.getType() != IntType()) {
            return mutableListOf(CEError(CEErrorType.IncompatibleTypes, loc))
        }
        if (array.getType() is ArrayType) { //guaranteed
            type = (array.getType() as ArrayType).type
        }
        return mutableListOf()
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): Any {
        val arrayReg = array.cgen(node, globals, locals) as RegisterArg
        val indexReg = index.cgen(node, globals, locals) as RegisterArg
        val exprReg = RegisterArg(generateReg())
        MemoryWrite(exprReg, arrayReg, indexReg, loc, node)
        return node //return exprreg instead?
    }
}

class IdLHS(
    val id: String, loc: ParseLocation
) : LHS(loc) {
    override fun toString() = id
}


class ForStatement(
    val expr: Expr, val stmts: List<Stmt>,
    loc: ParseLocation
) : Stmt(loc) {
    override fun toString() = "for ( $expr ) { ${stmts.joinToString("\n")} }"
    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        val errors  = mutableListOf<CEError>()
        errors += expr.typecheck(functions, scope)
        val blockScope = VariableScope(scope)
        stmts.forEach {
            errors += it.typecheck(functions, blockScope)
        }
        if (errors.isNotEmpty()) {
            return errors
        }
        if (expr.getType() != IntType()) {
            return mutableListOf(CEError(CEErrorType.IncompatibleTypes, loc))
        }
        return mutableListOf()
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): Any {
        //when we enter the for statement, we have a new scope
        val localsCopy = locals.toMutableMap()

        val forNode = CFGNode(generateLabel("ForStatement"))
        node.successors.add(forNode)

        val condReg = expr.cgen(forNode, globals, localsCopy) as RegisterArg
        val afterNode = CFGNode(generateLabel("AfterFor"))
        forNode.successors.add(afterNode)
        val bodyNode = CFGNode(generateLabel("ForBody"))
        forNode.successors.add(bodyNode)
        BranchOp(condReg, bodyNode, loc, forNode)
        var curNode = bodyNode
        stmts.forEach {
            val statementNode = it.cgen(bodyNode, globals, localsCopy) as CFGNode
            if (statementNode != curNode) {
                curNode.successors.add(it.cgen(bodyNode, globals, localsCopy) as CFGNode)
            }
            curNode = statementNode
        }
        curNode.successors.add(forNode) // Loop back
        return afterNode
    }
}

class ReturnStatement(
    val expr: Expr, loc: ParseLocation
) : Stmt(loc) {
    override fun toString() = "return $expr ;"
    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        return expr.typecheck(functions, scope)
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): CFGNode {
        val ret = expr.cgen(node, globals, locals) as RegisterArg
        ReturnOp(ret, loc, node)
        return node
    }
}


class IfStatement(
    val expr: Expr, val stmts: List<Stmt>,
    val elifClauses: List<ElseIfStatement>, val elseClause: ElseStatement?, loc: ParseLocation
) : Stmt(loc) {
    override fun toString() =
        """if ($expr) 
  {${stmts.joinToString("\n")}} 
 ${elifClauses.joinToString("\n")}
 ${elseClause?.toString() ?: ""}"""

    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        val errors = mutableListOf<CEError>()
        errors += expr.typecheck(functions, scope)
        val blockscope = VariableScope(scope)
        stmts.forEach {
            errors += it.typecheck(functions, blockscope)
        }
        val blockscope2 = VariableScope(scope)
        elifClauses.forEach {
            errors += it.typecheck(functions, blockscope2)
        }
        if (elseClause != null) {
            errors += elseClause.typecheck(functions, VariableScope(scope))
        }
        if (errors.isNotEmpty()) {
            return errors
        }
        if (expr.getType() != IntType()) {
            return mutableListOf(CEError(CEErrorType.IncompatibleTypes, loc))
        }
        return mutableListOf()
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): Any {
        /*
        we need to make the sucessor of the not-taken branch the successor of the if statement
        this should be done outside this cgen(), perhaps in Function's cgen()???
         */
        val ifNode = CFGNode(generateLabel("IfStatement"))
        val condReg = expr.cgen(ifNode, globals, locals) as RegisterArg
        val afterNode = CFGNode(generateLabel("AfterIf"))
        val thenNode = CFGNode(generateLabel("Then"))
        var elseNode: CFGNode? = null
        if (elseClause != null) {
            elseNode = elseClause.cgen(node, globals, locals) as CFGNode
        }
        BranchOp(condReg, thenNode, loc, ifNode)

        var curNode = ifNode

        if (elifClauses.isNotEmpty()) {
            //Before we can cgen(), we need to have the alternative branch CFG Node
            val elifNodes = elifClauses.map { CFGNode(generateLabel("ElseIf")) }
            elifNodes.forEach {
                curNode.successors.add(it)
                curNode = it
            }
            if (elseNode != null) {
                //do something with else clause here, maybe cgen
                curNode.successors.add(elseNode)
            } else {
                curNode.successors.add(afterNode)
            }

            for (i in elifClauses.indices) {
                elifNodes[i].successors.add(elifClauses[i].cgen(elifNodes[i], globals, locals) as CFGNode) //else if cgen() returns the taken branch node of the else if statement
            }
        } else if (elseNode != null) { //specifically for when there are no else if statements
            ifNode.successors.add(elseNode)
        } else { //no else if statements and no else statements
            ifNode.successors.add(afterNode)
        }

        elseNode?.successors?.add(afterNode)

        ifNode.successors.add(thenNode)
        curNode = thenNode
        stmts.forEach {
            val tempNode = it.cgen(curNode, globals, locals) as CFGNode
            /*
            it should be okay if we add a return statement successor
            in the code that the prof provided, we have a return and then a jump back
            but the jump should never execute because of the return
             */
            if (it !is ReturnStatement) {
                curNode.successors.add(tempNode)
                curNode = tempNode
            }
        }
        node.successors.add(ifNode)
        return afterNode
    }
}

class ElseIfStatement(
    val expr: Expr, val stmts: List<Stmt>,
    loc: ParseLocation
) : ASTNode(loc) {
    override fun toString() = "else if ($expr) { ${stmts.joinToString("\n")} }"

    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        // Keeping this one in the skeleton code as an example...
        val errors = mutableListOf<CEError>()
        errors += expr.typecheck(functions, scope)
        if (expr.getType() != IntType()) {
            errors.add(CEError(CEErrorType.IncompatibleTypes, loc))
        }
        val blockscope = VariableScope(scope)
        stmts.forEach {
            errors += it.typecheck(functions, blockscope)
        }
        return errors
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): Any {
        val condReg = expr.cgen(node, globals, locals) as RegisterArg
        val bodyNode = CFGNode(generateLabel("ElseIfBody"))
        BranchOp(condReg, bodyNode, loc, node)
        var curNode = bodyNode
        stmts.forEach {
            val tempNode = it.cgen(curNode, globals, locals) as CFGNode
            curNode.successors.add(tempNode)
            curNode = tempNode
        }
        return bodyNode
    }
}

class ElseStatement(val stmts: List<Stmt>, loc: ParseLocation) : ASTNode(loc) {
    override fun toString() = "else { ${stmts.joinToString("\n")} }"

    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        val errors = mutableListOf<CEError>()
        stmts.forEach {
            errors += it.typecheck(functions, scope)
        }
        return errors
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): Any {
        val bodyNode = CFGNode(generateLabel("ElseBody"))
        var curNode = bodyNode
        stmts.forEach {
            val tempNode = it.cgen(curNode, globals, locals) as CFGNode
            curNode.successors.add(tempNode)
            curNode = tempNode
        }
        return bodyNode
    }
}

/*
 * For typechecking expressions we not only return errors but set the type.
 * A typechecked expression SHOULD never have an UnknownType as a result
 *
 * We also don't add new errors if the subexpression returned an error
 * in order to limit the cascading error issue.
 */
abstract class Expr(loc: ParseLocation) : ASTNode(loc) {
    protected var _type: TypeDecl = UnknownType()
    fun getType(): TypeDecl = _type
}

class ExprId(val id: String, loc: ParseLocation) : Expr(loc) {
    // Set this if it is a global variable because well,
    // code generation into the IR is different...
    var global: Boolean = false
    override fun toString() = " $id /* $global */ "

    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        if (scope.contains(id)) {
            _type = scope.get(id)!!.type
        } else {
            return mutableListOf(CEError(CEErrorType.UnknownVariable, loc))
        }
        return mutableListOf()
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): RegisterArg {
        //im assuming that the variable must have been declared globally or locally
        if (id in globals) {
            GlobalReadOp(globals[id]!!, id, loc, node)
            return globals[id]!!
        }
        return locals[id]!!
    }
}

class ExprBinary(val lhs: Expr, val op: String, val rhs: Expr, loc: ParseLocation) : Expr(loc) {
    override fun toString() = "($lhs $op $rhs)"

    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        val errors = mutableListOf<CEError>()
        errors += lhs.typecheck(functions, scope)
        errors += rhs.typecheck(functions, scope)
        if (errors.isNotEmpty()) {
            return errors
        }
        if (lhs.getType() != rhs.getType()) {
            return mutableListOf(CEError(CEErrorType.IncompatibleTypes, loc))
        }
        _type = lhs.getType() //what if rhs type != lhs type
        return mutableListOf()
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): RegisterArg {
        val leftReg = lhs.cgen(node, globals, locals) as RegisterArg
        val rightReg = rhs.cgen(node, globals, locals) as RegisterArg
        val resultReg = RegisterArg(generateReg())

        BinaryOp(resultReg, leftReg, rightReg, op, loc, node)
        return resultReg
    }
}

class ExprArray(val array: Expr, val index: Expr, loc: ParseLocation) : Expr(loc) {
    override fun toString() = "($array[$index])"

    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        val errors = mutableListOf<CEError>()
        errors += array.typecheck(functions, scope)
        errors += index.typecheck(functions, scope)
        if (errors.isNotEmpty()) {
            return errors
        }
        if (index.getType() != IntType() || array.getType() !is ArrayType) {
            return mutableListOf(CEError(CEErrorType.IncompatibleTypes, loc))
        }
        val type = array.getType()
        if (type is ArrayType) {
            _type = type.type
        }
        return mutableListOf()
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): Any {
        val arrayReg = array.cgen(node, globals, locals) as RegisterArg
        val indexReg = index.cgen(node, globals, locals) as RegisterArg
        val resultReg = RegisterArg(generateReg())
        MemoryRead(resultReg, arrayReg, indexReg, loc, node)
        return resultReg
    }
}

class ExprConst(val constant: Int, loc: ParseLocation) : Expr(loc) {
    override fun toString() = " $constant "
    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        _type = IntType()
        return emptyList()
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): Any {
        val resultReg = RegisterArg(generateReg())
        AssignOpConst(resultReg, ConstArg(constant), loc, node)
        return resultReg
    }
}

class ExprAlloc(val datatype: TypeDecl, val size: Expr, loc: ParseLocation) : Expr(loc) {
    override fun toString() = "make array[$datatype]($size)"

    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        val errors = size.typecheck(functions, scope)
        if (errors.isNotEmpty()) {
            return errors
        }
        if (size.getType() != IntType()) {
            return mutableListOf(CEError(CEErrorType.IncompatibleTypes, loc))
        }
        _type = ArrayType(datatype);
        return mutableListOf()
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): Any {
        val sizeReg = size.cgen(node, globals, locals) as RegisterArg
        val resultReg = RegisterArg(generateReg())
        MakeOp(resultReg, sizeReg, loc, node)
        return resultReg
    }
}

class ExprLen(val array: Expr, loc: ParseLocation) : Expr(loc) {
    override fun toString() = "len($array)"

    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        val errors = array.typecheck(functions, scope)
        if (errors.isNotEmpty()) {
            return errors
        }
        if (array.getType() !is ArrayType) {
            return mutableListOf(CEError(CEErrorType.IncompatibleTypes, loc))
        }
        _type = IntType()
        return mutableListOf()
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): Any {
        val arrayReg = array.cgen(node, globals, locals) as RegisterArg
        val resultReg = RegisterArg(generateReg())
        LenOp(resultReg, arrayReg, loc, node)
        return resultReg
    }
}

class ExprCall(val id: String, val args: List<Expr>, loc: ParseLocation) : Expr(loc) {
    override fun toString() = "$id (${args.joinToString(", ")})"
    override fun typecheck(functions: Map<String, Function>, scope: VariableScope): List<CEError> {
        val errors = mutableListOf<CEError>()
        args.forEach {
            errors += it.typecheck(functions, scope)
        }
        if (errors.isNotEmpty()) {
            return errors
        }
        if (!functions.containsKey(id)) {
            return mutableListOf(CEError(CEErrorType.UnknownFuction, loc))
        }
        if (functions[id]!!.args.size != args.size) {
            return mutableListOf(CEError(CEErrorType.IncorrectArgument, loc))
        }
        for (i in args.indices) {
            if (functions[id]!!.args[i].type != args[i].getType()) {
                return mutableListOf(CEError(CEErrorType.IncorrectArgument, loc))
            }
        }
        return mutableListOf()
    }

    override fun cgen(node: CFGNode, globals: MutableMap<String, RegisterArg>, locals: MutableMap<String, RegisterArg>): Any {
        val argRegs = args.map { it.cgen(node, globals, locals) as RegisterArg }
        val resultReg = RegisterArg(generateReg())
        CallOp(resultReg, id, argRegs, loc, node)
        return resultReg
    }
}

// And the visitor
@Suppress("unchecked_cast")
class CEVisitor : CompilerExercisesBaseVisitor<Any>() {
    override fun visitProgram(ctx: CompilerExercisesParser.ProgramContext): Any {
        val topStatements = mutableListOf<TopLevelStatement>()
        for (c in ctx.children) {
            topStatements.add(visit(c) as TopLevelStatement)
        }
        return CEProgram(topStatements, loc(ctx))
    }

    // topLevelStatement : type id '(' args ')' stmts #functionDefn
    override fun visitFunctionDefn(ctx: CompilerExercisesParser.FunctionDefnContext): Any {
        val rettype = visit(ctx.getChild(0)) as TypeDecl
        val id = visit(ctx.getChild(1)) as String
        val args = visit(ctx.getChild(3)) as List<Arg>
        val stmts = visit(ctx.getChild(5)) as List<Stmt>
        return Function(rettype, id, args, stmts, loc(ctx))
    }

    override fun visitGlobalVar(ctx: CompilerExercisesParser.GlobalVarContext): Any {
        return GlobalDecl(
            visit(ctx.getChild(0)) as String,
            visit(ctx.getChild(2)) as Expr,
            UnknownType(),
            loc(ctx)
        )
    }

    override fun visitId(ctx: CompilerExercisesParser.IdContext): Any {
        return ctx.text
    }

    override fun visitArgsEmpty(ctx: CompilerExercisesParser.ArgsEmptyContext): Any {
        return mutableListOf<Arg>()
    }

    override fun visitArgsNonEmpty(ctx: CompilerExercisesParser.ArgsNonEmptyContext): Any {
        return visit(ctx.getChild(0)) as List<Arg>
    }

    override fun visitArglist(ctx: CompilerExercisesParser.ArglistContext): Any {
        if (ctx.childCount > 2) {
            val ret = visit(ctx.getChild(0)) as MutableList<Arg>
            ret.add(
                Arg(
                    visit(ctx.getChild(2)) as TypeDecl,
                    visit(ctx.getChild(3)) as String, loc(ctx)
                )
            )
            return ret
        }
        return mutableListOf<Arg>(
            Arg(
                visit(ctx.getChild(0)) as TypeDecl,
                visit(ctx.getChild(1)) as String, loc(ctx)
            )
        )
    }

    override fun visitTypeInt(ctx: CompilerExercisesParser.TypeIntContext): Any {
        return IntType()
    }

    override fun visitTypeArray(ctx: CompilerExercisesParser.TypeArrayContext): Any {
        return ArrayType(visit(ctx.getChild(2)) as TypeDecl)
    }

    override fun visitStmtsEmpty(ctx: CompilerExercisesParser.StmtsEmptyContext): Any {
        return mutableListOf<Stmt>()
    }

    override fun visitStmtsNonEmpty(ctx: CompilerExercisesParser.StmtsNonEmptyContext): Any {
        val ret = mutableListOf<Stmt>()
        for (i in 1..<(ctx.childCount - 1)) {
            ret.add(visit(ctx.getChild(i)) as Stmt)
        }
        return ret
    }

    override fun visitAssignStatement(ctx: CompilerExercisesParser.AssignStatementContext): Any {
        return AssignmentStmt(
            visit(ctx.getChild(0)) as LHS,
            visit(ctx.getChild(2)) as Expr,
            loc(ctx)
        )
    }

    override fun visitIfStatement(ctx: CompilerExercisesParser.IfStatementContext): Any {
        val elseClause = if (ctx.childCount > 6) visit(ctx.getChild(6)) as ElseStatement else null
        val elifClauses = visit(ctx.getChild(5)) as List<ElseIfStatement>
        return IfStatement(
            visit(ctx.getChild(2)) as Expr,
            visit(ctx.getChild(4)) as List<Stmt>,
            elifClauses,
            elseClause,
            loc(ctx)
        )
    }

    override fun visitForStatement(ctx: CompilerExercisesParser.ForStatementContext): Any {
        return ForStatement(
            visit(ctx.getChild(2)) as Expr,
            visit(ctx.getChild(4)) as List<Stmt>,
            loc(ctx)
        )
    }

    override fun visitReturnStatement(ctx: CompilerExercisesParser.ReturnStatementContext): Any {
        return ReturnStatement(
            visit(ctx.getChild(1)) as Expr,
            loc(ctx)
        )
    }

    override fun visitExprStatement(ctx: CompilerExercisesParser.ExprStatementContext): Any {
        return ExprStmt(
            visit(ctx.getChild(0)) as Expr,
            loc(ctx)
        )
    }

    override fun visitIdentifierLHS(ctx: CompilerExercisesParser.IdentifierLHSContext): Any {
        return IdLHS(ctx.text, loc(ctx))
    }

    override fun visitArrayLHS(ctx: CompilerExercisesParser.ArrayLHSContext): Any {
        return ArrayLHS(
            visit(ctx.getChild(0)) as Expr,
            visit(ctx.getChild(2)) as Expr,
            loc(ctx)
        )
    }

    override fun visitElifEmpty(ctx: CompilerExercisesParser.ElifEmptyContext): Any {
        return mutableListOf<ElseIfStatement>()
    }

    override fun visitElifNonEmpty(ctx: CompilerExercisesParser.ElifNonEmptyContext): Any {
        val remaining = visit(ctx.getChild(6)) as List<ElseIfStatement>
        val ret = listOf(
            ElseIfStatement(
                visit(ctx.getChild(3)) as Expr,
                visit(ctx.getChild(5)) as List<Stmt>,
                loc(ctx)
            )
        )
        return ret + remaining
    }

    override fun visitElseNonEmpty(ctx: CompilerExercisesParser.ElseNonEmptyContext): Any {
        return ElseStatement(
            visit(ctx.getChild(1)) as List<Stmt>,
            loc(ctx)
        )
    }

    override fun visitExprInt(ctx: CompilerExercisesParser.ExprIntContext): Any {
        return ExprConst(ctx.text.toInt(), loc(ctx))
    }

    override fun visitExprCall(ctx: CompilerExercisesParser.ExprCallContext): Any {
        if (ctx.childCount < 4) {
            return ExprCall(
                ctx.getChild(0).text,
                emptyList(), loc(ctx)
            )
        }
        return ExprCall(
            ctx.getChild(0).text,
            visit(ctx.getChild(2)) as List<Expr>,
            loc(ctx)
        )
    }

    override fun visitExprBinary(ctx: CompilerExercisesParser.ExprBinaryContext): Any {

        return ExprBinary(
            visit(ctx.getChild(0)) as Expr,
            ctx.getChild(1).text,
            visit(ctx.getChild(2)) as Expr,
            loc(ctx)
        )
    }

    override fun visitExprAlloc(ctx: CompilerExercisesParser.ExprAllocContext): Any {
        return ExprAlloc(
            visit(ctx.getChild(3)) as TypeDecl,
            visit(ctx.getChild(6)) as Expr,
            loc(ctx)
        )
    }

    override fun visitExprId(ctx: CompilerExercisesParser.ExprIdContext): Any {
        return ExprId(ctx.text, loc(ctx))
    }

    override fun visitExprLen(ctx: CompilerExercisesParser.ExprLenContext): Any {
        return ExprLen(visit(ctx.getChild(2)) as Expr, loc(ctx))
    }

    override fun visitExprHex(ctx: CompilerExercisesParser.ExprHexContext): Any {
        return ExprConst(ctx.text.split('x')[1].toInt(16), loc(ctx))
    }

    override fun visitExprArray(ctx: CompilerExercisesParser.ExprArrayContext): Any {
        return ExprArray(
            visit(ctx.getChild(0)) as Expr,
            visit(ctx.getChild(2)) as Expr,
            loc(ctx)
        )
    }

    override fun visitExprPren(ctx: CompilerExercisesParser.ExprPrenContext): Any {
        return visit(ctx.getChild(1)) as Expr
    }

    override fun visitExprListPair(ctx: CompilerExercisesParser.ExprListPairContext): Any {
        val ret = mutableListOf(visit(ctx.getChild(0)) as Expr)
        (visit(ctx.getChild(2)) as List<Expr>).forEach { ret.add(it) }
        return ret
    }

    override fun visitExprListSingle(ctx: CompilerExercisesParser.ExprListSingleContext): Any {
        return listOf(visit(ctx.getChild(0)) as Expr)
    }


}