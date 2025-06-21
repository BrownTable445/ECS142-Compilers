grammar CompilerExercises;

/*
 * Being a bit simlper in design this is a single file
 * that combines both the lexer and parser.  This also
 * serves as the semantic documentation
 */

program : topLevelStatement+ ;

/*
 * There are only 2 top-level structures: functions, external linkages
 * and global variables.  Global variables must include an initialization (since
 * the type of the expression is used to infer the type of the identifier.)
 *
 */
topLevelStatement : type id '(' args ')' stmts #functionDefn
         | id ':=' expr ';'            #globalVar
         ;
// When initializing global variables it is a type error (UnknownVariable)
// for the expression to refer to the current global variable or global variables that
// have yet to be defined.  However, the expression can refer to global variaables
// that have already been defined and functions wherever they are defined.

// Arguments to functions must declare the types
args :                     #argsEmpty
     | arglist             #argsNonEmpty
     ;
arglist : type id
        | arglist ',' type id
        ;
/*
 * And the type system is simple: we only have integers and
 * arrays of (something), so array[int], array[array[int]] and so-on.
 *
 * This is so that it simple but not entirely trivial, requiring
 * deep comparisons to make sure types match.
 */
type : 'int'                #typeInt
     | 'array' '[' type ']' #typeArray
     ;

stmts : '{' '}' #stmtsEmpty
      | '{' stmt+ '}' #stmtsNonEmpty
      ;

stmt : lhs ':=' expr ';'                     # assignStatement
// Assignment statements also act as variable declaration.  If the
// lhs is a variable which doesn't exist it is added to the current
// scope and the type is equal to the type of the expression.  The newly
// defined variable  is only valid within the current lexical scope
// (defined by the block of statements).

// If the variable does exist it must be of the same type as the
// expression and this acts to reassign the variable.

     | 'if' '(' expr ')' stmts elifs else?   # ifStatement
// the expression must evaluate to an integer.  0 is
// considered false, anything nonzero is considered true.

     | 'for' '(' expr ')' stmts              # forStatement
// For loop expression must evaluate to an integer.  Again, 0
// is false, anything else is true.

     | 'return' expr ';'                     # returnStatement
// Return needs to check that the expression is equal to the
// defined return type of the enclosing function.

     | expr ';'                              # exprStatement
     ;

// Left hand side of an assignment statement.  It must
// either be an identifier or an array expression.  For the
// former it creates/writes the variable, for the latter it
// writes to the memory location specified by the array.
//
// For the array type the value must be whatever it is an
// array of.  Arrays are length checked with location 0 holding
// the actual length before .  And since all types are 4 bytes (we
// assume a 32b pointer/32b int) this means the actual location
// is 4 + 4 * expr
lhs : id                             #identifierLHS
    | expr '[' expr ']'              #arrayLHS
    ;



elifs :                                              #elifEmpty
      | 'else' 'if' '(' expr ')'  stmts  elifs #elifNonEmpty
      ;

else : 'else' stmts  #elseNonEmpty
      ;

/*
 * All of these are left->right associative, which is the
 * correct behavior we want.
 */
expr : id                                        #exprId
// Type is equal to the type of the identifier.

     | expr '[' expr ']'                                 #exprArray
// The first expression must be an array type, the second
// an integer type, and the resulting type is whatever type the
// array contains.

// Note that we store the size of the array in 0, so
// the actual location is 1 more.  Since integers are 4 bytes
// the actual location is 4 * expr + 4.  Before access
// it also ensures that expr is <= the size of the array, so
// arrays need runtime bounds checking.

     | expr ('*' | '/') expr                             #exprBinary
     | expr ('+' | '-') expr                             #exprBinary
     | expr ('==' | '!=' | '<' | '<=' | '>' | '>=') expr #exprBinary
// These all accept integers and return integers.  They do NOT accept
// arrays

     | '(' expr ')'                                      #exprPren
// This passes through the type of the subexpression.

     | 'make' 'array' '[' type ']' '(' expr ')'          #exprAlloc
// This creates an array of the specified type by calling
// malloc.  The call to malloc is 4 + 4*expr, and expr
// must be an integer.  Thee size (expr) is then written
// to location 0.

     | 'len' '(' expr ')'                                #exprLen
// This requires an array type argument and returns a number
// (the value of location 0)

     | id '(' exprlist? ')'                      #exprCall
// The function needs to exist (otherwise it is an "UnknownFunction" error.
// The number of arguments must match and they must type-match
// (otherwise it is an "InvalidArgument" error)

     | INTEGER                                           #exprInt
     | HEX                                               #exprHex
     ;

exprlist : expr ',' exprlist #exprListPair
         | expr # exprListSingle
         ;

INTEGER: '-'? [1-9][0-9]* | '0' ;
HEX: ('0x' | '0X') [0-9a-fA-F]+;

id : IDENTIFIER
   ;

IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_]*;
WS      : [ \t\r\n]+ -> skip;
COMMENT : ('//' ~[\n]+
        | '/*' (~'*' | ('*' ~'/'))* '*/') -> skip;


