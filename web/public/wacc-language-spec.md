# WACC Language Specification

This document specifies the WACC language accepted by this compiler. It is written in the style of a language reference: syntax is described with grammar productions, and semantic restrictions are stated as compile-time or run-time rules.

The specification is based on the current implementation in `src/main/wacc/frontend` and the valid programs in `examples/valid`. In addition to the core WACC language, this compiler supports `float`, bitwise operators, side-effecting expressions, `for`, `do-while`, `switch`, exceptions, simple macros, and function overloading.

## 1. Notation

Grammar productions use the following conventions:

```text
X ::= Y          X is defined as Y
X?              zero or one X
X*              zero or more X
X+              one or more X
"token"         the literal token token
```

WACC uses semicolons as separators between statements. A trailing semicolon before `end`, `fi`, `done`, `else`, or a new `case` label is not an empty statement and is not part of the normal grammar.

## 2. Lexical Structure

### 2.1 Whitespace and Comments

Whitespace separates tokens and is otherwise ignored.

A line comment starts with `#` and continues to the end of the line. Comments are not recognised inside character or string literals.

Example:

```wacc
begin
  int x = 1 # comment after a statement
end
```

### 2.2 Keywords

The following words are reserved and cannot be used as identifiers:

```text
int char float string bool pair null
true false begin end is skip switch case default
read free return throw exit print println
if then else fi while do done for
newpair fst snd call len ord chr
try catch Break Continue
ArrayOutOfBoundsException BadCharException ArithmeticException
IntegerOverflowException NullDereferenceException Exception
```

`Break` and `Continue` are capitalised keywords. Lowercase `break` and `continue` are not statement keywords in this compiler.

### 2.3 Operators and Separators

The following operator and separator tokens are recognised:

```text
! - * / % +
> >= < <=
== != && ||
= ; , ( ) [ ]
& | ~
++ -- += -= *= /= %=
```

### 2.4 Identifiers

An identifier starts with a letter or `_`, followed by zero or more letters, digits, or `_`.

```text
Identifier ::= LetterOrUnderscore (LetterOrDigitOrUnderscore)*
```

Identifiers are case-sensitive.

Valid examples:

```wacc
int x = 0;
int _tmp = 1;
int long_variable_name_123 = 2
```

### 2.5 Literals

Integer literals are signed 32-bit decimal values. Decimal literals with leading zeroes are accepted. Binary, octal, and hexadecimal integer forms are not accepted.

```wacc
int x = 00042
```

Boolean literals are:

```text
true
false
```

Character and string literals are ASCII only. The printable graphic characters are accepted except unescaped `\`, `"`, and `'`.

Supported escape sequences are:

```text
\0 \b \t \n \f \r \\ \" \'
```

Float literals are accepted as single-precision `float` values.

```wacc
float x = 2.25
```

The null pair literal is:

```text
null
```

### 2.6 Macro Preprocessing

Before parsing, the compiler applies a simple line-oriented macro preprocessor.

```text
MacroDefinition ::= "@define" Identifier ReplacementText
```

Rules:

- A macro definition may be indented.
- Macro names follow the normal identifier character rules.
- Function-like macros are not supported.
- A macro name may not be defined more than once.
- Macro expansion is textual and applies outside strings, character literals, and comments.
- Macro expansion is recursive, but recursive cycles are rejected.
- A macro definition affects later source text; it does not emit a WACC statement.

Example:

```wacc
@define LIMIT 10

begin
  int x = LIMIT
end
```

## 3. Programs and Functions

### 3.1 Program Form

Every program has one outer `begin ... end` block.

```text
Program ::= "begin" FunctionDeclaration* Statement "end"
```

Function declarations, if present, must appear before the main statement body. Nested function declarations are not part of the grammar.

The compiler accepts any input file path; `.wacc` is the conventional extension used by most examples.

### 3.2 Function Declarations

```text
FunctionDeclaration ::=
    Type Identifier "(" ParameterList? ")" "is" Statement "end"

ParameterList ::= Parameter ("," Parameter)*
Parameter     ::= Type Identifier
```

Examples:

```wacc
int f() is
  return 0
end

int add(int x, int y) is
  return x + y
end
```

A function body must be statically returning. A compile-time syntax error occurs if a function can fall off the end without reaching a `return`, `throw`, or `exit`.

The return checker treats these constructs as returning:

- `return e`
- `throw E(e)`
- `exit e`
- `if e then s1 else s2 fi`, only if both branches return
- `switch`, only if it has a `default` and every possible case entry returns, accounting for fall-through and `Break`
- `try ... catch ... done`, only if the try body and every catch body return
- `begin s end`, if `s` returns
- a statement sequence, if its last reachable statement returns

Loops are not treated as statically returning by themselves, even if their condition is syntactically `true`.

## 4. Types

### 4.1 Type Grammar

```text
Type         ::= (BaseType | PairType) ArraySuffix*
BaseType     ::= "int" | "bool" | "char" | "float" | "string"
ArraySuffix  ::= "[]"

PairType     ::= "pair" "(" PairElemType "," PairElemType ")"
PairElemType ::= BaseType
               | PairType
               | (BaseType | PairType) ArraySuffix+
               | "pair"
```

The standalone erased type `pair` is allowed only as a pair element type. A top-level variable, parameter, or return type must use a concrete `pair(T, U)` if it is a pair.

Valid examples:

```wacc
int x = 1;
bool b = true;
char c = 'A';
float f = 1.5;
string s = "hello";
int[] xs = [1, 2, 3];
int[] row1 = [1];
int[] row2 = [2];
int[][] matrix = [row1, row2];
pair(int, char) p = newpair(1, 'a');
pair(int, pair) node = newpair(1, null)
```

### 4.2 Primitive Types

`int` is a signed 32-bit integer type.

`bool` contains `true` and `false`.

`char` contains ASCII character values.

`float` is a single-precision floating-point type.

`string` is a primitive source type represented at run time like a character array pointer.

### 4.3 Array Types

An array type is written by appending one or more `[]` suffixes to a base or pair type.

```wacc
int[] a = [1, 2, 3];
int[] b = [4, 5];
int[][] nested = [a, b]
```

Array literals are heap values. The compiler infers array literal element types from the declaration, assignment target, or expected context.

The empty array literal `[]` is accepted when its expected type can be inferred:

```wacc
int[] xs = [];
xs = [1]
```

### 4.4 Strings and Character Arrays

A `char[]` value is compatible with `string`.

```wacc
string s = ['h', 'i'];
char[] chars = ['o', 'k'];
string[] values = [chars, "box"]
```

A `char[]` can be printed as a string and can be indexed and updated as an array:

```wacc
char[] text = ['h', 'i'];
text[0] = 'H';
println text
```

A source-level `string` value is not indexable by this compiler. Indexing syntax is array-element syntax and is rejected semantically for `string`.

### 4.5 Pair Types

Pairs are constructed with `newpair(e1, e2)` and accessed with `fst` and `snd`.

```wacc
pair(int, bool) p = newpair(10, true);
int x = fst p;
bool y = snd p
```

Pair element types may use the erased `pair` type. This supports nested structures such as linked lists:

```wacc
pair(int, pair) node = newpair(1, null)
```

The literal `null` has erased pair type. It is compatible with concrete pair types where context supplies the missing type.

### 4.6 Exception Types

Exception type names are used in `catch` clauses. They are not parsed as ordinary variable, parameter, or return types.

Supported exception type names are:

```text
ArithmeticException
BadCharException
ArrayOutOfBoundsException
IntegerOverflowException
NullDereferenceException
Exception
```

## 5. Expressions

### 5.1 Expression Grammar

```text
Expression ::= SideEffectingExpression
             | PrefixOperator Expression
             | Expression BinaryOperator Expression
             | Atom

PrefixOperator ::= "!" | "-" | "len" | "ord" | "chr" | "~"

SideEffectingExpression ::= UnarySideEffectingExpression
                          | CompoundAssignmentExpression

UnarySideEffectingExpression ::= ("++" | "--") LValue

CompoundAssignmentExpression ::= LValue CompoundAssignmentOperator Expression

CompoundAssignmentOperator ::= "+=" | "-=" | "*=" | "/=" | "%="

Atom ::= IntegerLiteral
       | BooleanLiteral
       | CharacterLiteral
       | FloatLiteral
       | StringLiteral
       | "null"
       | Identifier
       | ArrayElement
       | "(" Expression ")"

ArrayElement ::= Identifier ("[" Expression "]")+
```

Function calls, pair creation, pair-element r-values, and array literals are r-values, not general expressions. For example, `call f()` is valid on the right-hand side of a declaration or assignment, but it is not parsed as a nested expression operand.

### 5.2 Operator Precedence and Associativity

Operators are listed from highest precedence to lowest precedence.

| Level | Operators | Associativity | Operand rule | Result |
| ---: | --- | --- | --- | --- |
| 1 | `!`, `-`, `len`, `ord`, `chr`, `~`, `++`, `--` | prefix | see unary and side-effecting rules | see unary and side-effecting rules |
| 2 | `*`, `/`, `%` | left | numeric, but `%` requires `int` | numeric or `int` |
| 3 | `+`, `-` | left | `int` or `float` | `int` or `float` |
| 4 | `>`, `>=`, `<`, `<=` | non-associative | same ordered type: `int`, `char`, or `float` | `bool` |
| 5 | `==`, `!=` | non-associative | compatible types | `bool` |
| 6 | `&` | left | `int` | `int` |
| 7 | `|` | left | `int` | `int` |
| 8 | `&&` | right | `bool` | `bool` |
| 9 | `||` | right | `bool` | `bool` |

Compound assignment expressions have lower precedence than the binary operators above. The left-hand side must be an l-value, and the right-hand side is a full expression.

### 5.3 Unary Operators

```text
!e       e must be bool; result is bool
-e       e must be int or float; result has the same numeric family
len e    e must be an array; result is int
ord e    e must be char; result is int
chr e    e must be int; result is char
~e       e must be int; result is int
```

`chr` performs a run-time ASCII range check. A run-time error or catchable `BadCharException` occurs if the integer is outside `0..127`.

### 5.4 Arithmetic Operators

`+`, `-`, `*`, and `/` accept `int` and `float` operands. If either operand is `float`, integer operands are converted to `float` and the result is `float`.

`%` accepts only `int` operands.

Integer division and modulo by zero are checked at run time.

### 5.4.1 Side-effecting Expressions

Side-effecting expressions update an l-value and evaluate to the updated value.

```text
++x       x must be an int or float l-value; result is the updated value
--x       x must be an int or float l-value; result is the updated value
x += e    x must be an int or float l-value; result is the updated value
x -= e    x must be an int or float l-value; result is the updated value
x *= e    x must be an int or float l-value; result is the updated value
x /= e    x must be an int or float l-value; result is the updated value
x %= e    x and e must have type int; result is the updated value
```

For `+=`, `-=`, `*=`, and `/=`, an `int` l-value requires an `int` right-hand side. A `float` l-value accepts `int` or `float` right-hand sides; integer values are converted to `float`.

Examples:

```wacc
int x = 1;
println ++x;
println (x += 3);

int[] a = [10, 20];
println (a[0] -= 4);

pair(int, int) p = newpair(5, 6);
println ++fst p
```

### 5.5 Boolean Operators

`&&` and `||` require `bool` operands and use short-circuit evaluation.

### 5.6 Bitwise Operators

`&`, `|`, and `~` require `int` operands and produce `int`.

### 5.7 Comparison Operators

`>`, `>=`, `<`, and `<=` require both operands to have the same ordered type. The ordered types are `int`, `char`, and `float`.

`==` and `!=` require the operands to have a common compatible type. Pairs, arrays, strings, primitive values, and `null` may be compared when the compatibility rules allow it.

### 5.8 Array Element Expressions

An array element expression has one or more indices:

```wacc
a[0]
matrix[i][j]
```

Each index expression must have type `int`.

The base identifier must have an array type. If too many indices are supplied, a compile-time type error occurs.

At run time, array access checks:

- the base pointer is not null,
- each index is at least `0`,
- each index is less than the array length.

## 6. L-values and R-values

### 6.1 L-values

```text
LValue ::= Identifier
         | ArrayElement
         | PairElement

PairElement ::= "fst" LValue
              | "snd" LValue
```

L-values are assignable storage locations.

Examples:

```wacc
x = 1;
a[0] = 2;
fst p = 3;
snd a[1] = 'x';
fst snd nested = 7
```

### 6.2 R-values

```text
RValue ::= Expression
         | ArrayLiteral
         | NewPair
         | PairElement
         | FunctionCall

ArrayLiteral ::= "[" ExpressionList? "]"
ExpressionList ::= Expression ("," Expression)*

NewPair ::= "newpair" "(" Expression "," Expression ")"

FunctionCall ::= "call" Identifier "(" ArgumentList? ")"
ArgumentList ::= Expression ("," Expression)*
```

R-values appear on the right-hand side of declarations and assignments.

Examples:

```wacc
int x = 1 + 2;
int[] xs = [1, 2, 3];
pair(int, char) p = newpair(10, 'a');
int first = fst p;
int y = call f(1, 2)
```

## 7. Statements

### 7.1 Statement Grammar

```text
Statement ::= StatementAtom (";" StatementAtom)*

StatementAtom ::= "skip"
                | Type Identifier "=" RValue
                | LValue "=" RValue
                | SideEffectingExpression
                | "read" LValue
                | "free" Expression
                | "return" Expression
                | "throw" ExceptionConstructor
                | "exit" Expression
                | "print" Expression
                | "println" Expression
                | "begin" Statement "end"
                | IfStatement
                | WhileStatement
                | ForStatement
                | DoWhileStatement
                | SwitchStatement
                | TryCatchStatement
                | "Break"
                | "Continue"
```

### 7.2 Declaration Statements

```text
Declaration ::= Type Identifier "=" RValue
```

A compile-time error occurs if the right-hand side is not compatible with the declared type.

The declared variable is not in scope inside its own initializer.

```wacc
int x = 1
```

### 7.3 Assignment Statements

```text
Assignment ::= LValue "=" RValue
```

A compile-time error occurs if the right-hand side is not compatible with the left-hand side type.

Pair-element assignments whose element types are completely unknown are rejected unless the type of at least one side is known or specified.

Side-effecting expressions may also be used as standalone statements:

```wacc
x += 1;
++a[0];
--fst p
```

Ordinary non-side-effecting expressions are not standalone statements.

### 7.4 Read Statements

```text
ReadStatement ::= "read" LValue
```

The target l-value must have type `int`, `char`, or `float`.

The run-time helper preserves the previous value if input cannot be read, including EOF.

### 7.5 Free Statements

```text
FreeStatement ::= "free" Expression
```

The expression must have array or pair type. The valid examples use direct array or pair variables.

Freeing a null pair is a run-time error. Array freeing is lowered through the array free helper.

### 7.6 Return Statements

```text
ReturnStatement ::= "return" Expression
```

`return` is legal only inside a function. A compile-time semantic error occurs if `return` appears in the main program.

The returned expression must be compatible with the function's declared return type.

### 7.7 Throw Statements

```text
ThrowStatement ::= "throw" ExceptionConstructor

ExceptionConstructor ::= "ArrayOutOfBoundsException" "(" Expression ")"
                       | "ArithmeticException" "(" Expression ")"
                       | "IntegerOverflowException" "(" Expression ")"
                       | "NullDereferenceException" "(" Expression ")"
                       | "BadCharException" "(" Expression ")"
                       | "Exception" "(" Expression ")"
```

The constructor argument must have type `string`.

### 7.8 Exit Statements

```text
ExitStatement ::= "exit" Expression
```

The expression must have type `int`.

### 7.9 Print Statements

```text
PrintStatement   ::= "print" Expression
PrintlnStatement ::= "println" Expression
```

`print` emits the expression without a newline. `println` emits the expression followed by a newline.

Printing rules:

- `int`, `bool`, `char`, `float`, and `string` values print as values.
- `char[]` prints as a string.
- non-character arrays and pairs print as pointers.
- `bool` prints as `true` or `false`.

### 7.10 Blocks

```text
Block ::= "begin" Statement "end"
```

A block introduces a nested variable scope.

### 7.11 If Statements

```text
IfStatement ::= "if" Expression "then" Statement "fi"
              | "if" Expression "then" Statement "else" Statement "fi"
```

The condition must have type `bool`.

The `then` branch and `else` branch, if present, are nested scopes.

### 7.12 While Statements

```text
WhileStatement ::= "while" Expression "do" Statement "done"
```

The condition must have type `bool`.

The body is a nested scope. `Break` and `Continue` are valid inside the body. `Continue` jumps to the condition check.

### 7.13 For Statements

```text
ForStatement ::= "for (" Statement "," Expression "," Statement ")" Statement "done"
```

The middle expression must have type `bool`.

The initializer, condition, update statement, and body share the same loop scope. Variables declared by the initializer are visible in the condition, update statement, and body, but not after the loop.

`Break` exits the loop. `Continue` jumps to the update statement before rechecking the condition.

Example:

```wacc
for (int i = 0, i < 10, i = i + 1)
  println i
done
```

### 7.14 Do-while Statements

```text
DoWhileStatement ::= "do" Statement "while" Expression
```

The body executes before the condition is checked. The condition must have type `bool`.

The body is a nested scope. `Continue` jumps to the trailing condition check.

### 7.15 Break and Continue

```text
BreakStatement    ::= "Break"
ContinueStatement ::= "Continue"
```

`Break` is valid inside loops and `switch` statements.

`Continue` is valid only inside loops.

### 7.16 Switch Statements

```text
SwitchStatement ::= "switch (" Expression ")" SwitchCase* "end"

SwitchCase ::= SwitchLabel+ Statement*
SwitchLabel ::= "case" Expression ":"
              | "default" ":"
```

The selector may be any expression. Each `case` expression must be compatible with the selector type.

The compiler supports `int`, `char`, `bool`, `float`, string-compatible, pair-compatible, and array-compatible case expressions where normal expression and compatibility rules allow them. The valid examples include `int`, `char`, `float`, expression selectors, grouped case labels, and `default`.

Several labels may share the same body:

```wacc
switch (grade)
  case 'A': case 'B': case 'C':
    println "passing";
    Break
  default:
    println "unknown"
end
```

Execution starts at the first matching label. If no case label matches, execution starts at the first `default` label, if one exists. If no label matches and there is no `default`, the switch does nothing.

Case bodies are contiguous. Reaching the end of a case body falls through to the next case body. Use `Break` to leave the switch.

### 7.17 Try-catch Statements

```text
TryCatchStatement ::= "try" Statement CatchHandler+ "done"

CatchHandler ::= "catch" ExceptionType Identifier "do" Statement

ExceptionType ::= "ArithmeticException"
                | "BadCharException"
                | "ArrayOutOfBoundsException"
                | "IntegerOverflowException"
                | "NullDereferenceException"
                | "Exception"
```

The try body is a nested scope. Each catch handler has its own nested scope.

The catch variable is bound as a `string` containing the exception message.

Handlers are tested in source order. A handler matches when its exception type has the same run-time exception id as the thrown exception.

Important: `catch Exception` matches `throw Exception(...)`. It is not a catch-all for every exception type.

## 8. Names, Declarations, and Scopes

### 8.1 Variable Declarations

A compile-time error occurs if a variable is redeclared in the same scope.

Inner scopes may shadow outer variables.

```wacc
int x = 1;
begin
  bool x = true;
  println x
end;
println x
```

### 8.2 Scope Boundaries

These constructs introduce nested variable scopes:

- `begin ... end` blocks,
- `if` branches,
- `else` branches,
- `while` bodies,
- `do-while` bodies,
- `try` bodies,
- each `catch` handler,
- function bodies.

`for` creates one loop scope shared by the initializer, condition, update, and body.

`switch` does not create a separate nested variable scope in the renamer implementation. Case bodies are processed in source order in the surrounding scope.

### 8.3 Function Names

Function names live in a separate namespace from variables.

Therefore these are valid:

```wacc
int f(int f) is
  return f
end

int f = call f(99)
```

Function names may also coincide with libc symbol names such as `malloc`, `scanf`, `printf`, or `puts`; overloaded functions are name-mangled by the backend.

### 8.4 Parameters

Function parameters are declared in the function parameter scope. A compile-time error occurs if two parameters in the same parameter list have the same name.

The function body is a nested scope under the parameter scope, so the body may shadow a parameter:

```wacc
bool f(int x) is
  bool x = true;
  return x
end
```

## 9. Type Compatibility

### 9.1 General Compatibility

Two values are compatible if:

- their semantic types are identical,
- one type can be weakened to the other by a rule below,
- or an unknown type is being resolved by context during type inference.

A compile-time type error occurs when no compatibility rule applies.

### 9.2 Weakening Rules

The compiler implements these weakening rules:

```text
char[]              -> string
pair(T, U)          -> pair
pair                -> pair(T, U)
```

The last two rules describe compatibility between concrete pair types and the erased pair type. They are what make `null` and nested erased pair components useful.

### 9.3 Arrays

Array compatibility is based on element compatibility and the expected dimensional context.

For declarations of array literals, the declared array dimensions and element type must match the inferred literal type.

For indexing, each supplied index removes one array dimension. If the number of indices equals the array dimension count, the result is the element type. If fewer indices are supplied, the result is a lower-dimensional array.

### 9.4 Pairs

Concrete pair types are compatible when both components are compatible.

Nested concrete pairs are erased when they occur as pair element display types, following the usual WACC pair-erasure model.

The erased type `pair` does not carry component information. If an assignment attempts to exchange pair elements and both sides remain completely unknown, the compiler reports a type inference error.

### 9.5 Exceptions

Exception values are not ordinary expression values. Exception type names appear only in `catch` clauses, and exception constructors appear only in `throw` statements.

## 10. Functions and Overloading

### 10.1 Declaration Collection

The compiler builds the complete function table before type-checking function bodies. This permits:

- calls to functions declared later in the source file,
- direct recursion,
- mutual recursion.

### 10.2 Overload Signatures

Functions may share a name if their parameter type lists differ.

```wacc
int tag(int x) is return x end
int tag(bool x) is return 0 end
int tag(char[] xs) is return len xs end
```

A compile-time error occurs if two functions have the same name and the same parameter type list.

The return type is not part of overload selection.

### 10.3 Call Resolution

```text
FunctionCall ::= "call" Identifier "(" ArgumentList? ")"
```

A call is resolved as follows:

1. Select overloads with the same name.
2. Reject the call if no such function exists.
3. Select overloads with the same arity as the call.
4. Reject the call if no overload has that arity.
5. Keep overloads whose parameter types are compatible with the argument types.
6. Rank matches by more exact matches and fewer weakening conversions.
7. Reject the call if several best-ranked overloads remain.
8. Recheck arguments against the selected parameter types.

Examples covered by the valid programs:

- overloading by arity,
- overloading by primitive parameter type,
- overloading by parameter order,
- overloading by array element type,
- overloading by pair type,
- exact `char[]` match preferred over weakening to `string`.

## 11. Exceptions and Run-time Errors

### 11.1 Explicit Exceptions

An explicit `throw` allocates an exception object containing:

- an exception id,
- a message string.

If there is an active `try-catch`, control transfers to the innermost active catch dispatcher. If no handler catches the exception in main code, the program reports:

```text
fatal error: <message>
```

and exits with the run-time error exit code.

### 11.2 Implicit Run-time Exceptions

The compiler also synthesizes exceptions for checked run-time failures:

| Failure | Exception id/type | Message |
| --- | --- | --- |
| division or modulo by zero | `ArithmeticException` | `divide or modulo by zero` |
| `chr` outside ASCII range `0..127` | `BadCharException` | `int is not ascii character 0-127` |
| array index out of bounds | `ArrayOutOfBoundsException` | `array index out of bounds` |
| integer overflow or underflow | `IntegerOverflowException` | `integer overflow or underflow occurred` |
| null or freed pair dereference | `NullDereferenceException` | `null dereference or freed pair` |

Inside an active `try-catch`, these implicit exceptions can be caught by a matching handler. Outside a handler context, they are reported as fatal run-time errors.

### 11.3 Handler Matching

Handlers are exact-match handlers over the implementation's exception ids.

```wacc
try
  int x = 10 / 0
catch ArithmeticException err do
  println err
done
```

`catch Exception` catches only a `GeneralException` produced by `throw Exception(...)`; it does not catch `ArithmeticException`, `BadCharException`, or the other specific exception types.

### 11.4 Propagation Through Function Calls

Function calls communicate exceptional completion through the compiler's global exception slot. After a call returns, the caller checks whether an exception was raised and either dispatches to its active handler or reports/propagates the exception.

This allows exceptions thrown inside a function call to be caught by a surrounding try-catch in the caller.

## 12. Input and Output

### 12.1 Reading

`read` supports:

- `int`,
- `char`,
- `float`.

Reading into pair elements and array elements is valid when the selected l-value has one of those types.

If `scanf` cannot read a value, the previous value of the target is preserved.

### 12.2 Printing

The compiler provides print helpers for:

- integers,
- booleans,
- characters,
- floats,
- strings and `char[]`,
- pointers for arrays and pairs.

`println e` is equivalent to `print e` followed by a newline.

## 13. Implementation-defined Behaviour

This section records behaviour that follows from this compiler implementation.

- `call`, `newpair`, array literals, and pair-element r-values are not general expressions; they are r-values used in declarations and assignments.
- Side-effecting expressions are general expressions, and they are the only expressions accepted as standalone expression statements.
- `free` is parsed as accepting an expression and type-checked for array or pair type, but the valid examples and backend lowering use direct array or pair variables.
- `switch` bodies fall through like C/Java switch bodies; `Break` is required to stop fall-through.
- `switch` does not introduce a new variable scope.
- `do-while` has no closing `done`.
- `for` uses `Continue` to jump to the update statement.
- `do-while` uses `Continue` to jump to the trailing condition check.
- Function overloads are lowered by name mangling, so source-level function names can overlap with C library names.

## 14. Minimal Complete Example

```wacc
begin
  int max(int x, int y) is
    if x > y then
      return x
    else
      return y
    fi
  end

  int[] values = [3, 7, 4];
  int best = call max(values[0], values[1]);

  switch (best)
    case 7:
      println "ok";
      Break
    default:
      println "unexpected"
  end
end
```

## 15. Compiler Usage

Build the compiler:

```bash
make
```

Compile a WACC program:

```bash
./compile path/to/program.wacc
```

Options:

```bash
./compile path/to/program.wacc --architecture arm32
./compile path/to/program.wacc --architecture aarch64
./compile path/to/program.wacc --peephole-optim
./compile path/to/program.wacc --no-peephole
```

Defaults:

- target architecture: `arm32`,
- peephole optimisation: enabled,
- output file: `<program>.s` in the current working directory.
