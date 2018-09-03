# symbol
# Faith
import algorithm, strformat, strutils, sequtils, tables, macros

type
  ExpressionKind* = enum ExprBinary, ExprSymbol, ExprNumber, ExprGroup, ExprPolynom

  Expression* = ref object
    case kind*: ExpressionKind:
    of ExprBinary:
      operation*: Operation
      left*:      Expression
      right*:     Expression
    of ExprSymbol:
      name*:      string
    of ExprNumber:
      i*:         int
    of ExprGroup:
      child*:     Expression
    of ExprPolynom:
      elements*:  seq[Expression]
      x*:         string

  
  Operation* = enum Add, Sub, Mult, Div, Pow, Log, Root, Sin, Cos, Tan, Cot

const PRIORITY: array[Operation, int] = [0, 0, 2, 3, 4, 4, 5, -1, -1, -1, -1]

var depth = 0

proc a*(i: int): Expression =
  Expression(kind: ExprNumber, i: i)

converter b(i: int): Expression =
  i.a

using
  left: Expression
  right: Expression
  expression: Expression



template binary*(op: Operation, l: Expression, r: Expression): Expression =
  Expression(kind: ExprBinary, operation: op, left: l, right: r)

template add*(left, right): Expression =
  binary(Add, left, right)

template sub*(left, right): Expression =
  binary(Sub, left, right)

template mult*(left, right): Expression =
  binary(Mult, left, right)

template divide*(left, right): Expression =
  binary(Div, left, right)

template pow*(left, right): Expression =
  binary(Pow, left, right)

template log*(left, right): Expression =
  binary(Log, left, right)

template root*(left, right): Expression =
  binary(Root, left, right)

template symbol*(a: string): Expression =
  Expression(kind: ExprSymbol, name: `a`)

template polynom*(ax: string, aelements: seq[Expression] = @[]): Expression =
  Expression(kind: ExprPolynom, x: ax, elements: aelements)


proc `$`*(expression: Expression): string
proc expand*(expression: Expression): Expression

proc `==`*(left, right): bool =
  if left.kind != right.kind:
    return false
  case left.kind:
  of ExprBinary:
    false
  of ExprSymbol:
    left.name == right.name
  of ExprNumber:
    left.i == right.i
  of ExprGroup:
    left.child == right.child
  of ExprPolynom:
    left.elements.zip(right.elements).allIt(it[0] == it[1])

template num*(op: untyped): untyped =
  if left.kind == ExprNumber and right.kind == ExprNumber:
    depth -= 1
    return `op`(left.i, right.i).int

proc `+`*(left, right): Expression =
  num `+`
  if left == right:
    result = mult(2.a, right)
  elif left == 0.a:
    result = right
  elif right == 0.a:
    result = left
  else:
    result = add(left, right)

proc `-`*(left, right): Expression =
  num `-`
  if left == right:
    result = 0.a
  else:
    result = sub(left, right)

proc `*`*(left, right): Expression  =
  num `*`
  if left == right:
    result = pow(left, 2.a)
  elif left == 1.a:
    result = right
  elif right == 1.a:
    result = left
  else:
    result = mult(left, right)

proc `/`*(left, right): Expression =
  if right.kind == ExprNumber and right.i == 0:
    raise newException(ValueError, "") 
  num `/`
  if left == right:
    result = 1.a
  else:
    result = divide(left, right)

proc `**`*(left, right): Expression =
  pow(left, right)

proc loadA*(expression; a: Expression): tuple[e: Expression, power: int] =
  if expression.kind == ExprBinary:
    if expression.operation == Mult:
      if expression.left.kind == ExprBinary and expression.left.operation== Pow and expression.left.left == a and expression.left.right.kind == ExprNumber:
        return (e: expression.right, power: expression.left.right.i)
      elif expression.right.kind == ExprBinary and expression.right.operation== Pow and expression.right.left == a and expression.right.right.kind == ExprNumber:
        return (e: expression.left, power: expression.right.right.i)
      elif expression.left == a:
        return (e: expression.right, power: 1)
      elif expression.right == a:
        return (e: expression.left, power: 1)
  elif expression.kind == ExprSymbol:
    return (e: 1.a, power: 1)
  return (e: expression, power: 0)

proc calculate*(expression): Expression =
  result = case expression.kind:
  of ExprBinary:
    let l = expression.left
    let r = expression.right
    case expression.operation:
    of Add: l + r
    of Sub: l - r
    of Mult: l * r
    of Div: l / r
    of Pow: l ** r
    else: expression
  of ExprSymbol, ExprNumber:
    expression
  of ExprGroup:
    expression.child.calculate
  of ExprPolynom:
    expression
      
proc collect*(expression; a: string): Expression =
  # x * y + x ** 2 + 2 , x => 2 * x ** 2 + y * x + 2
  var elements = expression.elements.mapIt(loadA(it, symbol(a)))
  elements.sort do (x: tuple[e: Expression, power: int], y: tuple[e: Expression, power: int]) -> int:
    cmp(x.power, y.power)

  echo elements
  result = polynom(a)
  var i = 0
  while i < elements.len:
    var element = elements[i]
    var power = element.power
    var nextPower = element.power
    var b = 0.a
    while power == nextPower:
      b = b + element.e
      i += 1
      if i < elements.len:
        element = elements[i]
        nextPower = element.power
      else:
        break
    echo b
    b = b.calculate
    echo b
    if power > 0:
      result.elements.add(mult(b, symbol(a)).calculate)
    else:
      result.elements.add(b)
  if result.elements.len == 0:
    result = result.elements[0]

proc flat*(left, right): seq[Expression]

proc flat*(expression): seq[Expression] =
  if expression.kind == ExprBinary and expression.operation in {Add, Sub}:
    flat(expression.left, expression.right)
  else:
    @[expression]

proc flat*(left, right): seq[Expression] =
  result = flat(left).concat(flat(right))

proc merge*[T](a: var Table[T, int], b: Table[T, int]) =
  for k, v in b:
    if not a.hasKey(k):
      a[k] = 0
    a[k] += v

proc findAll*(expression): Table[string, int] =
  case expression.kind:
  of ExprBinary:
    result = findAll(expression.left)
    result.merge(findAll(expression.right))
  of ExprSymbol:
    result = {expression.name: 1}.toTable()
  of ExprNumber:
    result = initTable[string, int]()
  of ExprGroup:
    result = findAll(expression.child)
  of ExprPolynom:
    result = initTable[string, int]()
    for element in expression.elements:
      result.merge(findAll(element))

proc findA*(expressions: seq[Expression]): string =
  var symbols = initTable[string, int]()
  for expression in expressions:
    let all = findAll(expression)
    symbols.merge(all)
  result = ""
  var m = 0
  for name, size in symbols:
    if size > m:
      result = name

proc expandAdd*(left, right): Expression =
  let children = flat(left, right)
  let a = findA(children)
  result = polynom(a, children)
  result = result.collect(a)

proc expandSub*(left, right): Expression =
  result = left - right

proc expandMult*(left, right): Expression =
  if left.kind == ExprBinary:
    let l = left.left
    let r = left.right
    let op = left.operation
    result = 
      case op:
      of Add: (l * right).expand + (r * right).expand
      of Sub: (l * right).expand - (r * right).expand
      of Mult: (l * r) * right
      of Div: (l / r) * right
      of Pow: (l ** r) * right
      else: left * right
  elif right.kind == ExprBinary:
    let l = right.left
    let r = right.right
    let op = right.operation
    result =
      case op:
      of Add: (left * l).expand + (left * r).expand
      of Sub: (left * l).expand - (left * r).expand
      of Mult: (left * l) * r
      of Div: (left / l) * right
      of Pow: (left ** l) * right
      else: left * right
  else:
    result = left * right
  if result.kind == ExprBinary:
    if result.operation == Mult:
      result = result.left * result.right

proc expandDiv*(left, right): Expression =
  left / right

proc expandPow*(left, right): Expression =
  left ** right


proc subs*(expression, left, right): Expression =
  # sympy subs
  result = expression
  if expression == left:
    return right
  case expression.kind:
  of ExprBinary:
    result.left = expression.left.subs(left, right)
    result.right = expression.right.subs(left, right)
  else:
    discard




proc expand*(expression): Expression =
  result = case expression.kind:
  of ExprBinary:
    let l = expression.left.expand
    let r = expression.right.expand
    case expression.operation:
    of Add: l.expandAdd(r)
    of Sub: l.expandSub(r)
    of Mult: l.expandMult(r)
    of Div: l.expandDiv(r)
    of Pow: l.expandPow(r)
    else: expression
  of ExprSymbol:
    expression
  of ExprNumber:
    expression
  of ExprGroup:
    expression.child.expand
  of ExprPolynom:
    expression

proc operator*(operation: Operation): string =
  let operators: array[Operation, string] = ["+", "-", "*", "/", "**", "log", "root", "cos", "sin", "tan", "cot"]
  operators[operation]


proc group*(left; right: Operation): Expression =
  if left.kind == ExprBinary and PRIORITY[left.operation] < PRIORITY[right]:
    Expression(kind: ExprGroup, child: left)
  else:
    left

proc `$`*(expression): string = 
  case expression.kind:
  of ExprBinary:
    let left = group(expression.left, expression.operation)
    let right = group(expression.right, expression.operation)
    &"{left} {operator(expression.operation)} {right}"
  of ExprSymbol:
    expression.name
  of ExprNumber:
    $expression.i
  of ExprGroup:
    &"({expression.child})"
  of ExprPolynom:
    expression.elements.mapIt($it).join(" + ")


let x = symbol("x")
let y = symbol("y")
echo "first:", (x ** 2 * 2 * x + 4).subs(x, y)
# echo "second:", ((x + 2) * (x - 2)).expand().expand()

