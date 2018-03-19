package encrywm.frontend.ast

object Ast {

  import encrywm.builtins.Types._

  sealed trait AST_NODE

  sealed trait TREE_ROOT extends AST_NODE
  object TREE_ROOT {
    case class Contract(body: List[STMT]) extends TREE_ROOT
    case class Expression(body: List[STMT]) extends TREE_ROOT
  }

  sealed trait STMT extends AST_NODE
  object STMT {

    case class FunctionDef(name: Identifier, args: Arguments, body: List[STMT], returnType: Identifier) extends STMT
    case class Return(value: Option[EXPR]) extends STMT

    case class Assign(target: EXPR, value: EXPR) extends STMT
    // Do we need operators like `+=`, `-=`, etc?
    case class AugAssign(target: EXPR, op: OPERATOR, value: EXPR) extends STMT

    case class For(target: EXPR, iter: EXPR, body: List[STMT], orelse: List[STMT]) extends STMT
    case class If(test: EXPR, body: List[STMT], orelse: List[STMT]) extends STMT

    case class Assert(test: EXPR, msg: Option[EXPR]) extends STMT

    case class Expr(value: EXPR) extends STMT

    // Under discussion
    case object Unlock extends STMT
    case object Halt extends STMT

    // col_offset is the byte offset in the utf8 string the parser uses
    case class attributes(lineno: Int, col_offset: Int)
  }

  sealed trait EXPR extends AST_NODE { var tpeOpt: Option[TYPE] }
  object EXPR {

    case class BoolOp(op: BOOL_OP, values: List[EXPR]) extends EXPR { var tpeOpt: Option[TYPE] = Some(BOOLEAN) }
    case class BinOp(left: EXPR, op: OPERATOR, right: EXPR, override var tpeOpt: Option[TYPE] = None) extends EXPR
    case class UnaryOp(op: UNARY_OP, operand: EXPR, override var tpeOpt: Option[TYPE] = None) extends EXPR
    case class Lambda(args: Arguments, body: EXPR, override var tpeOpt: Option[TYPE] = None) extends EXPR
    case class IfExp(test: EXPR, body: EXPR, orelse: EXPR, override var tpeOpt: Option[TYPE] = None) extends EXPR

    // Sequences are required for compare to distinguish between
    // x < 4 < 3 and (x < 4) < 3
    case class Compare(left: EXPR, ops: List[COMP_OP], comparators: List[EXPR]) extends EXPR {
      override var tpeOpt: Option[TYPE] = Some(BOOLEAN) }
    case class Call(func: EXPR, args: List[EXPR], keywords: List[Keyword], override var tpeOpt: Option[TYPE] = None) extends EXPR

    sealed trait Num extends EXPR
    case class IntConst(n: Int) extends EXPR with Num { var tpeOpt: Option[TYPE] = Some(INT) }
    case class LongConst(n: Long) extends EXPR with Num { var tpeOpt: Option[TYPE] = Some(LONG) }
    case class FloatConst(n: Float) extends EXPR with Num { var tpeOpt: Option[TYPE] = Some(FLOAT) }
    case class DoubleConst(n: Double) extends EXPR with Num { var tpeOpt: Option[TYPE] = Some(DOUBLE) }

    case class Str(s: String) extends EXPR { override var tpeOpt: Option[TYPE] = Some(STRING) }

    // The following expression can appear in assignment context
    case class Attribute(value: EXPR, attr: Identifier, ctx: EXPR_CTX, override var tpeOpt: Option[TYPE] = None) extends EXPR
    case class Subscript(value: EXPR, slice: SLICE, ctx: EXPR_CTX, override var tpeOpt: Option[TYPE] = None) extends EXPR
    case class Name(id: Identifier, ctx: EXPR_CTX, override var tpeOpt: Option[TYPE] = None) extends EXPR

    case class Dict(keys: List[EXPR], values: List[EXPR], override var tpeOpt: Option[TYPE] = None) extends EXPR
    case class ESet(elts: List[EXPR], override var tpeOpt: Option[TYPE] = None) extends EXPR
    case class EList(elts: List[EXPR], ctx: EXPR_CTX, override var tpeOpt: Option[TYPE] = None) extends EXPR
    case class Tuple(elts: List[EXPR], ctx: EXPR_CTX, override var tpeOpt: Option[TYPE] = None) extends EXPR

    case class Decl(target: EXPR, typeOpt: Option[Identifier]) extends EXPR { var tpeOpt: Option[TYPE] = Some(UNIT) }
  }

  // col_offset is the byte offset in the utf8 string the parser uses
  case class Attributes(lineno: Int, col_offset: Int)

  sealed trait EXPR_CTX
  object EXPR_CTX {

    case object Load extends EXPR_CTX
    case object Store extends EXPR_CTX
    case object Param extends EXPR_CTX

    // TODO: Do we need those?
    case object AugLoad extends EXPR_CTX
    case object AugStore extends EXPR_CTX
  }

  sealed trait SLICE extends AST_NODE
  object SLICE {

    case object Ellipsis extends SLICE
    case class Slice(lower: Option[EXPR], upper: Option[EXPR], step: Option[EXPR]) extends SLICE
    case class ExtSlice(dims: List[SLICE]) extends SLICE
    case class Index(value: EXPR) extends SLICE
  }

  sealed trait BOOL_OP
  object BOOL_OP {
    case object And extends BOOL_OP
    case object Or extends BOOL_OP
  }

  sealed trait OPERATOR extends AST_NODE
  case object OPERATOR {
    case object Add extends OPERATOR
    case object Sub  extends OPERATOR
    case object Mult  extends OPERATOR
    case object Div  extends OPERATOR
    case object Mod  extends OPERATOR
    case object Pow  extends OPERATOR

    case object FloorDiv extends OPERATOR
  }

  sealed trait UNARY_OP
  object UNARY_OP {

    case object Invert extends UNARY_OP
    case object Not extends UNARY_OP
    case object UAdd extends UNARY_OP
    case object USub extends UNARY_OP
  }

  sealed trait COMP_OP
  object COMP_OP {

    case object Eq extends COMP_OP
    case object NotEq extends COMP_OP
    case object Lt extends COMP_OP
    case object LtE extends COMP_OP
    case object Gt extends COMP_OP
    case object GtE extends COMP_OP
    case object Is extends COMP_OP
    case object IsNot extends COMP_OP
    case object In extends COMP_OP
    case object NotIn extends COMP_OP
  }

  // not sure what to call the first argument for raise and except
  sealed trait EXCP_HANDLER extends AST_NODE
  object EXCP_HANDLER {

    case class ExceptHandler(`type`: Option[EXPR], name: Option[EXPR], body: List[STMT]) extends EXCP_HANDLER
  }

  case class Identifier(name: String)

  case class Arguments(args: List[EXPR.Decl]) extends AST_NODE

  // keyword arguments supplied to call
  case class Keyword(arg: Identifier, value: EXPR) extends AST_NODE

  // import name with optional 'as' alias.
  case class Alias(name: Identifier, asname: Option[Identifier]) extends AST_NODE
}
