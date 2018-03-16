package encrywm.frontend.parser

import encrywm.builtins.ESObject

object Ast {

  sealed trait AST_NODE

  case class Identifier(name: String)

  sealed trait TYPE {
    type Underlying
    val name: String
  }
  object TYPE {

    // Primitives
    case object UNIT extends TYPE {
      override type Underlying = Unit
      override val name: String = "unit"
    }
    case object BOOLEAN extends TYPE {
      override type Underlying = Boolean
      override val name: String = "bool"
    }
    case object INT extends TYPE {
      override type Underlying = Int
      override val name: String = "int"
    }
    case object LONG extends TYPE {
      override type Underlying = Long
      override val name: String = "long"
    }
    case object FLOAT extends TYPE {
      override type Underlying = Float
      override val name: String = "float"
    }
    case object DOUBLE extends TYPE {
      override type Underlying = Double
      override val name: String = "double"
    }
    case object STRING extends TYPE {
      override type Underlying = String
      override val name: String = "string"
    }
    case object BYTE_VECTOR extends TYPE {
      override type Underlying = Array[Byte]
      override val name: String = "bytes"
    }

    // Complex types
    case class LIST(valT: TYPE) extends TYPE {
      override type Underlying = List[valT.Underlying]
      override val name: String = "list"
    }
    case class DICT(keyT: TYPE, valT: TYPE) extends TYPE {
      override type Underlying = Map[keyT.Underlying, valT.Underlying]
      override val name: String = "dict"
    }
    case class TYPE_REF(name: String) extends TYPE {
      override type Underlying = ESObject
    }
  }

  sealed trait TREE_ROOT extends AST_NODE
  object TREE_ROOT {
    case class Contract(body: Seq[STMT]) extends TREE_ROOT
    case class Expression(body: Seq[STMT]) extends TREE_ROOT
  }

  sealed trait STMT extends AST_NODE
  object STMT {

    case class FunctionDef(name: Identifier, args: Arguments, body: Seq[STMT], returnType: Identifier) extends STMT
    case class Return(value: Option[EXPR]) extends STMT

    case class Assign(target: EXPR, value: EXPR) extends STMT
    case class AugAssign(target: EXPR, op: OPERATOR, value: EXPR) extends STMT

    case class Print(dest: Option[EXPR], values: Seq[EXPR], nl: Boolean) extends STMT

    case class For(target: EXPR, iter: EXPR, body: Seq[STMT], orelse: Seq[STMT]) extends STMT
    case class If(test: EXPR, body: Seq[STMT], orelse: Seq[STMT]) extends STMT

    case class Assert(test: EXPR, msg: Option[EXPR]) extends STMT

    case class Expr(value: EXPR) extends STMT

    // Under discussion
    case object Unlock extends STMT
    case object Abort extends STMT

    // col_offset is the byte offset in the utf8 string the parser uses
    case class attributes(lineno: Int, col_offset: Int)
  }

  sealed trait EXPR extends AST_NODE
  object EXPR {

    sealed trait TYPED_EXPR extends EXPR { var tpeOpt: Option[TYPE] }

    case class BoolOp(op: BOOL_OP, values: Seq[EXPR]) extends TYPED_EXPR { var tpeOpt: Option[TYPE] = Some(TYPE.BOOLEAN) }
    case class BinOp(left: EXPR, op: OPERATOR, right: EXPR) extends TYPED_EXPR { var tpeOpt: Option[TYPE] = None }
    case class UnaryOp(op: UNARY_OP, operand: EXPR) extends TYPED_EXPR { var tpeOpt: Option[TYPE] = None }
    case class Lambda(args: Arguments, body: EXPR) extends TYPED_EXPR { var tpeOpt: Option[TYPE] = None }
    case class IfExp(test: EXPR, body: EXPR, orelse: EXPR) extends TYPED_EXPR { var tpeOpt: Option[TYPE] = None }

    // Sequences are required for compare to distinguish between
    // x < 4 < 3 and (x < 4) < 3
    case class Compare(left: EXPR, ops: Seq[COMP_OP], comparators: Seq[EXPR]) extends TYPED_EXPR {
      override var tpeOpt: Option[TYPE] = Some(TYPE.BOOLEAN) }
    case class Call(func: EXPR, args: Seq[EXPR], keywords: Seq[Keyword]) extends TYPED_EXPR { var tpeOpt: Option[TYPE] = None }

    sealed trait Num extends TYPED_EXPR
    case class IntConst(n: Int) extends Num { var tpeOpt: Option[TYPE] = Some(TYPE.INT) }
    case class LongConst(n: Long) extends Num { var tpeOpt: Option[TYPE] = Some(TYPE.LONG) }
    case class FloatConst(n: Float) extends Num { var tpeOpt: Option[TYPE] = Some(TYPE.FLOAT) }
    case class DoubleConst(n: Double) extends Num { var tpeOpt: Option[TYPE] = Some(TYPE.DOUBLE) }

    case class Str(s: String) extends TYPED_EXPR { override var tpeOpt: Option[TYPE] = Some(TYPE.STRING) }

    // The following expression can appear in assignment context
    case class Attribute(value: EXPR, attr: Identifier, ctx: EXPR_CTX) extends TYPED_EXPR { var tpeOpt: Option[TYPE] = None }
    case class Subscript(value: EXPR, slice: SLICE, ctx: EXPR_CTX) extends TYPED_EXPR { var tpeOpt: Option[TYPE] = None }
    case class Name(id: Identifier, ctx: EXPR_CTX) extends TYPED_EXPR { var tpeOpt: Option[TYPE] = None }

    case class Dict(keys: Seq[EXPR], values: Seq[EXPR]) extends TYPED_EXPR { var tpeOpt: Option[TYPE] = None }
    case class Set(elts: Seq[EXPR]) extends TYPED_EXPR { var tpeOpt: Option[TYPE] = None }
    case class List(elts: Seq[EXPR], ctx: EXPR_CTX) extends TYPED_EXPR { var tpeOpt: Option[TYPE] = None }
    case class Tuple(elts: Seq[EXPR], ctx: EXPR_CTX) extends TYPED_EXPR { var tpeOpt: Option[TYPE] = None }

    // TODO: This expr stands out, shouldn't we move it to STMTs?
    case class Decl(target: EXPR, typeOpt: Option[Identifier]) extends EXPR
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
    case class ExtSlice(dims: Seq[SLICE]) extends SLICE
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

    case class ExceptHandler(`type`: Option[EXPR], name: Option[EXPR], body: Seq[STMT]) extends EXCP_HANDLER
  }

  case class Arguments(args: Seq[EXPR.Decl]) extends AST_NODE

  // keyword arguments supplied to call
  case class Keyword(arg: Identifier, value: EXPR) extends AST_NODE

  // import name with optional 'as' alias.
  case class Alias(name: Identifier, asname: Option[Identifier]) extends AST_NODE
}
