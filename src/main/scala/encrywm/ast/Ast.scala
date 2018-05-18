package encrywm.ast

import encrywm.utils.mast.Utils.listRootHash
import scorex.crypto.hash.Blake2b256

object Ast {

  import encrywm.lib.Types._

  type Hash = Array[Byte]

  sealed trait AST_NODE {
    val hash: Hash
  }

  sealed trait TREE_ROOT extends AST_NODE

  object TREE_ROOT {
    case class Contract(body: List[STMT]) extends TREE_ROOT {
      override val hash: Hash = listRootHash(body.map(_.hash))
    }
    case class Expression(body: List[STMT]) extends TREE_ROOT {
      override val hash: Hash = listRootHash(body.map(_.hash))
    }
  }

  sealed trait STMT extends AST_NODE

  object STMT {

    case class FunctionDef(name: Identifier, args: Arguments, body: List[STMT], returnType: Identifier) extends STMT {
      override val hash: Hash = listRootHash(name.hash +: args.hash +: body.map(_.hash) :+ returnType.hash)
    }
    case class Return(value: Option[EXPR]) extends STMT {
      override val hash: Hash = value.map(_.hash).getOrElse(Array.emptyByteArray)
    }

    case class Let(target: EXPR, value: EXPR, global: Boolean = false) extends STMT {
      override val hash: Hash = listRootHash(List(target.hash, value.hash, if (global) Array(1: Byte) else Array(0: Byte)))
    }

    // Reassignment could be introduced later.
    case class AugAssign(target: EXPR, op: OPERATOR, value: EXPR) extends STMT {
      override val hash: Hash = listRootHash(List(target.hash, op.hash, value.hash))
    }

    // For loop could be introduced later.
    case class For(target: EXPR, iter: EXPR, body: List[STMT], orelse: List[STMT]) extends STMT {
      override val hash: Hash = listRootHash(target.hash +: iter.hash +: (body.map(_.hash) ++ orelse.map(_.hash)))
    }
    case class If(test: EXPR, body: List[STMT], orelse: List[STMT]) extends STMT {
      override val hash: Hash = listRootHash(test.hash +: (body.map(_.hash) ++ orelse.map(_.hash)))
    }
    case class Match(target: EXPR, branches: List[STMT]) extends STMT {
      override val hash: Hash = listRootHash(target.hash +: branches.map(_.hash))
    }
    case class Case(cond: EXPR, body: List[STMT], isDefault: Boolean = false) extends STMT {
      override val hash: Hash = listRootHash(cond.hash +: body.map(_.hash))
    }

    case class Assert(test: EXPR, msg: Option[EXPR]) extends STMT {
      override val hash: Hash = listRootHash(List(test.hash, msg.map(_.hash).getOrElse(Array.emptyByteArray)))
    }

    case class Expr(value: EXPR) extends STMT {
      override val hash: Hash = value.hash
    }

    case class UnlockIf(test: EXPR) extends STMT {
      override val hash: Hash = test.hash
    }
    case object Halt extends STMT {
      override val hash: Hash = Blake2b256("STMT_HALT")
    }
    case object Pass extends STMT {
      override val hash: Hash = Blake2b256("STMT_PASS")
    }

    // col_offset is the byte offset in the utf8 string the parser uses
    case class Attributes(lineno: Int, col_offset: Int)
  }

  sealed trait EXPR extends AST_NODE { var tpeOpt: Option[ESType] }

  object EXPR {

    case class BoolOp(op: BOOL_OP, values: List[EXPR]) extends EXPR {
      var tpeOpt: Option[ESType] = Some(ESBoolean)
      override val hash: Hash = listRootHash(op.hash +: values.map(_.hash))
    }
    case class BinOp(left: EXPR, op: OPERATOR, right: EXPR, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(List(left.hash, op.hash, right.hash))
    }
    case class UnaryOp(op: UNARY_OP, operand: EXPR, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(List(op.hash, operand.hash))
    }
    case class Lambda(args: Arguments, body: EXPR, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(List(args.hash, body.hash))
    }
    case class IfExp(test: EXPR, body: EXPR, orelse: EXPR, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(List(test.hash, body.hash, orelse.hash))
    }

    // Sequences are required for compare to distinguish between
    // x < 4 < 3 and (x < 4) < 3
    case class Compare(left: EXPR, ops: List[COMP_OP], comparators: List[EXPR]) extends EXPR {
      override var tpeOpt: Option[ESType] = Some(ESBoolean)
      override val hash: Hash = listRootHash(left.hash +: (ops.map(_.hash) ++ comparators.map(_.hash)))
    }
    //TODO: Add ESType to hash
    case class Call(func: EXPR, args: List[EXPR], keywords: List[Keyword], override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(func.hash +: (args.map(_.hash) ++ keywords.map(_.hash)))
    }

    sealed trait Num extends EXPR

    case class IntConst(n: Int) extends EXPR with Num {
      var tpeOpt: Option[ESType] = Some(ESInt)
      override val hash: Hash = Blake2b256(n.toString)
    }
    case class LongConst(n: Long) extends EXPR with Num {
      var tpeOpt: Option[ESType] = Some(ESLong)
      override val hash: Hash = Blake2b256(n.toString)
    }

    sealed trait Bool extends EXPR

    case object True extends EXPR with Bool {
      override var tpeOpt: Option[ESType] = Some(ESBoolean)
      override val hash: Hash = Blake2b256("BOOL_TRUE")
    }
    case object False extends EXPR with Bool {
      override var tpeOpt: Option[ESType] = Some(ESBoolean)
      override val hash: Hash = Blake2b256("BOOL_FALSE")
    }

    case class Str(s: String) extends EXPR {
      override var tpeOpt: Option[ESType] = Some(ESString)
      override val hash: Hash = Blake2b256(s)
    }

    case class Base58Str(s: String) extends EXPR {
      override var tpeOpt: Option[ESType] = Some(ESByteVector)
      override val hash: Hash = Blake2b256(hash)
    }

    // The following expression can appear in assignment context
    case class Attribute(value: EXPR, attr: Identifier, ctx: EXPR_CTX, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(List(value.hash, attr.hash, ctx.hash))
    }
    case class Subscript(value: EXPR, slice: SLICE, ctx: EXPR_CTX, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(List(value.hash, slice.hash, ctx.hash))
    }
    case class Name(id: Identifier, ctx: EXPR_CTX, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(List(id.hash, ctx.hash))
    }

    case class ESDictNode(keys: List[EXPR], values: List[EXPR], override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(keys.map(_.hash) ++ values.map(_.hash))
    }
    case class ESSet(elts: List[EXPR], override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(elts.map(_.hash))
    }
    case class ESList(elts: List[EXPR], ctx: EXPR_CTX, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(elts.map(_.hash))
    }
    case class ESTuple(elts: List[EXPR], ctx: EXPR_CTX, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(elts.map(_.hash))
    }

    sealed trait Transformer extends EXPR

    case class SizeOf(coll: EXPR) extends EXPR with Transformer {
      override var tpeOpt: Option[ESType] = Some(ESInt)
      override val hash: Hash = coll.hash
    }

    case class Exists(coll: EXPR, predicate: EXPR) extends EXPR with Transformer {
      override var tpeOpt: Option[ESType] = Some(ESBoolean)
      override val hash: Hash = listRootHash(List(coll.hash, predicate.hash))
    }

    case class Sum(coll: EXPR, override var tpeOpt: Option[ESType] = None) extends EXPR with Transformer {
      override val hash: Hash = coll.hash
    }

    case class Map(coll: EXPR, func: EXPR, override var tpeOpt: Option[ESType] = None) extends EXPR with Transformer {
      override val hash: Hash = listRootHash(List(coll.hash, func.hash))
    }

    case class IsDefined(opt: EXPR) extends EXPR with Transformer {
      override var tpeOpt: Option[ESType] = Some(ESBoolean)
      override val hash: Hash = opt.hash
    }

    case class Get(opt: EXPR, override var tpeOpt: Option[ESType] = None) extends EXPR with Transformer {
      override val hash: Hash = opt.hash
    }

    case class Declaration(target: EXPR, typeOpt: Option[TypeIdentifier]) extends EXPR {
      var tpeOpt: Option[ESType] = Some(ESUnit)
      override val hash: Hash = listRootHash(List(target.hash, typeOpt.map(_.hash).getOrElse(Array.emptyByteArray)))
    }

    case class TypeMatching(name: Identifier, tipe: TypeIdentifier) extends EXPR {
      var tpeOpt: Option[ESType] = Some(ESUnit)
      override val hash: Hash = listRootHash(List(name.hash, tipe.hash))
    }

    case class SchemaMatching(name: Identifier, schemaId: Identifier) extends EXPR {
      var tpeOpt: Option[ESType] = Some(ESUnit)
      override val hash: Hash = listRootHash(List(name.hash, schemaId.hash))
    }

    // Used to define default condition in `case` branch.
    case object GenericCond extends EXPR {
      override var tpeOpt: Option[ESType] = Some(ESUnit)
      override val hash: Hash = Blake2b256("EXPR_GENERIC_COND")
    }
  }

  // col_offset is the byte offset in the utf8 string the parser uses
  case class Attributes(lineno: Int, col_offset: Int)

  sealed trait EXPR_CTX {
    val hash: Hash
  }

  object EXPR_CTX {

    case object Load extends EXPR_CTX {
      override val hash: Hash = Blake2b256("EXPR_CTX_LOAD")
    }
    case object Store extends EXPR_CTX {
      override val hash: Hash = Blake2b256("EXPR_CTX_STORE")
    }
    case object Param extends EXPR_CTX {
      override val hash: Hash = Blake2b256("EXPR_CTX_PARAM")
    }

    case object AugLoad extends EXPR_CTX {
      override val hash: Hash = Blake2b256("EXPR_CTX_AUGLOAD")
    }
    case object AugStore extends EXPR_CTX {
      override val hash: Hash = Blake2b256("EXPR_CTX_AUGSTORE")
    }
  }

  sealed trait SLICE extends AST_NODE

  object SLICE {

    case object Ellipsis extends SLICE {
      override val hash: Hash = Blake2b256("ELLIPSIS")
    }
    case class Slice(lower: Option[EXPR], upper: Option[EXPR], step: Option[EXPR]) extends SLICE {
      override val hash: Hash = Blake2b256("OPERATOR_ADD")
    }
    case class ExtSlice(dims: List[SLICE]) extends SLICE {
      override val hash: Hash = Blake2b256("OPERATOR_ADD")
    }
    case class Index(value: EXPR) extends SLICE {
      override val hash: Hash = Blake2b256("OPERATOR_ADD")
    }
  }

  sealed trait BOOL_OP {
    val hash: Hash
  }

  object BOOL_OP {
    case object And extends BOOL_OP {
      override val hash: Hash = Blake2b256("BOOL_OP_AND")
    }
    case object Or extends BOOL_OP {
      override val hash: Hash = Blake2b256("BOOL_OP_OR")
    }
  }

  sealed trait OPERATOR extends AST_NODE

  case object OPERATOR {
    case object Add extends OPERATOR {
      override val hash: Hash = Blake2b256("OPERATOR_ADD")
    }
    case object Sub  extends OPERATOR{
      override val hash: Hash = Blake2b256("OPERATOR_SUB")
    }
    case object Mult  extends OPERATOR{
      override val hash: Hash = Blake2b256("OPERATOR_MULT")
    }
    case object Div  extends OPERATOR{
      override val hash: Hash = Blake2b256("OPERATOR_DIV")
    }
    case object Mod  extends OPERATOR{
      override val hash: Hash = Blake2b256("OPERATOR_MOD")
    }
    case object Pow  extends OPERATOR{
      override val hash: Hash = Blake2b256("OPERATOR_POW")
    }
    case object FloorDiv extends OPERATOR{
      override val hash: Hash = Blake2b256("OPERATOR_FLOORDIV")
    }
  }

  sealed trait UNARY_OP {
    val hash: Hash
  }

  object UNARY_OP {

    case object Invert extends UNARY_OP {
      override val hash: Hash = Blake2b256("UNARY_OP_INVERT")
    }
    case object Not extends UNARY_OP {
      override val hash: Hash = Blake2b256("UNARY_OP_NOT")
    }
    case object UAdd extends UNARY_OP {
      override val hash: Hash = Blake2b256("UNARY_OP_UADD")
    }
    case object USub extends UNARY_OP {
      override val hash: Hash = Blake2b256("UNARY_OP_USUB")
    }
  }

  sealed trait COMP_OP {
    val hash: Hash
  }

  object COMP_OP {

    case object Eq extends COMP_OP {
      override val hash: Hash = Blake2b256("COMP_OP_EQ")
    }
    case object NotEq extends COMP_OP {
      override val hash: Hash = Blake2b256("COMP_OP_NOT_EQ")
    }
    case object Lt extends COMP_OP {
      override val hash: Hash = Blake2b256("COMP_OP_LT")
    }
    case object LtE extends COMP_OP {
      override val hash: Hash = Blake2b256("COMP_OP_LTE")
    }
    case object Gt extends COMP_OP {
      override val hash: Hash = Blake2b256("COMP_OP_GT")
    }
    case object GtE extends COMP_OP {
      override val hash: Hash = Blake2b256("COMP_OP_GTE")
    }
    case object Is extends COMP_OP {
      override val hash: Hash = Blake2b256("COMP_OP_IS")
    }
    case object IsNot extends COMP_OP {
      override val hash: Hash = Blake2b256("COMP_OP_ISNOT")
    }
    case object In extends COMP_OP {
      override val hash: Hash = Blake2b256("COMP_OP_IN")
    }
    case object NotIn extends COMP_OP {
      override val hash: Hash = Blake2b256("COMP_OP_NOTIN")
    }
  }

  // not sure what to call the first argument for raise and except
  sealed trait EXCP_HANDLER extends AST_NODE
  object EXCP_HANDLER {

    case class ExceptHandler(`type`: Option[EXPR], name: Option[EXPR], body: List[STMT]) extends EXCP_HANDLER {
      override val hash: Hash = listRootHash(
        List(`type`.map(_.hash).getOrElse(Array.emptyByteArray), name.map(_.hash).getOrElse(Array.emptyByteArray)) ++ body.map(_.hash)
      )
    }
  }

  case class Identifier(name: String){
    val hash: Array[Byte] = Blake2b256(name)
  }

  case class TypeIdentifier(ident: Identifier, typeParams: List[Identifier]) {
    val hash: Array[Byte] = typeParams.foldLeft(ident.hash){
      case (resultHash, typeParam) => Blake2b256(resultHash ++ typeParam.hash)
    }
  }

  case class Arguments(args: List[(Identifier, TypeIdentifier)]) extends AST_NODE {
    override val hash: Hash = args.foldLeft(Array.emptyByteArray){
      case (argumentsHash, argument) => Blake2b256(argumentsHash ++ argument._1.hash ++ argument._2.hash)
    }
  }

  // keyword arguments supplied to call
  case class Keyword(arg: Identifier, value: EXPR) extends AST_NODE {
    override val hash: Hash = listRootHash(List(arg.hash ++ value.hash))
  }

  // import name with optional 'as' alias.
  case class Alias(name: Identifier, asname: Option[Identifier]) extends AST_NODE {
    override val hash: Hash = listRootHash(List(name.hash ++ asname.map(_.hash).getOrElse(Array.emptyByteArray)))
  }
}
