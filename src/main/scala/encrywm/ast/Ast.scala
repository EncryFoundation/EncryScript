package encrywm.ast

import encrywm.utils.mast.Utils.listRootHash
import scorex.crypto.hash.Blake2b256

object Ast {

  import encrywm.lib.Types._

  type Hash =  Array[Byte]
  type VariableName = String

  trait Hashable {

    val hash: Hash
  }

  trait VariableContainable {

    val variables: List[VariableName]
  }

  sealed trait AST_NODE

  sealed trait TREE_ROOT extends AST_NODE with Hashable

  object TREE_ROOT {
    case class Contract(body: List[STMT]) extends TREE_ROOT {
      override val hash: Hash = listRootHash(body.map(_.hash))
    }
    case class Expression(body: List[STMT]) extends TREE_ROOT {
      override val hash: Hash = listRootHash(body.map(_.hash))
    }
  }

  sealed trait STMT extends Hashable with VariableContainable with AST_NODE

  object STMT {

    case class FunctionDef(name: Identifier, args: Arguments, body: List[STMT], returnType: Identifier) extends STMT {
      override val variables: List[VariableName] =
        body.foldLeft(List[String]()){ case (bodyVars, bodyElem) => bodyVars ++ bodyElem.variables }.toList ++ List(name.name)
      override val hash: Hash = listRootHash(name.hash +: args.hash +: body.map(_.hash) :+ returnType.hash)
    }
    
    case class Return(value: Option[EXPR]) extends STMT {
      override val variables: List[VariableName] = value.map(_.variables).getOrElse(List.empty[String])
      override val hash: Hash = value.map(_.hash).getOrElse(Array.emptyByteArray)
    }

    case class Let(target: EXPR, value: EXPR, global: Boolean = false) extends STMT {
      val variableName: String = target.variables.head
      override val variables: List[VariableName] = value.variables
      override val hash: Hash = listRootHash(List(target.hash, value.hash, if (global) Array(1: Byte) else Array(0: Byte)))
    }

    // Reassignment could be introduced later.
    case class AugAssign(target: EXPR, op: OPERATOR, value: EXPR) extends STMT {
      override val variables: List[VariableName] = List.empty[String]
      override val hash: Hash = listRootHash(List(target.hash, op.hash, value.hash))
    }

    // For loop could be introduced later.
    case class For(target: EXPR, iter: EXPR, body: List[STMT], orelse: List[STMT]) extends STMT {
      override val variables: List[VariableName] = List.empty[String]
      override val hash: Hash = listRootHash(target.hash +: iter.hash +: (body.map(_.hash) ++ orelse.map(_.hash)))
    }
    case class If(test: EXPR, body: List[STMT], orelse: List[STMT]) extends STMT {
      override val variables: List[VariableName] = test.variables ++
        body.foldLeft(List[String]()){ case (bodyVars, bodyElem) => bodyVars ++ bodyElem.variables } ++
          orelse.foldLeft(List[String]()){ case (orElseVars, bodyElem) => orElseVars ++ bodyElem.variables }
      override val hash: Hash = listRootHash(test.hash +: (body.map(_.hash) ++ orelse.map(_.hash)))
    }
    case class Match(target: EXPR, branches: List[STMT]) extends STMT {
      override val variables: List[VariableName] = target.variables
      override val hash: Hash = listRootHash(target.hash +: branches.map(_.hash))
    }
    case class Case(cond: EXPR, body: List[STMT], isDefault: Boolean = false) extends STMT {
      override val variables: List[VariableName] = List.empty[String]
      override val hash: Hash = listRootHash(cond.hash +: body.map(_.hash))
    }

    case class Assert(test: EXPR, msg: Option[EXPR]) extends STMT {
      override val variables: List[VariableName] = List.empty[String]
      override val hash: Hash = listRootHash(List(test.hash, msg.map(_.hash).getOrElse(Array.emptyByteArray)))
    }

    case class Expr(value: EXPR) extends STMT {
      override val variables: List[VariableName] = value.variables
      override val hash: Hash = value.hash
    }

    case class UnlockIf(test: EXPR) extends STMT {
      override val variables: List[VariableName] = test.variables
      override val hash: Hash = test.hash
    }
    case object Halt extends STMT {
      override val variables: List[VariableName] = List.empty[String]
      override val hash: Hash = Blake2b256("STMT_HALT")
    }
    case object Pass extends STMT {
      override val variables: List[VariableName] = List.empty[String]
      override val hash: Hash = Blake2b256("STMT_PASS")
    }

    // col_offset is the byte offset in the utf8 string the parser uses
    case class Attributes(lineno: Int, col_offset: Int)
  }

  sealed trait EXPR extends Hashable with VariableContainable with AST_NODE {
    var tpeOpt: Option[ESType]
  }

  object EXPR {

    case class BoolOp(op: BOOL_OP, values: List[EXPR]) extends EXPR {
      var tpeOpt: Option[ESType] = Some(ESBoolean)
      override val hash: Hash = listRootHash(op.hash +: values.map(_.hash))
      override val variables: List[VariableName] =
        values.foldLeft(List[String]()){ case (valueVars, value) => valueVars ++ value.variables }
    }

    case class BinOp(left: EXPR, op: OPERATOR, right: EXPR, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(List(left.hash, op.hash, right.hash))
      override val variables: List[VariableName] = left.variables ++ right.variables
    }

    case class UnaryOp(op: UNARY_OP, operand: EXPR, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(List(op.hash, operand.hash))
      override val variables: List[VariableName] = operand.variables
    }

    case class Lambda(args: Arguments, body: EXPR, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(List(args.hash, body.hash))
      override val variables: List[VariableName] = List.empty[String]
    }

    case class IfExp(test: EXPR, body: EXPR, orelse: EXPR, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(List(test.hash, body.hash, orelse.hash))
      override val variables: List[VariableName] = test.variables ++ body.variables ++ orelse.variables
    }

    // Sequences are required for compare to distinguish between
    // x < 4 < 3 and (x < 4) < 3
    case class Compare(left: EXPR, ops: List[COMP_OP], comparators: List[EXPR]) extends EXPR {
      override var tpeOpt: Option[ESType] = Some(ESBoolean)
      override val hash: Hash = listRootHash(left.hash +: (ops.map(_.hash) ++ comparators.map(_.hash)))
      override val variables: List[VariableName] =
        left.variables ++ comparators.foldLeft(List[String]()){ case (comparatorVars, comparator) => comparatorVars ++ comparator.variables }
    }

    //TODO: Add ESType to hash
    case class Call(func: EXPR, args: List[EXPR], keywords: List[Keyword], override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(func.hash +: (args.map(_.hash) ++ keywords.map(_.hash)))
      override val variables: List[VariableName] =
        func.variables ++ args.foldLeft(List[String]()){ case (argumentVars, argument) => argumentVars ++ argument.variables }
    }

    sealed trait Num extends EXPR

    case class IntConst(n: Int) extends EXPR with Num {
      var tpeOpt: Option[ESType] = Some(ESInt)
      override val hash: Hash = Blake2b256(n.toString)
      override val variables: List[VariableName] = List.empty[String]
    }
    case class LongConst(n: Long) extends EXPR with Num {
      var tpeOpt: Option[ESType] = Some(ESLong)
      override val hash: Hash = Blake2b256(n.toString)
      override val variables: List[VariableName] = List.empty[String]
    }

    sealed trait Bool extends EXPR

    case object True extends EXPR with Bool {
      override var tpeOpt: Option[ESType] = Some(ESBoolean)
      override val hash: Hash = Blake2b256("BOOL_TRUE")
      override val variables: List[VariableName] = List.empty[String]
    }
    case object False extends EXPR with Bool {
      override var tpeOpt: Option[ESType] = Some(ESBoolean)
      override val hash: Hash = Blake2b256("BOOL_FALSE")
      override val variables: List[VariableName] = List.empty[String]
    }

    case class Str(s: String) extends EXPR {
      override var tpeOpt: Option[ESType] = Some(ESString)
      override val hash: Hash = Blake2b256(s)
      override val variables: List[VariableName] = List.empty[String]
    }

    case class Base58Str(s: String) extends EXPR {
      override var tpeOpt: Option[ESType] = Some(ESByteVector)
      override val hash: Array[Byte] = Blake2b256(s)
      override val variables: List[VariableName] = List.empty[String]
    }

    // The following expression can appear in assignment context
    case class Attribute(value: EXPR, attr: Identifier, ctx: EXPR_CTX, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(List(value.hash, attr.hash, ctx.hash))
      override val variables: List[VariableName] = value.variables
    }
    case class Subscript(value: EXPR, slice: SLICE, ctx: EXPR_CTX, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(List(value.hash, slice.hash, ctx.hash))
      override val variables: List[VariableName] = List.empty[String]
    }
    case class Name(id: Identifier, ctx: EXPR_CTX, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(List(id.hash, ctx.hash))
      override val variables: List[VariableName] = List(id.name)
    }

    case class ESDictNode(keys: List[EXPR], values: List[EXPR], override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(keys.map(_.hash) ++ values.map(_.hash))
      override val variables: List[VariableName] =
        values.foldLeft(List[String]()){ case (valueVars, value) => valueVars ++ value.variables }
    }
    case class ESSet(elts: List[EXPR], override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(elts.map(_.hash))
      override val variables: List[VariableName] = elts.foldLeft(List[String]()){ case (eltsVars, elt) => eltsVars ++ elt.variables }
    }
    case class ESList(elts: List[EXPR], ctx: EXPR_CTX, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(elts.map(_.hash))
      override val variables: List[VariableName] = elts.foldLeft(List[String]()){ case (eltsVars, elt) => eltsVars ++ elt.variables }
    }
    case class ESTuple(elts: List[EXPR], ctx: EXPR_CTX, override var tpeOpt: Option[ESType] = None) extends EXPR {
      override val hash: Hash = listRootHash(elts.map(_.hash))
      override val variables: List[VariableName] = elts.foldLeft(List[String]()){ case (eltsVars, elt) => eltsVars ++ elt.variables }
    }

    sealed trait Transformer extends EXPR

    case class SizeOf(coll: EXPR) extends EXPR with Transformer {
      override var tpeOpt: Option[ESType] = Some(ESInt)
      override val hash: Hash = coll.hash
      override val variables: List[VariableName] = List.empty[String]
    }

    case class Exists(coll: EXPR, predicate: EXPR) extends EXPR with Transformer {
      override var tpeOpt: Option[ESType] = Some(ESBoolean)
      override val hash: Hash = listRootHash(List(coll.hash, predicate.hash))
      override val variables: List[VariableName] = List.empty[String]
    }

    case class Sum(coll: EXPR, override var tpeOpt: Option[ESType] = None) extends EXPR with Transformer {
      override val hash: Hash = coll.hash
      override val variables: List[VariableName] = List.empty[String]
    }

    case class Map(coll: EXPR, func: EXPR, override var tpeOpt: Option[ESType] = None) extends EXPR with Transformer {
      override val hash: Hash = listRootHash(List(coll.hash, func.hash))
      override val variables: List[VariableName] = List.empty[String]
    }

    case class IsDefined(opt: EXPR) extends EXPR with Transformer {
      override var tpeOpt: Option[ESType] = Some(ESBoolean)
      override val hash: Hash = opt.hash
      override val variables: List[VariableName] = List.empty[String]
    }

    case class Get(opt: EXPR, override var tpeOpt: Option[ESType] = None) extends EXPR with Transformer {
      override val hash: Hash = opt.hash
      override val variables: List[VariableName] = List.empty[String]
    }

    case class Declaration(target: EXPR, typeOpt: Option[TypeIdentifier]) extends EXPR {
      var tpeOpt: Option[ESType] = Some(ESUnit)
      override val hash: Array[Byte] = listRootHash(List(target.hash, typeOpt.map(_.hash).getOrElse(Array.emptyByteArray)))
      override val variables: List[VariableName] = target.variables
    }

    case class TypeMatching(name: Identifier, tipe: TypeIdentifier) extends EXPR {
      var tpeOpt: Option[ESType] = Some(ESUnit)
      override val hash: Hash = listRootHash(List(name.hash, tipe.hash))
      override val variables: List[VariableName] = List.empty[String]
    }

    case class SchemaMatching(name: Identifier, schemaId: Identifier) extends EXPR {
      var tpeOpt: Option[ESType] = Some(ESUnit)
      override val hash: Hash = listRootHash(List(name.hash, schemaId.hash))
      override val variables: List[VariableName] = List.empty[String]
    }

    // Used to define default condition in `case` branch.
    case object GenericCond extends EXPR {
      override var tpeOpt: Option[ESType] = Some(ESUnit)
      override val hash: Hash = Blake2b256("EXPR_GENERIC_COND")
      override val variables: List[VariableName] = List.empty[String]
    }
  }

  // col_offset is the byte offset in the utf8 string the parser uses
  case class Attributes(lineno: Int, col_offset: Int)

  sealed trait EXPR_CTX extends Hashable

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

  sealed trait SLICE extends Hashable

  object SLICE {

    case object Ellipsis extends SLICE {
      override val hash: Array[Byte] = Blake2b256("ELLIPSIS")
    }
    case class Slice(lower: Option[EXPR], upper: Option[EXPR], step: Option[EXPR]) extends SLICE {
      override val hash: Array[Byte] = Blake2b256("OPERATOR_ADD")
    }
    case class ExtSlice(dims: List[SLICE]) extends SLICE {
      override val hash: Array[Byte] = Blake2b256("OPERATOR_ADD")
    }
    case class Index(value: EXPR) extends SLICE {
      override val hash: Array[Byte] = Blake2b256("OPERATOR_ADD")
    }
  }

  sealed trait BOOL_OP extends Hashable

  object BOOL_OP {
    case object And extends BOOL_OP {
      override val hash: Hash = Blake2b256("BOOL_OP_AND")
    }
    case object Or extends BOOL_OP {
      override val hash: Hash = Blake2b256("BOOL_OP_OR")
    }
  }

  sealed trait OPERATOR extends Hashable

  case object OPERATOR {
    case object Add extends OPERATOR {
      override val hash: Hash = Blake2b256("OPERATOR_ADD")
    }
    case object Sub extends OPERATOR {
      override val hash: Hash = Blake2b256("OPERATOR_SUB")
    }
    case object Mult  extends OPERATOR {
      override val hash: Hash = Blake2b256("OPERATOR_MULT")
    }
    case object Div  extends OPERATOR {
      override val hash: Hash = Blake2b256("OPERATOR_DIV")
    }
    case object Mod extends OPERATOR {
      override val hash: Hash = Blake2b256("OPERATOR_MOD")
    }
    case object Pow extends OPERATOR {
      override val hash: Hash = Blake2b256("OPERATOR_POW")
    }
  }

  sealed trait UNARY_OP extends Hashable

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

  sealed trait COMP_OP extends Hashable

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
  sealed trait EXCP_HANDLER extends AST_NODE with Hashable
  object EXCP_HANDLER {

    case class ExceptHandler(`type`: Option[EXPR], name: Option[EXPR], body: List[STMT]) extends EXCP_HANDLER {
      override val hash: Array[Byte] =
        listRootHash(
        List(`type`.map(_.hash).getOrElse(Array()), name.map(_.hash).getOrElse(Array())) ++ body.map(_.hash)
      )
    }
  }

  case class Identifier(name: String) extends Hashable {
    val hash: Array[Byte] = Blake2b256(name)
  }

  case class TypeIdentifier(ident: Identifier, typeParams: List[Identifier]) {
    val hash: Array[Byte] = typeParams.foldLeft(ident.hash){
      case (resultHash, typeParam) => Blake2b256(resultHash ++ typeParam.hash)
    }
  }

  case class Arguments(args: List[(Identifier, TypeIdentifier)]) extends Hashable with AST_NODE {
    override val hash: Hash = args.foldLeft(Array.emptyByteArray){
      case (argumentsHash, argument) => Blake2b256(argumentsHash ++ argument._1.hash ++ argument._2.hash)
    }
  }

  // keyword arguments supplied to call
  case class Keyword(arg: Identifier, value: EXPR) extends Hashable with AST_NODE {
    override val hash: Hash = listRootHash(List(arg.hash ++ value.hash))
  }

  // import name with optional 'as' alias.
  case class Alias(name: Identifier, asname: Option[Identifier]) extends Hashable with AST_NODE {
    override val hash: Hash = listRootHash(List(name.hash ++ asname.map(_.hash).getOrElse(Array.emptyByteArray)))
  }
}
