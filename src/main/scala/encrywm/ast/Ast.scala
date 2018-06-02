package encrywm.ast

object Ast {

  import encrywm.lib.Types._

  type Hash = Array[Byte]
  type VariableName = String

  trait VariableContainable {

    val variables: List[VariableName]
  }

  sealed trait AST_NODE

  sealed trait TREE_ROOT extends AST_NODE

  object TREE_ROOT {

    case class Contract(body: List[STMT]) extends TREE_ROOT {

      override def toString: VariableName =
        body.map(_.toString).tail.foldLeft(body.head.toString) {
          case (contract, stmt) => s"$contract\n$stmt"
        }
    }

    case class Expression(body: List[STMT]) extends TREE_ROOT {

      override def toString: VariableName =
        body.map(_.toString).tail.foldLeft(body.head.toString) {
          case (contract, stmt) => s"$contract\n$stmt"
      }
    }
  }

  sealed trait STMT extends VariableContainable with AST_NODE

  object STMT {

    case class FunctionDef(name: Identifier, args: Arguments, body: List[STMT], returnType: Identifier) extends STMT {

      override def toString: VariableName = s"def ${name.name}(" +
        s"${args.args.head._1.name}: ${args.args.head._2.ident.name}" +
        s"${args.args.tail.map(arg => s"${arg._1.name}: ${arg._2.ident.name}").fold("")(_ + ", " + _)}): -> ${returnType.name}:"
          .concat(body.map(_.toString).fold("")((funcBody, expr) => s"$funcBody \n $expr"))

      override val variables: List[VariableName] =
        body.foldLeft(List[String]()) { case (bodyVars, bodyElem) => bodyVars ++ bodyElem.variables } ++ List(name.name)
    }
    
    case class Return(value: Option[EXPR]) extends STMT {

      override def toString: VariableName = "return " + value.map(_.toString).getOrElse("")

      override val variables: List[VariableName] = value.map(_.variables).getOrElse(List.empty[String])
    }

    case class Let(target: EXPR, value: EXPR) extends STMT {

      val variableName: String = target.variables.head

      override def toString: VariableName = s"let $target = $value"

      override val variables: List[VariableName] = value.variables
    }

    case class If(test: EXPR, body: List[STMT], orelse: List[STMT]) extends STMT {

      override def toString: VariableName =
        body.foldLeft( s"if ($test):" ) {
          case (ifStmtBody, ifStmtBodyStmt) => s"$ifStmtBody\n$ifStmtBodyStmt"
        } ++
          (
            if (orelse.nonEmpty)
              orelse.foldLeft("else:"){
                case (orElseBody, orElseStmtBody) => s"$orElseBody\n$orElseStmtBody"
              }
            else ""
            )

      override val variables: List[VariableName] = test.variables ++
        body.foldLeft(List[String]()) { case (bodyVars, bodyElem) => bodyVars ++ bodyElem.variables } ++
          orelse.foldLeft(List[String]()) { case (orElseVars, bodyElem) => orElseVars ++ bodyElem.variables }
    }

    case class Match(target: EXPR, branches: List[STMT]) extends STMT {

      override def toString: VariableName = s"match $target:\n" + branches.tail.foldLeft(branches.head.toString)((resultStr, branch) => s"$resultStr\n$branch")

      override val variables: List[VariableName] = target.variables
    }

    case class Case(cond: EXPR, body: List[STMT], isDefault: Boolean = false) extends STMT {

      override def toString: VariableName = s"  case $cond:\n" + body.tail.foldLeft(body.head.toString)((caseBody, stmt) => s"$caseBody\n$stmt")

      override val variables: List[VariableName] = List.empty[String]
    }

    case class Assert(test: EXPR, msg: Option[EXPR]) extends STMT {

      override def toString: VariableName = "<assert_stmt>"

      override val variables: List[VariableName] = List.empty[String]
    }

    case class Expr(value: EXPR) extends STMT {

      override def toString: VariableName = value.toString

      override val variables: List[VariableName] = value.variables
    }

    case class UnlockIf(test: EXPR) extends STMT {

      override def toString: VariableName = s"unlock if $test"

      override val variables: List[VariableName] = test.variables
    }

    case object Halt extends STMT {

      override def toString: VariableName = "abort"

      override val variables: List[VariableName] = List.empty[String]
    }

    case object Pass extends STMT {

      override def toString: VariableName = "pass"

      override val variables: List[VariableName] = List.empty[String]
    }

    // col_offset is the byte offset in the utf8 string the parser uses
    case class Attributes(lineno: Int, col_offset: Int)
  }

  sealed trait EXPR extends VariableContainable with AST_NODE {
    var tipe: ESType
  }

  object EXPR {

    case class BoolOp(op: BOOL_OP, values: List[EXPR]) extends EXPR {

      var tipe: ESType = ESBoolean

      override def toString: VariableName =
        values.tail.foldLeft(values.head.toString)((str, expr) => str.concat(s" $op $expr"))

      override val variables: List[VariableName] =
        values.foldLeft(List[String]()){ case (valueVars, value) => valueVars ++ value.variables }
    }

    case class BinOp(left: EXPR, op: OPERATOR, right: EXPR, override var tipe: ESType = Nit) extends EXPR {

      override def toString: VariableName = s"$left $op $right"

      override val variables: List[VariableName] = left.variables ++ right.variables
    }

    case class UnaryOp(op: UNARY_OP, operand: EXPR, override var tipe: ESType = Nit) extends EXPR {

      override def toString: VariableName = s"$op($operand)"

      override val variables: List[VariableName] = operand.variables
    }

    case class Lambda(args: Arguments, body: EXPR, override var tipe: ESType = Nit) extends EXPR {

      override def toString: VariableName =
        s"lamb ( ${args.args.tail.foldLeft(s"${args.args.head._1}: ${args.args.head._2}")(
          (lambdaArguments, ident) => s"$lambdaArguments, ${ident._1}: ${ident._2.ident.name}")}) = $body"

      override val variables: List[VariableName] = List.empty[String]
    }

    case class IfExp(test: EXPR, body: EXPR, orelse: EXPR, override var tipe: ESType = Nit) extends EXPR {

      override def toString: VariableName = s"$body if $test else $orelse"

      override val variables: List[VariableName] = test.variables ++ body.variables ++ orelse.variables
    }

    // Sequences are required for compare to distinguish between
    // x < 4 < 3 and (x < 4) < 3
    case class Compare(left: EXPR, ops: List[COMP_OP], comparators: List[EXPR]) extends EXPR {

      override var tipe: ESType = ESBoolean

      override def toString: VariableName =
        s"$left ${ops.map(_.toString).zip(comparators.map(_.toString)).foldLeft("")((compareStmt, elem) => s"$compareStmt${elem._1} ${elem._2}")}"

      override val variables: List[VariableName] =
        left.variables ++ comparators.foldLeft(List[String]()){ case (comparatorVars, comparator) => comparatorVars ++ comparator.variables }
    }

    case class Call(func: EXPR, args: List[EXPR], keywords: List[Keyword], override var tipe: ESType = Nit) extends EXPR {

      override def toString: VariableName =
        s"$func (${args.tail.foldLeft(args.head.toString)((arguments, expr) => s"$arguments, $expr")})"

      override val variables: List[VariableName] =
        func.variables ++ args.foldLeft(List[String]()){ case (argumentVars, argument) => argumentVars ++ argument.variables }
    }

    sealed trait Num extends EXPR

    case class IntConst(n: Int) extends EXPR with Num {

      var tipe: ESType = ESInt

      override def toString: VariableName = n.toString

      override val variables: List[VariableName] = List.empty[String]
    }

    case class LongConst(n: Long) extends EXPR with Num {

      var tipe: ESType = ESLong

      override def toString: VariableName = n.toString

      override val variables: List[VariableName] = List.empty[String]
    }

    sealed trait Bool extends EXPR

    case object True extends EXPR with Bool {

      override var tipe: ESType = ESBoolean

      override def toString: VariableName = true.toString

      override val variables: List[VariableName] = List.empty[String]
    }

    case object False extends EXPR with Bool {

      override var tipe: ESType = ESBoolean

      override def toString: VariableName = false.toString

      override val variables: List[VariableName] = List.empty[String]
    }

    case class Str(s: String) extends EXPR {

      override var tipe: ESType = ESString

      override def toString: VariableName = '\"' + s + '\"'

      override val variables: List[VariableName] = List.empty[String]
    }

    case class Base58Str(s: String) extends EXPR {

      override var tipe: ESType = ESByteVector

      override def toString: VariableName = s"base58{$s}"

      override val variables: List[VariableName] = List.empty[String]
    }

    // The following expression can appear in assignment context
    case class Attribute(value: EXPR, attr: Identifier, override var tipe: ESType = Nit) extends EXPR {

      override def toString: VariableName = s"$value.${attr.name}"

      override val variables: List[VariableName] = value.variables
    }

    case class Subscript(value: EXPR, slice: SLICE, override var tipe: ESType = Nit) extends EXPR {

      override def toString: VariableName = "<subscript_expr>"

      override val variables: List[VariableName] = List.empty[String]
    }

    case class Name(id: Identifier, override var tipe: ESType = Nit) extends EXPR {

      override def toString: VariableName = id.name

      override val variables: List[VariableName] = List(id.name)
    }

    case class ESDictNode(keys: List[EXPR], values: List[EXPR], override var tipe: ESType = Nit) extends EXPR {

      override def toString: VariableName = super.toString

      override val variables: List[VariableName] =
        values.foldLeft(List[String]()){ case (valueVars, value) => valueVars ++ value.variables }
    }

    case class ESSet(elts: List[EXPR], override var tipe: ESType = Nit) extends EXPR {

      override def toString: VariableName =
        s"{${elts.drop(1).foldLeft(elts.head.toString)((str, expr) => s"$str, $expr")}}"

      override val variables: List[VariableName] = elts.foldLeft(List[String]()) { case (eltsVars, elt) => eltsVars ++ elt.variables }
    }

    case class ESList(elts: List[EXPR], override var tipe: ESType = Nit) extends EXPR {

      override def toString: VariableName =
        s"[${elts.drop(1).foldLeft(elts.head.toString)((str, expr) => s"$str, $expr")}]"

      override val variables: List[VariableName] = elts.foldLeft(List[String]()) { case (eltsVars, elt) => eltsVars ++ elt.variables }
    }

    case class ESTuple(elts: List[EXPR], override var tipe: ESType = Nit) extends EXPR {

      override def toString: VariableName =
        s"(${elts.drop(1).foldLeft(elts.head.toString)((str, expr) => s"$str, $expr")})"

      override val variables: List[VariableName] = elts.foldLeft(List[String]()) { case (eltsVars, elt) => eltsVars ++ elt.variables }
    }

    sealed trait Transformer extends EXPR

    case class SizeOf(coll: EXPR) extends EXPR with Transformer {

      override var tipe: ESType = ESInt

      override val variables: List[VariableName] = List.empty[String]
    }

    case class Exists(coll: EXPR, predicate: EXPR) extends EXPR with Transformer {

      override var tipe: ESType = ESBoolean

      override val variables: List[VariableName] = List.empty[String]
    }

    case class Sum(coll: EXPR, override var tipe: ESType = Nit) extends EXPR with Transformer {

      override val variables: List[VariableName] = List.empty[String]
    }

    case class Map(coll: EXPR, func: EXPR, override var tipe: ESType = Nit) extends EXPR with Transformer {

      override val variables: List[VariableName] = List.empty[String]
    }

    case class IsDefined(opt: EXPR) extends EXPR with Transformer {

      override var tipe: ESType = ESBoolean

      override val variables: List[VariableName] = List.empty[String]
    }

    case class Get(opt: EXPR, override var tipe: ESType = Nit) extends EXPR with Transformer {

      override val variables: List[VariableName] = List.empty[String]
    }

    case class Declaration(target: EXPR, typeOpt: Option[TypeIdentifier]) extends EXPR {

      override var tipe: ESType = ESUnit

      override def toString: VariableName = s"$target${typeOpt.map(ti => s": ${ti.ident.name}").getOrElse("")}"

      override val variables: List[VariableName] = target.variables
    }

    case class TypeMatching(name: Identifier, typeId: TypeIdentifier) extends EXPR {

      override var tipe: ESType = ESUnit

      override val variables: List[VariableName] = List.empty[String]
    }

    case class SchemaMatching(name: Identifier, schemaId: Identifier) extends EXPR {

      override var tipe: ESType = ESUnit

      override val variables: List[VariableName] = List.empty[String]
    }

    // Used to define default condition in `case` branch.
    case object GenericCond extends EXPR {

      override def toString: VariableName = "_"

      override var tipe: ESType = ESUnit

      override val variables: List[VariableName] = List.empty[String]
    }
  }

  // col_offset is the byte offset in the utf8 string the parser uses
  case class Attributes(lineno: Int, col_offset: Int)

  sealed trait SLICE

  object SLICE {

    case object Ellipsis extends SLICE

    case class Slice(lower: Option[EXPR], upper: Option[EXPR], step: Option[EXPR]) extends SLICE

    case class ExtSlice(dims: List[SLICE]) extends SLICE

    case class Index(value: EXPR) extends SLICE
  }

  sealed trait BOOL_OP

  object BOOL_OP {

    case object And extends BOOL_OP {

      override def toString: VariableName = "&&"
    }

    case object Or extends BOOL_OP {

      override def toString: VariableName = "||"
    }
  }

  sealed trait OPERATOR

  case object OPERATOR {

    case object Add extends OPERATOR {

      override def toString: VariableName = "+"
    }

    case object Sub extends OPERATOR {

      override def toString: VariableName = "-"
    }

    case object Mult  extends OPERATOR {

      override def toString: VariableName = "*"
    }

    case object Div  extends OPERATOR {

      override def toString: VariableName = "/"
    }

    case object Mod extends OPERATOR {

      override def toString: VariableName = "%"
    }

    case object Pow extends OPERATOR {

      override def toString: VariableName = "=="
    }
  }

  sealed trait UNARY_OP

  object UNARY_OP {

    case object Invert extends UNARY_OP {

      override def toString: VariableName = "~"
    }

    case object Not extends UNARY_OP {

      override def toString: VariableName = "!"
    }

    case object UAdd extends UNARY_OP {

      override def toString: VariableName = "+"
    }

    case object USub extends UNARY_OP {

      override def toString: VariableName = "-"
    }
  }

  sealed trait COMP_OP

  object COMP_OP {

    case object Eq extends COMP_OP {

      override def toString: VariableName = "=="
    }

    case object NotEq extends COMP_OP {

      override def toString: VariableName = "<>"
    }

    case object Lt extends COMP_OP {

      override def toString: VariableName = "<"
    }

    case object LtE extends COMP_OP {

      override def toString: VariableName = "<="
    }

    case object Gt extends COMP_OP {

      override def toString: VariableName = ">"
    }

    case object GtE extends COMP_OP {

      override def toString: VariableName = ">="
    }

    case object Is extends COMP_OP {

      override def toString: VariableName = "is"
    }

    case object IsNot extends COMP_OP {

      override def toString: VariableName = "is not"
    }

    case object In extends COMP_OP {

      override def toString: VariableName = "in"
    }

    case object NotIn extends COMP_OP {

      override def toString: VariableName = "not in"
    }
  }

  case class Identifier(name: String)

  case class TypeIdentifier(ident: Identifier, typeParams: List[Identifier])

  case class Arguments(args: List[(Identifier, TypeIdentifier)]) extends AST_NODE

  // keyword arguments supplied to call
  case class Keyword(arg: Identifier, value: EXPR) extends AST_NODE

  // import name with optional 'as' alias.
  case class Alias(name: Identifier, asname: Option[Identifier]) extends AST_NODE
}
