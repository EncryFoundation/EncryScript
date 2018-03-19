package encrywm.frontend.serializer

import com.google.common.primitives.{Bytes, Ints, Shorts}
import encrywm.frontend.ast.Ast
import encrywm.frontend.ast.Ast.EXCP_HANDLER.ExceptHandler
import encrywm.frontend.ast.Ast.TREE_ROOT._
import encrywm.frontend.ast.Ast.EXPR._
import encrywm.frontend.ast.Ast.SLICE.{Ellipsis, ExtSlice, Index, Slice}
import encrywm.frontend.ast.Ast.STMT._
import encrywm.frontend.ast.Ast.OPERATOR._
import encrywm.frontend.ast.Ast._
import scorex.crypto.encode.Base58

object ContractSerializer extends TreeNodeSerializer[Contract] {

  override def toBytes(tree: Contract): Array[Byte] = Bytes.concat(
      Array(prefix),
      Shorts.toByteArray(tree.body.length.toShort),
      tree.body.foldLeft(Array.emptyByteArray)( (arr, stmt) => arr ++ STMTSerializer.toBytes(stmt))
    )

  override def fromBytes(bytes: Array[Byte]): Option[Contract] = {
    val stmtQty = Shorts.fromByteArray(bytes.slice(0, 2))
    var sp = 2
    val body = (0 until stmtQty).foldLeft(Seq[STMT]()){
      case (seq, _) =>
        val length = Shorts.fromByteArray(bytes.slice(sp, sp + 2))
        val res = seq :+ AstSerializer.fromBytes(bytes.slice(sp, sp + length + 3)).get.asInstanceOf[STMT]
        sp += 3 + length
        res
    }
    Option(Contract(body.toList))
  }

  val prefix: Byte = 3
}

object ExpressionSerializer extends TreeNodeSerializer[Expression] {

  override def toBytes(tree: Expression): Array[Byte] = Bytes.concat(
    Array(prefix),
    Shorts.toByteArray(tree.body.length.toShort),
    tree.body.foldLeft(Array[Byte]())( (arr, stmt) => arr ++ STMTSerializer.toBytes(stmt))
  )

  override def fromBytes(bytes: Array[Byte]): Option[Expression] = {
    val stmtQty = Shorts.fromByteArray(bytes.slice(0, 2))
    var sp = 2
    val body = (0 until stmtQty).foldLeft(Seq[STMT]()){
      case (seq, _) =>
        val length = Shorts.fromByteArray(bytes.slice(sp, sp + 2))
        val res = seq :+ AstSerializer.fromBytes(bytes.slice(sp, sp + length + 3)).get.asInstanceOf[STMT]
        sp += 3 + length
        res
    }
    Option(Expression(body.toList))
  }
  val prefix: Byte = 4
}

object FunctionDefSerializer extends TreeNodeSerializer[FunctionDef] {

  override def toBytes(tree: FunctionDef): Array[Byte] = {
    val decodeName = Base58.decode(tree.name.name).get
    val returnType = Base58.decode(tree.returnType.name).get
    val desArgs = ArgumentsSerializer.toBytes(tree.args)
    Bytes.concat(
      Array(prefix),
      Ints.toByteArray(decodeName.length),
      decodeName,
      Ints.toByteArray(desArgs.length),
      desArgs,
      Shorts.toByteArray(tree.body.length.toShort),
      tree.body.foldLeft(Array[Byte]())((arr, stmt) => arr ++ AstSerializer.toBytes(stmt)),
      returnType,
    )
  }

  override def fromBytes(bytes: Array[Byte]): Option[FunctionDef] = {
    val nameLength = Ints.fromByteArray(bytes.slice(0, 4))
    val name = Identifier(Base58.encode(bytes.slice(4, 4 + nameLength)))
    val argsLength = Ints.fromByteArray(bytes.slice(4 + nameLength, 8 + nameLength))
    val args = ArgumentsSerializer.fromBytes(bytes.slice(8 + nameLength, 8 + nameLength + argsLength)).get
    val stmtsQty = Shorts.fromByteArray(bytes.slice(8 + nameLength + argsLength, 10 + nameLength + argsLength))
    var sp = 10 + nameLength + argsLength
    val stmts = (0 until stmtsQty).foldLeft(Seq[STMT]()){
      (seq, i) => {
        val stmtLength = Shorts.fromByteArray(bytes.slice(sp, sp + 2))
        val stmt = AstSerializer.fromBytes(bytes.slice(sp, sp + stmtLength + 3)).get.asInstanceOf[STMT]
        sp += 3 + stmtLength
        seq :+ stmt
      }
    }
    val returnType = Identifier(Base58.encode(bytes.slice(sp, bytes.length)))
    Option(FunctionDef(name, args, stmts.toList, returnType))
  }

  val prefix: Byte = 5
}

object ReturnSerializer extends TreeNodeSerializer[Return] {

  override def toBytes(tree: Return): Array[Byte] = {
    Bytes.concat(
      Array(prefix),
      tree.value match {
        case Some(v) => EXPRSerializer.toBytes(v)
        case None => Array.emptyByteArray
      }
    )
  }

  override def fromBytes(bytes: Array[Byte]): Option[Return] = {
    val retBytes = bytes.slice(2, bytes.length)
    if(retBytes.isEmpty) None else Option(Return(EXPRSerializer.fromBytes(retBytes)))
  }

  val prefix: Byte = 6
}

object AssignSerializer extends TreeNodeSerializer[Assign] {

  override def toBytes(tree: Assign): Array[Byte] = ???

  override def fromBytes(bytes: Array[Byte]): Option[Assign] = ???

  val prefix: Byte = 7
}

object AugAssignSerializer extends TreeNodeSerializer[AugAssign] {

  override def toBytes(tree: AugAssign): Array[Byte] = ???

  override def fromBytes(bytes: Array[Byte]): Option[AugAssign] = ???

  val prefix: Byte = 8
}

object ForSerializer extends TreeNodeSerializer[For]{

  override def toBytes(tree: For): Array[Byte] = ???

  override def fromBytes(bytes: Array[Byte]): Option[For] = ???

  val prefix: Byte = 10
}

object IfSerializer extends TreeNodeSerializer[If]{

  override def toBytes(tree: If): Array[Byte] = ???

  override def fromBytes(bytes: Array[Byte]): Option[If] = ???

  val prefix: Byte = 11
}

object AssertSerializer extends TreeNodeSerializer[Assert] {

  override def toBytes(tree: Assert): Array[Byte] = ???

  override def fromBytes(bytes: Array[Byte]): Option[Assert] = ???

  val prefix: Byte = 12
}

object ExprSerializer extends TreeNodeSerializer[Expr]{

  override def toBytes(tree: Expr): Array[Byte] = ???

  override def fromBytes(bytes: Array[Byte]): Option[Expr] = ???

  val prefix: Byte = 13
}

object BoolOpSerializer extends TreeNodeSerializer[BoolOp]{

  override def toBytes(tree: BoolOp) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 14
}

object BinOpSerializer extends TreeNodeSerializer[BinOp]{

  override def toBytes(tree: BinOp) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 15
}

object UnaryOpSerializer extends TreeNodeSerializer[UnaryOp]{

  override def toBytes(tree: UnaryOp) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 16
}

object LambdaSerializer extends TreeNodeSerializer[Lambda]{

  override def toBytes(tree: Lambda) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 17
}

object IfExpSerializer extends TreeNodeSerializer[IfExp]{

  override def toBytes(tree: IfExp) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 18
}

object CompareSerializer extends TreeNodeSerializer[Compare]{

  override def toBytes(tree: Compare) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 19
}

object CallSerializer extends TreeNodeSerializer[Call]{

  override def toBytes(tree: Call) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 20
}

object IntConstSerializer extends TreeNodeSerializer[IntConst]{

  override def toBytes(tree: IntConst) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 21
}

object LongConstSerializer extends TreeNodeSerializer[LongConst]{

  override def toBytes(tree: LongConst) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 22
}

object FloatConstSerializer extends TreeNodeSerializer[FloatConst]{

  override def toBytes(tree: FloatConst) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 23
}

object DoubleConstSerializer extends TreeNodeSerializer[DoubleConst]{

  override def toBytes(tree: DoubleConst) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 24
}

object StrSerializer extends TreeNodeSerializer[Str]{

  override def toBytes(tree: Str) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 25
}

object AttributeSerializer extends TreeNodeSerializer[Attribute]{

  override def toBytes(tree: Attribute) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 26
}

object SubscriptSerializer extends TreeNodeSerializer[Subscript]{

  override def toBytes(tree: Subscript) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 27
}

object NameSerializer extends TreeNodeSerializer[Name]{

  override def toBytes(tree: Name) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 28
}

object DictSerializer extends TreeNodeSerializer[Dict]{

  override def toBytes(tree: Dict) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 29
}

object SetSerializer extends TreeNodeSerializer[ESet]{

  override def toBytes(tree: ESet) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 30
}

object ListSerializer extends TreeNodeSerializer[EList]{

  override def toBytes(tree: EList) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 31
}

object TupleSerializer extends TreeNodeSerializer[Tuple]{

  override def toBytes(tree: Tuple) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 32
}

object DeclSerializer extends TreeNodeSerializer[Decl]{

  override def toBytes(tree: Decl) = {
    val bytes = EXPRSerializer.toBytes(tree.target) ++ Base58.decode(tree.typeOpt.get.name).getOrElse(Array.emptyByteArray)
    Bytes.concat(
      Shorts.toByteArray(bytes.length.toShort),
      Array(prefix),
      bytes
    )
  }

  override def fromBytes(bytes: Array[Byte]): Option[Decl] = {
    val exprLength = Shorts.fromByteArray(bytes.slice(0, 2))
    val expr = EXPRSerializer.fromBytes(bytes.slice(2, 2 + exprLength)).get
    val typeOpt = Option(Identifier(Base58.encode(bytes.slice(2 + exprLength, bytes.length))))
    Option(Decl(expr, typeOpt))
  }

  val prefix: Byte = 33
}

object EllipsisSerializer extends TreeNodeSerializer[Ellipsis.type]{

  override def toBytes(tree: SLICE.Ellipsis.type) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 34
}

object SliceSerializer extends TreeNodeSerializer[Slice]{

  override def toBytes(tree: Slice) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 35
}

object ExtSliceSerializer extends TreeNodeSerializer[ExtSlice]{

  override def toBytes(tree: ExtSlice) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 36
}

object IndexSerializer extends TreeNodeSerializer[Index]{

  override def toBytes(tree: Index) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 37
}

object AddSerializer extends TreeNodeSerializer[Add.type]{

  override def toBytes(tree: OPERATOR.Add.type) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 38
}

object SubSerializer extends TreeNodeSerializer[Sub.type]{

  override def toBytes(tree: OPERATOR.Sub.type) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 39
}

object MultSerializer extends TreeNodeSerializer[Mult.type]{

  override def toBytes(tree: OPERATOR.Mult.type) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 40
}

object DivSerializer extends TreeNodeSerializer[Div.type]{

  override def toBytes(tree: OPERATOR.Div.type) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 41
}

object ModSerializer extends TreeNodeSerializer[Mod.type]{

  override def toBytes(tree: OPERATOR.Mod.type) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 42
}

object PowSerializer extends TreeNodeSerializer[Pow.type]{

  override def toBytes(tree: OPERATOR.Pow.type) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 43
}

object FloorDivSerializer extends TreeNodeSerializer[FloorDiv.type]{

  override def toBytes(tree: OPERATOR.FloorDiv.type) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 44
}

object ExceptHandlerSerializer extends TreeNodeSerializer[ExceptHandler]{

  override def toBytes(tree: ExceptHandler) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 45
}

object ArgumentsSerializer extends TreeNodeSerializer[Arguments]{

  override def toBytes(tree: Arguments): Array[Byte] = {
    Bytes.concat(
      Shorts.toByteArray(tree.args.length.toShort),
      tree.args.foldLeft(Array[Byte]())(_ ++ DeclSerializer.toBytes(_))
    )
  }

  override def fromBytes(bytes: Array[Byte]) = {
    
    val exprQty = Shorts.fromByteArray(bytes.slice(0, 2))
    
    var sp = 2
    val exprs = (0 until exprQty).foldLeft(Seq[EXPR.Decl]()){
      (seq, i) => {
        val exprLength = Shorts.fromByteArray(bytes.slice(sp, sp + 2))
        
        val expr = DeclSerializer.fromBytes(bytes.slice(sp + 2, exprLength)).get
        
        sp += 2 + exprLength
        seq :+ expr
      }
    }
    Option(Arguments(exprs.toList))
  }

  val prefix: Byte = 46
}

object KeywordSerializer extends TreeNodeSerializer[Keyword]{

  override def toBytes(tree: Keyword) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 47
}

object AliasSerializer extends TreeNodeSerializer[Alias]{

  override def toBytes(tree: Alias) = ???

  override def fromBytes(bytes: Array[Byte]) = ???

  val prefix: Byte = 48
}

object STMTSerializer extends TreeNodeSerializer[STMT]{

  override def toBytes(tree: STMT) = {
    val bodyBytes = tree match {
      case fd: FunctionDef =>FunctionDefSerializer.toBytes(fd)
      case r: Return => ReturnSerializer.toBytes(r)
      case assign: Assign => AssignSerializer.toBytes(assign)
      case augAssign: AugAssign => AugAssignSerializer.toBytes(augAssign)
      case f: For => ForSerializer.toBytes(f)
      case i: If => IfSerializer.toBytes(i)
      case assert: Assert => AssertSerializer.toBytes(assert)
    }
    Bytes.concat(
      Shorts.toByteArray(bodyBytes.length.toShort),
      Array(prefix),
      bodyBytes
    )
  }

  override def fromBytes(bytes: Array[Byte]) = {
    bytes.head match {
      case FunctionDefSerializer.prefix => {
        
        
        FunctionDefSerializer.fromBytes(bytes.slice(1, bytes.length))
      }
      case ReturnSerializer.prefix => ReturnSerializer.fromBytes(bytes.slice(1, bytes.length))
      case AssignSerializer.prefix => AssignSerializer.fromBytes(bytes.slice(1, bytes.length))
      case AugAssignSerializer.prefix => AugAssignSerializer.fromBytes(bytes.slice(1, bytes.length))
      case ForSerializer.prefix => ForSerializer.fromBytes(bytes.slice(1, bytes.length))
      case IfSerializer.prefix => IfSerializer.fromBytes(bytes.slice(1, bytes.length))
      case AssertSerializer.prefix => AssertSerializer.fromBytes(bytes.slice(1, bytes.length))
      case _ => None
    }
  }

  val prefix: Byte = 0

}

object EXPRSerializer extends TreeNodeSerializer[EXPR]{

  override def toBytes(tree: EXPR): Array[Byte] = {
    val bodyBytes = tree match {
      case boolOp: BoolOp => BoolOpSerializer.toBytes(boolOp)
      case binOp: BinOp => BinOpSerializer.toBytes(binOp)
      case unaryOp: UnaryOp => UnaryOpSerializer.toBytes(unaryOp)
      case lambda: Lambda => LambdaSerializer.toBytes(lambda)
      case ifExp: IfExp => IfExpSerializer.toBytes(ifExp)
      case compare: Compare => CompareSerializer.toBytes(compare)
      case call: Call => CallSerializer.toBytes(call)
      case intConst: IntConst => IntConstSerializer.toBytes(intConst)
      case longConst: LongConst => LongConstSerializer.toBytes(longConst)
      case floatConst: FloatConst => FloatConstSerializer.toBytes(floatConst)
      case doubleConst: DoubleConst => DoubleConstSerializer.toBytes(doubleConst)
      case str: Str => StrSerializer.toBytes(str)
      case attribute: Attribute => AttributeSerializer.toBytes(attribute)
      case subscript: Subscript => SubscriptSerializer.toBytes(subscript)
      case name: Name => NameSerializer.toBytes(name)
      case dict: Dict => DictSerializer.toBytes(dict)
      case set: ESet => SetSerializer.toBytes(set)
      case list: EList => ListSerializer.toBytes(list)
      case tuple: Tuple => TupleSerializer.toBytes(tuple)
      case _ => Array.emptyByteArray
    }
    Bytes.concat(
      Shorts.toByteArray(bodyBytes.length.toShort),
      Array(prefix),
      bodyBytes)
  }

  override def fromBytes(bytes: Array[Byte]): Option[Ast.EXPR] = bytes.head match {
    case BoolOpSerializer.prefix => BoolOpSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case BinOpSerializer.prefix => BinOpSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case UnaryOpSerializer.prefix => UnaryOpSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case LambdaSerializer.prefix => LambdaSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case IfExpSerializer.prefix => IfExpSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case CompareSerializer.prefix => CompareSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case CallSerializer.prefix => CallSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case IntConstSerializer.prefix => IntConstSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case LongConstSerializer.prefix => LongConstSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case FloatConstSerializer.prefix => FloatConstSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case DoubleConstSerializer.prefix => DoubleConstSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case StrSerializer.prefix => StrSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case AttributeSerializer.prefix => AttributeSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case SubscriptSerializer.prefix => SubscriptSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case NameSerializer.prefix => NameSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case DictSerializer.prefix => DictSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case SetSerializer.prefix => SetSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case ListSerializer.prefix => ListSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case TupleSerializer.prefix => TupleSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case _ => None
  }

  val prefix: Byte = 1
}

object TREE_ROOTSerializer extends TreeNodeSerializer[TREE_ROOT]{

  override def toBytes(tree: TREE_ROOT) = {
    val bodyBytes = tree match {
      case contract: Contract => ContractSerializer.toBytes(contract)
      case expression: Expression => ExpressionSerializer.toBytes(expression)
    }
    Bytes.concat(
      Shorts.toByteArray(bodyBytes.length.toShort),
      Array(prefix),
      bodyBytes)
  }

  override def fromBytes(bytes: Array[Byte]): Option[TREE_ROOT] = bytes.head match {
    case ContractSerializer.prefix => ContractSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
    case ExpressionSerializer.prefix => ExpressionSerializer.fromBytes(bytes.slice(1, bytes.length - 1))
  }

  val prefix: Byte = 2
}

object AstSerializer{

  def toBytes(node: Ast.AST_NODE): Array[Byte] = node match {
    case tr: TREE_ROOT => TREE_ROOTSerializer.toBytes(tr)
    case stmt: STMT => STMTSerializer.toBytes(stmt)
    case expr: EXPR => EXPRSerializer.toBytes(expr)
    case _ => Array(-1: Byte)
  }

  def fromBytes(bytes: Array[Byte]): Option[Ast.AST_NODE] = {
    val body = bytes.slice(2, bytes.length)
    body.head match {
      case TREE_ROOTSerializer.prefix => TREE_ROOTSerializer.fromBytes(body.slice(1, body.length))
      case STMTSerializer.prefix => STMTSerializer.fromBytes(body.slice(1, body.length))
      case EXPRSerializer.prefix => EXPRSerializer.fromBytes(body.slice(1, body.length))
      case _ => None
    }
  }
}