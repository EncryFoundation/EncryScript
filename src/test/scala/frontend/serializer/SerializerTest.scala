package frontend.serializer

import encrywm.frontend.ast.Ast.STMT.FunctionDef
import encrywm.frontend.ast.Ast.TREE_ROOT.{Contract, Expression}
import encrywm.frontend.serializer.AstSerializer
import org.scalatest.{Matchers, PropSpec}

class SerializerTest extends PropSpec with Matchers {

  property("FunctionDefSerializer") {

    val funcDef = Generator.generateSTMTs(3).head.asInstanceOf[FunctionDef]

    val fdBytes = AstSerializer.toBytes(funcDef)

    val fdDes = AstSerializer.fromBytes(fdBytes).get.asInstanceOf[FunctionDef]

    funcDef.name.name shouldEqual fdDes.name.name

  }

  property("ContractSerializer") {

    val stmts = Generator.generateSTMTs(3)

    val contract = Contract(stmts)

    val sContract = AstSerializer.toBytes(contract)

    val dsContract = AstSerializer.fromBytes(sContract).get.asInstanceOf[Contract]

    contract.body.length shouldEqual dsContract.body.length

  }

  property("ExpressionSerializer") {

    val stmts = Generator.generateSTMTs(3)

    val exp = Expression(stmts)

    val sExp = AstSerializer.toBytes(exp)

    val dsExp = AstSerializer.fromBytes(sExp).get.asInstanceOf[Expression]

    exp.body.length shouldEqual dsExp.body.length

  }

}
