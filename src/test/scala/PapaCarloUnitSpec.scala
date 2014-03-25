package codemodels.incrementalparsers.javaip

import name.lakhin.eliah.projects.papacarlo.syntax.Node
import org.scalatest._
import name.lakhin.eliah.projects.papacarlo.test.utils.ErrorMonitor

abstract class PapaCarloUnitSpec  extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors {
  // Helper methods : parsing

  def parse(code : String, f : Node => Any) {
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    val m = new ErrorMonitor(lexer,syntax)
    try{
    syntax.onNodeMerge.bind {node => {
      f(node)
    }}
    lexer.input(code)
    } finally {
      assert(0==syntax.getErrors.size,"There are syntax errors: "+m.getResult+". Code parsed '"+code+"'")
    }
  }

  def parseAndGetRoot(code : String) : Node = {
    var root : Option[Node] = None
    parse(code, node => root = Option(node))
    return root.get
  }

  def parseAndGetClassesList(code : String) : List[Node] = {
    var classes = List[Node]()
    parse(code,node => {
      val classNode = node.getBranches.get("classDeclaration").get.head
      classes ::= classNode
    })
    return classes
  }

  def parseAndGetClass(code : String) : Node = {
    var classes = List[Node]()
    parse(code,node => {
      if (!(node.getBranches contains "classDeclaration")){
        throw new RuntimeException("The node has not a classDeclaration: "+node.prettyPrint())
      }
      val classNode = node.getBranches.get("classDeclaration").get.head
      classes ::= classNode
    })
    assert(1==classes.size)
    return classes.head
  }

  def parseAndGetClassesMap(code : String) : Map[String,Node] = {
    var classes = Map[String,Node]()
    parse(code,node => {
      val classNode : Node = node.getBranches.get("classDeclaration").get.head
      classes += (classNode.getValues.get("name").get.head -> classNode)
    })
    return classes
  }

  def parseAndGetMember(memberCode : String) : Node = {
    val code = "class A { "+memberCode+" }"
    var members = List[Node]()
    parse(code,node => {
      members = getBranch(node,"classDeclaration").getBranches.get("members").get
    })
    assert(1==members.size)
    return members.head
  }

  def parseAndGetMethod(code : String) : Node = {
    return getBranch(parseAndGetMember(code),"method")
  }

  def parseAndGetField(code : String) : Node = {
    return getBranch(parseAndGetMember(code),"field")
  }

  def parseStmt(stmtCode : String) : Node = {
    val code = "class A { void foo(){"+stmtCode+"} }"
    var members = List[Node]()
    parse(code,node => {
      members = getBranches(getBranch(node,"classDeclaration"),"members")
    })

    assert(1==members.size)
    val m : Node = members.head.getBranches.get("method").get.head
    assert(1==m.getBranches.get("stmts").size,"No statement parsed, maybe there are parsing errors?")
    val s = m.getBranches.get("stmts").get.head
    return s
  }

  def parseExpr(exprCode : String) : Node = {
    val s = parseStmt(exprCode+";")
    return s.getBranches.get("expression").get.head
  }

  // Helper methods : assert

  def assertIsPrimitive(name:String,node: Node,arrayLevel: Int = 0) {
    assert(arrayLevel==getBranches(node,"array").size)
    assert("primitiveType"==getBranch(node,"baseType").getKind,"It is not a primitive type: "+node.prettyPrint())
    assert(name==getValue(getBranch(node,"baseType"),"name"))
  }

  def assertIsClass(name:String,node: Node,arrayLevel: Int = 0) {
    assert(arrayLevel==getBranches(node,"array").size)
    val baseType = getBranch(node,"baseType")
    assert("classType"==baseType.getKind)
    assert(name==getValue(baseType,"name"))
  }

  def assertQualId(parts:List[String],node: Node) {
    assert("qualifiedIdentifier"==node.getKind)
    assert(parts==node.getValues("part").reverse)
  }

  def assertIsMethodCall(methodName:String,node:Node) {
    assertNodeIs("expMethodCall", Map[String,String](), node)
    assertNodeIs("variableReference", Map[String,String]("name"->methodName), getBranch(node,"base"))
  }

  def toExpOp(node:Node) : Node = {
    assertNodeIs("expArrayAccess",Map[String,String](),node,"Expected an expArrayAccess(toExpOp method)")
    assert(!hasBranch(node,"index"))
    val node1 = getBranch(node,"value")
    if (node1.getKind=="expAccess"){
      assertNodeIs("expAccess",Map[String,String](),node1,"Expected an expAccess inside an expArrayAccess (toExpOp method)")
      assert(!hasValue(node1,"fieldName"))
      val node2 = getBranch(node1,"value")
      assertNodeIs("expOp",Map[String,String](),node2)
      return node2
    } else {
      assert(Array("+","-","/","*").contains(node1.getKind))
      return node1
    }
  }

  def assertIsIntegerLiteral(value:Int, node: Node){
    /*if (node.getKind=="expArrayAccess"){
      assertNodeIs("expArrayAccess",Map[String,String](),node)
      assert(!hasBranch(node,"index"))
      val node1 = getBranch(node,"value")
      return assertIsIntegerLiteral(value,node1)
    } else {*/
      /*assertNodeIs("expAccess",Map[String,String](),node,"Node expected to be an expAccess (assertIsIntegerLiteral)")
      assert(!hasValue(node,"fieldName"))
      val node2 = getBranch(node,"value")*/
      assertNodeIs("integerLiteral",Map[String,String]("value"->value.toString),node)
    //}
  }

  def canSimplify(node:Node) : Boolean = {
    if (node.getKind=="expArrayAccess" && !hasBranch(node,"index")) {
      return true;
    }
    if (node.getKind=="expAccess" && !hasValue(node,"fieldName")) {
      return true;
    }
    return false
  }

  def simplify(node:Node) : Node = {
    assert(canSimplify(node))
    if (node.getKind=="expArrayAccess" && !hasBranch(node,"index")) {
      val node1 = getBranch(node,"value")
      if (canSimplify(node1)){
        return simplify(node1)
      } else {
        return node1
      }
    }
    if (node.getKind=="expAccess" && !hasValue(node,"fieldName")) {
      val node1 = getBranch(node,"value")
      if (canSimplify(node1)){
        return simplify(node1)
      } else {
        return node1
      }
    }
    throw new RuntimeException("Strange...");
  }

  def assertNodeIs(kind:String,values:Map[String,String],node:Node,extraMsg:String = ""){
    if (kind!=node.getKind && canSimplify(node)){
      return assertNodeIs(kind,values,simplify(node),extraMsg)
    }
    assert(kind==node.getKind,"Expected "+kind+", actual kind is "+node.getKind+". "+extraMsg+". Node: "+node.prettyPrint())
    values.foreach { case (key,value)=> assert(value==getValue(node,key),"Value of "+key+" expected to be "+value+". Node: "+node.prettyPrint()) }
  }

  def assertAccessQualifier(name:String,node: Node){
    val qualifiers = getBranches(node,"qualifiers")
    assert(0<qualifiers.size,"No qualifiers, so no access qualifier "+name+" for "+node.prettyPrint())
    assert(None!=qualifiers.find(q=>hasBranch(q,"access") && getValue(getBranch(q,"access"),"name")==name),"no access qualifier "+name+" found for "+node.prettyPrint())
  }

  def getBranches(node:Node, name:String) : List[Node] = {
    if (!node.getBranches.contains(name)){
      return List[Node]()
    }
    return node.getBranches.get(name).getOrElse(List[Node]())
  }

  def getValues(node:Node, name:String) : List[String] = {
    if (!node.getValues.contains(name)){
      return List[String]()
    }
    return node.getValues.get(name).getOrElse(List[String]())
  }

  def hasBranch(node:Node, name:String) : Boolean = {
    if (None==node.getBranches.get(name)){
      return false
    }
    val l = getBranches(node,name)
    return l.size > 0
  }

  def hasValue(node:Node, name:String) : Boolean = {
    if (None==node.getValues.get(name)){
      return false
    }
    val l = getValues(node,name)
    return l.size > 0
  }

  def getBranch(node:Node, name:String) : Node = {
    val l = getBranches(node,name)
    assert(1==l.size,"Node "+node.prettyPrint()+" expected to have one branch named "+name+". Kind: "+node.getKind)
    return l.head
  }

  def getValue(node:Node, name:String) : String = {
    val l = getValues(node,name)
    assert(1==l.size,"Node "+node.prettyPrint()+" expected to have one value named "+name+". Kind: "+node.getKind)
    return l.head
  }

  def assertAbstractQualifier(node: Node) {
    assert(hasBranch(node,"qualifiers"),"node "+node.prettyPrint()+" has not qualifiers branches")
    assert(None!=getBranches(node,"qualifiers").find(q => hasValue(q,"abstract") && "abstract"==getValue(q,"abstract")),"Abstract qualifier not found for "+node.prettyPrint())
  }

  def assertStaticQualifier(node: Node) {
    assert(hasBranch(node,"qualifiers"),"node "+node.prettyPrint()+" has not qualifiers branches")
    assert(None!=getBranches(node,"qualifiers").find(q => hasValue(q,"static") && "static"==getValue(q,"static")),"Static qualifier not found for "+node.prettyPrint())
  }
}
