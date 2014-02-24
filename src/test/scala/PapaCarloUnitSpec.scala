package codemodels.incrementalparsers.javaip

import name.lakhin.eliah.projects.papacarlo.syntax.Node
import org.scalatest._

abstract class PapaCarloUnitSpec  extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors {
  // Helper methods : parsing

  def parse(code : String, f : Node => Any) {
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    syntax.onNodeMerge.bind {node => {
      f(node)
    }}
    lexer.input(code)
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

  def parseAndGetMember(code : String) : Node = {
    val code = "class A { void foo(){} }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = getBranch(node,"classDeclaration").getBranches.get("members").get
    }}
    lexer.input(code)
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
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranches.get("classDeclaration").get.head.getBranches.get("members").get
    }}
    lexer.input(code)

    assert(1==members.size)
    val m : Node = members.head.getBranches.get("method").get.head
    assert(1==m.getBranches.get("stmts").size)
    val s = m.getBranches.get("stmts").get.head
    return s
  }

  def parseExpr(exprCode : String) : Node = {
    val s = parseStmt("a = "+exprCode+";")
    return s.getBranches.get("value").get.head
  }

  // Helper methods : assert

  def assertIsPrimitive(name:String,node: Node,arrayLevel: Int = 0) {
    assert(arrayLevel==getBranches(node,"array").size)
    assert("primitiveType"==getBranch(node,"baseType").getKind,"It is not a primitive type: "+node.prettyPrint())
    assert(name==node.getBranches.get("baseType").get.head.getValues.get("name").get)
  }

  def assertIsClass(name:String,node: Node,arrayLevel: Int = 0) {
    assert(arrayLevel==node.getBranches("array").size)
    assert("classType"==node.getBranches.get("baseType").get.head.getKind)
    assert(name==node.getBranches.get("baseType").get.head.getValues.get("name").get)
  }

  def assertQualId(parts:List[String],node: Node) {
    assert("qualifiedIdentifier"==node.getKind)
    assert(parts==node.getValues("part").reverse)
  }

  def assertNodeIs(kind:String,values:Map[String,String],node:Node){
    assert(kind==node.getKind)
    values.foreach { case (key,value)=> assert(value==node.getValues.get(key).value,"Value of "+key+" expected to be "+value+". Node: "+node.prettyPrint()) }
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
    assert(1==l.size,"Node "+node.prettyPrint()+" expected to have one branch named "+name)
    return l.head
  }

  def getValue(node:Node, name:String) : String = {
    return node.getValues.get(name).get.head
  }

  def assertAbstractQualifier(node: Node) {
    assert(hasBranch(node,"qualifiers"),"node "+node.prettyPrint()+" has not qualifiers branches")
    assert(None!=getBranches(node,"qualifiers").find(q => hasBranch(q,"abstract") && "abstract"==getValue(getBranch(q,"abstract"),"abstract")),"Abstract qualifier not found for "+node.prettyPrint())
  }

  def assertStaticQualifier(node: Node) {
    assert(hasBranch(node,"qualifiers"),"node "+node.prettyPrint()+" has not qualifiers branches")
    assert(None!=getBranches(node,"qualifiers").find(q => hasValue(q,"static") && "static"==getValue(q,"static")),"Static qualifier not found for "+node.prettyPrint())
  }
}
