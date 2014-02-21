package codemodels.incrementalparsers.javaip

import name.lakhin.eliah.projects.papacarlo.lexis.{Matcher, Tokenizer,
  Contextualizer, Token}
import name.lakhin.eliah.projects.papacarlo.{Syntax, Lexer}
import name.lakhin.eliah.projects.papacarlo.syntax.Rule
import name.lakhin.eliah.projects.papacarlo.syntax.Node
import name.lakhin.eliah.projects.papacarlo.syntax.rules.NamedRule
import scala.collection.mutable

import org.scalatest._

abstract class UnitSpec extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors

class TokenizerSpec extends UnitSpec {

  private def check_token(kind:String,value:String,token:Token) : Unit = {
    assert(kind==token.kind,"kind is wrong")
    assert(value==token.value,"value is wrong")
  }

  it should "parse a basic class" in {
    val code = "class A { }"
    val lexer = JavaIP.tokenizer
    val tokens = lexer.tokenize(code)    
    assert(7==tokens.length)
    check_token("class","class",tokens(0))
    check_token("whitespace"," ",tokens(1))
    check_token("identifier","A",tokens(2))
    check_token("whitespace"," ",tokens(3))
    check_token("{","{",tokens(4))
    check_token("whitespace"," ",tokens(5))
    check_token("}","}",tokens(6))
  }

  it should "parse a field definition" in {
    val code = "class A {int[] bits;}"
    val lexer = JavaIP.tokenizer
    val tokens = lexer.tokenize(code)  
    assert(12==tokens.length)
    check_token("class","class",tokens(0))
    check_token("whitespace"," ",tokens(1))
    check_token("identifier","A",tokens(2))
    check_token("whitespace"," ",tokens(3))
    check_token("{","{",tokens(4))
    check_token("int","int",tokens(5))
    check_token("[","[",tokens(6))
    check_token("]","]",tokens(7))
    check_token("whitespace"," ",tokens(8))
    check_token("identifier","bits",tokens(9))
    check_token(";",";",tokens(10))
    check_token("}","}",tokens(11))
  }  

  it should "parse a method definition" in {
    val code = "class A {void foo(int a){return 1+2;}}"
    val lexer = JavaIP.tokenizer
    val tokens = lexer.tokenize(code)  
    assert(22==tokens.length)
    check_token("class","class",tokens(0))
    check_token("whitespace"," ",tokens(1))
    check_token("identifier","A",tokens(2))
    check_token("whitespace"," ",tokens(3))
    check_token("{","{",tokens(4))
    check_token("void","void",tokens(5))
    check_token("whitespace"," ",tokens(6))
    check_token("identifier","foo",tokens(7))
    check_token("(","(",tokens(8))
    check_token("int","int",tokens(9))
    check_token("whitespace"," ",tokens(10))
    check_token("identifier","a",tokens(11))
    check_token(")",")",tokens(12))
    check_token("{","{",tokens(13))
    check_token("return","return",tokens(14))
    check_token("whitespace"," ",tokens(15))
    check_token("integer","1",tokens(16))
    check_token("+","+",tokens(17))
    check_token("integer","2",tokens(18))
    check_token(";",";",tokens(19))
    check_token("}","}",tokens(20))
    check_token("}","}",tokens(21))
  }  

}

class ParserSpec extends UnitSpec {

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
      val classNode = node.getBranch("classDeclaration").get
      classes ::= classNode
    })
    return classes
  }

  def parseAndGetClassesMap(code : String) : Map[String,Node] = {
    var classes = Map[String,Node]()
    parse(code,node => {
      val classNode = node.getBranch("classDeclaration").get
      classes += (classNode.getValue("name") -> classNode)
    })
    return classes
  }

  def parseStmt(stmtCode : String) : Node = {
    val code = "class A { void foo(){"+stmtCode+"} }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
      syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)
    val m = members.head.getBranch("method").get
    assert(1==m.getBranches("stmts").size)
    val s = m.getBranches("stmts").head
    return s
  }

  def parseExpr(exprCode : String) : Node = {
    val s = parseStmt("a = "+exprCode+";")
    return s.getBranch("value").get
  }

  // Helper methods : assert

  def assertIsPrimitive(name:String,node: Node,arrayLevel: Int = 0) {
    assert(arrayLevel==node.getBranches("array").size)
    assert("primitiveType"==node.getBranch("baseType").get.getKind)
    assert(name==node.getBranch("baseType").get.getValue("name"))
  }

  def assertIsClass(name:String,node: Node,arrayLevel: Int = 0) {
    assert(arrayLevel==node.getBranches("array").size)
    assert("classType"==node.getBranch("baseType").get.getKind)
    assert(name==node.getBranch("baseType").get.getValue("name"))
  }

  def assertQualId(parts:List[String],node: Node) {
    assert("qualifiedIdentifier"==node.getKind)
    assert(parts==node.getValues("part").reverse)
  }

  def assertNodeIs(kind:String,values:Map[String,String],node:Node){
    assert(kind==node.getKind)
    values.foreach { case (key,value)=> assert(value==node.getValue(key),"Value of "+key+" expected to be "+value+". Node: "+node.prettyPrint()) }
  }

  def assertAccessQualifier(name:String,node: Node){
    assert(name==node.getBranch("qualifiers").get.getBranch("access").get.getValue("name"))
  }

  def assertAbstractQualifier(node: Node){
    assert("abstract"==node.getBranch("qualifiers").get.getValue("abstract"))
  }

  // Tests

  it should "parse a basic class" in {
    var classes = parseAndGetClassesList("class A { }")

    assert(1==classes.size)
    assert("A"==classes.head.getValue("name"))
  }

  it should "parse a basic class with qualifiers" in {
    var classes = parseAndGetClassesMap("public static class A { }")

    assert(1==classes.size)
    assert(classes contains "A")
    assert(2==classes.get("A").get.getBranches("qualifiers").size)
    assert(1==classes.get("A").get.getBranches("qualifiers").filter(n => n.hasValue("static")).size)
    assertAccessQualifier("public",classes.get("A").get)
  }

  it should "parse a basic class with comments" in {
    var classes = parseAndGetClassesMap("public /*ciao*/ static // hey! \n class /*come va?*/ A // last comment\n{ }")

    assert(1==classes.size)
    assert(classes contains "A")
    assert(2==classes.get("A").get.getBranches("qualifiers").size)
    assert(1==classes.get("A").get.getBranches("qualifiers").filter(n => n.hasValue("static")).size)
    assertAccessQualifier("public",classes.get("A").get)
  }

  it should "parse import directives" in {
    val code = "import java.applet.*;\n" +
               "import java.awt.*;\n"+
               "class A { }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var classes = Map[String,Node]()
    var imports = List[Node]()
    syntax.onNodeMerge.bind {node => {
      imports = node.getBranches("imports")
      val classNode = node.getBranch("classDeclaration").get
      classes += (classNode.getValue("name") -> classNode)
    }}
    lexer.input(code)

    assert(2==imports.size)
    var import1 = imports.head
    var import2 = imports.tail.head

    assert(List("java","applet","*")==import1.getValues("part").reverse)
    assert(List("java","awt","*")==import2.getValues("part").reverse)

    assert(1==classes.size)
    assert(classes contains "A")
    assert(0==classes.get("A").get.getBranches("qualifiers").size)
    assert(0==classes.get("A").get.getBranches("qualifiers").filter(n => n.hasValue("static")).size)
    assert(0==classes.get("A").get.getBranches("qualifiers").filter(n => n.hasBranch("access") && n.getBranch("access").get.hasValue("public")).size)
    assert(0==classes.get("A").get.getBranches("qualifiers").filter(n => n.hasBranch("access") && n.getBranch("access").get.hasValue("protected")).size)
    assert(0==classes.get("A").get.getBranches("qualifiers").filter(n => n.hasBranch("access") && n.getBranch("access").get.hasValue("private")).size)
  }

  it should "parse a method declaration with void return type" in {
    val code = "class A { void foo(){} }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var methods = List[Node]()
    syntax.onNodeMerge.bind {node => {
      val members = node.getBranch("classDeclaration").get.getBranches("members")
      methods = members
    }}
    lexer.input(code)

    assert(1==methods.size)
    val m = methods.head
    assert("foo"==m.getBranch("method").get.getValue("name"))
    assert("voidType"==m.getBranch("method").get.getBranch("returnType").get.getKind)
  }

  it should "parse a method declaration with primitive return type" in {
    val code = "class A { int foo(){} }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var methods = List[Node]()
    syntax.onNodeMerge.bind {node => {
      val members = node.getBranch("classDeclaration").get.getBranches("members")
      methods = members
    }}
    lexer.input(code)

    assert(1==methods.size)
    val m = methods.head
    assert("foo"==m.getBranch("method").get.getValue("name"))
    assertIsPrimitive("int",m.getBranch("method").get.getBranch("returnType").get)
  }

  it should "parse a field declaration with primitive type" in {
    val code = "class A { int foo; }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)
    val m = members.head
    assert("foo"==m.getBranch("field").get.getValue("name"))
    assertIsPrimitive("int",m.getBranch("field").get.getBranch("type").get)
  }

  it should "parse a field declaration with qualifiers" in {
    val code = "class A { private int foo; }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)
    val m = members.head.getBranch("field").get
    assertAccessQualifier("private",m)
    assert("foo"==m.getValue("name"))
    assertIsPrimitive("int",m.getBranch("type").get)
  }

  it should "parse a field declaration with array type" in {
    val code = "class A { int[] foo; }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)
    val m = members.head
    assert("foo"==m.getBranch("field").get.getValue("name"))

    assertIsPrimitive("int",m.getBranch("field").get.getBranch("type").get,1)
  }

  it should "parse a field declaration with triple array type" in {
    val code = "class A { int[][][] foo; }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)
    val m = members.head
    assert("foo"==m.getBranch("field").get.getValue("name"))

    assertIsPrimitive("int",m.getBranch("field").get.getBranch("type").get,3)
  }

  it should "parse method parameters" in {
    val code = "class A { void baz(int a,String b){} }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)
    val m = members.head.getBranch("method").get
    val p1 = m.getBranches("params").head
    val p2 = m.getBranches("params").tail.head
    assert("a"==p1.getValue("name"))
    assertIsPrimitive("int",p1.getBranch("type").get)
    assert("b"==p2.getValue("name"))
    assertIsClass("String",p2.getBranch("type").get)
  }

  it should "parse class extends clause" in {
    val code = "class A extends B {}"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var classes = List[Node]()
    syntax.onNodeMerge.bind {node => {
      classes = node.getBranches("classDeclaration")
    }}
    lexer.input(code)

    assert(1==classes.size)
    val c = classes.head
    assertQualId(List("B"),c.getBranch("baseClass").get)
  }

  it should "parse qualified identifier" in {
    val code = "class A extends B.C.D {}"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var classes = List[Node]()
    syntax.onNodeMerge.bind {node => {
      classes = node.getBranches("classDeclaration")
    }}
    lexer.input(code)

    assert(1==classes.size)
    val c = classes.head
    assertQualId(List("B","C","D"),c.getBranch("baseClass").get)
  }

  it should "parse class implements clause" in {
    val code = "class A implements C, D {}"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var classes = List[Node]()
    syntax.onNodeMerge.bind {node => {
      classes = node.getBranches("classDeclaration")
    }}
    lexer.input(code)

    assert(1==classes.size)
    val c = classes.head
    assert(2==c.getBranches("interfaces").size)
    val i1 = c.getBranches("interfaces").head
    val i2 = c.getBranches("interfaces").tail.head
    assertQualId(List("C"),i1)
    assertQualId(List("D"),i2)
  }

  it should "parse class extends and implements clause" in {
    val code = "class A extends B implements C, D {}"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var classes = List[Node]()
    syntax.onNodeMerge.bind {node => {
      classes = node.getBranches("classDeclaration")
    }}
    lexer.input(code)

    assert(1==classes.size)
    val c = classes.head
    assert(2==c.getBranches("interfaces").size)
    val bc = c.getBranch("baseClass").get
    val i1 = c.getBranches("interfaces").head
    val i2 = c.getBranches("interfaces").tail.head
    assertQualId(List("B"),bc)
    assertQualId(List("C"),i1)
    assertQualId(List("D"),i2)
  }

  it should "parse a field declaration with initializer" in {
    val code = "class A { int foo = 1; }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)

    val m = members.head.getBranch("field").get
    assert("foo"==m.getValue("name"))
    assertIsPrimitive("int",m.getBranch("type").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),m.getBranch("initializationValue").get)
  }

  it should "parse sum expression" in {
    val code = "class A { int foo = 1+2; }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)

    val m = members.head.getBranch("field").get
    assert("foo"==m.getValue("name"))
    assertIsPrimitive("int",m.getBranch("type").get)
    assertNodeIs("+",Map[String,String](),m.getBranch("initializationValue").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),m.getBranch("initializationValue").get.getBranch("left").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),m.getBranch("initializationValue").get.getBranch("right").get)
  }

  it should "parse subtraction expression" in {
    val code = "class A { int foo = 1-2; }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)

    val m = members.head.getBranch("field").get
    assert("foo"==m.getValue("name"))
    assertIsPrimitive("int",m.getBranch("type").get)
    assertNodeIs("-",Map[String,String](),m.getBranch("initializationValue").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),m.getBranch("initializationValue").get.getBranch("left").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),m.getBranch("initializationValue").get.getBranch("right").get)
  }

  it should "parse multiplication expression" in {
    val code = "class A { int foo = 1*2; }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)

    val m = members.head.getBranch("field").get
    assert("foo"==m.getValue("name"))
    assertIsPrimitive("int",m.getBranch("type").get)
    assertNodeIs("*",Map[String,String](),m.getBranch("initializationValue").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),m.getBranch("initializationValue").get.getBranch("left").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),m.getBranch("initializationValue").get.getBranch("right").get)
  }

  it should "parse division expression" in {
    val code = "class A { int foo = 1/2; }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)

    val m = members.head.getBranch("field").get
    assert("foo"==m.getValue("name"))
    assertIsPrimitive("int",m.getBranch("type").get)
    assertNodeIs("/",Map[String,String](),m.getBranch("initializationValue").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),m.getBranch("initializationValue").get.getBranch("left").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),m.getBranch("initializationValue").get.getBranch("right").get)
  }

  it should "parse variable reference expression" in {
    val code = "class A { int foo = a; }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)

    val m = members.head.getBranch("field").get
    assert("foo"==m.getValue("name"))
    assertIsPrimitive("int",m.getBranch("type").get)
    assertNodeIs("variableReference",Map[String,String]("name"->"a"),m.getBranch("initializationValue").get)
  }

  it should "parse field access expression" in {
    val code = "class A { int foo = 1 . a; }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)

    val m = members.head.getBranch("field").get
    assert("foo"==m.getValue("name"))
    assertIsPrimitive("int",m.getBranch("type").get)
    assertNodeIs("fieldAccess",Map[String,String]("fieldName"->"a"),m.getBranch("initializationValue").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),m.getBranch("initializationValue").get.getBranch("container").get)
  }

  it should "parse abstract class" in {
    val code = "abstract class A { }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var classes = List[Node]()
    syntax.onNodeMerge.bind {node => {
      classes ::= node.getBranch("classDeclaration").get
    }}
    lexer.input(code)

    assert(1==classes.size)
    val c = classes.head
    assertAbstractQualifier(c)
  }

  it should "parse abstract method" in {
    val code = "abstract class A { abstract void foo(); }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)
    val m = members.head.getBranch("method").get
  }

  it should "parse this ref" in {
    val code = "class A { void foo(){ this; } }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)
    val m = members.head.getBranch("method").get
    assert(1==m.getBranches("stmts").size)
    val s = m.getBranches("stmts").head
    assertNodeIs("expressionStatement",Map[String,String](),s);
    assertNodeIs("thisReference",Map[String,String](),s.getBranch("expression").get);
  }

  it should "parse function call without args" in {
    val code = "class A { int foo = baz(); }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)

    val m = members.head.getBranch("field").get
    val v = m.getBranch("initializationValue").get
    assertNodeIs("functionCall",Map[String,String]("name"->"baz"),v)
    assert(0==v.getBranches("params").size)
  }

  it should "parse function call with args" in {
    val code = "class A { int foo = baz(1,2); }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)

    val m = members.head.getBranch("field").get
    val v = m.getBranch("initializationValue").get
    assertNodeIs("functionCall",Map[String,String]("name"->"baz"),v)
    assert(2==v.getBranches("actualParams").size)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),v.getBranches("actualParams").head)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),v.getBranches("actualParams").tail.head)
  }

  it should "parse string literal" in {
    val code = "class A { int foo = \"ciao\"; }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)

    val m = members.head.getBranch("field").get
    val v = m.getBranch("initializationValue").get
    assertNodeIs("stringLiteral",Map[String,String]("value"->"\"ciao\""),v)
  }

  it should "parse char literal" in {
    val code = "class A { int foo = 'a'; }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)

    val m = members.head.getBranch("field").get
    val v = m.getBranch("initializationValue").get
    assertNodeIs("charLiteral",Map[String,String]("value"->"'a'"),v)
  }

  it should "parse instantiation of qualified class name with args" in {
    val code = "class A { int foo = new fooz.Baz(1,2); }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var members = List[Node]()
    syntax.onNodeMerge.bind {node => {
      members = node.getBranch("classDeclaration").get.getBranches("members")
    }}
    lexer.input(code)

    assert(1==members.size)

    val m = members.head.getBranch("field").get
    val v = m.getBranch("initializationValue").get
    assertNodeIs("instantiation",Map[String,String](),v)
    assertQualId(List[String]("fooz","Baz"),v.getBranch("className").get)
    assert(2==v.getBranches("actualParams").size)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),v.getBranches("actualParams").head)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),v.getBranches("actualParams").tail.head)
  }

  it should "parse assignment" in {
    val s = parseStmt("a.b = 1;")
    assertNodeIs("assignment",Map[String,String](),s);
    assertQualId(List[String]("a","b"),s.getBranch("assigned").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),s.getBranch("value").get)
  }

  it should "parse return statement" in {
    val s = parseStmt("return 1;")
    assertNodeIs("returnStmt",Map[String,String](),s);
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),s.getBranch("value").get)
  }

  it should "parse variable declaration statement without initialization" in {
    val s = parseStmt("int a;")
    assertNodeIs("localVarDecl",Map[String,String]("name"->"a"),s);
    assert(false==s.hasBranch("initializationValue"))
  }

  it should "parse variable declaration statement with initialization" in {
    val s = parseStmt("int a = 1;")
    assertNodeIs("localVarDecl",Map[String,String]("name"->"a"),s);
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),s.getBranch("initializationValue").get)
  }

  it should "parse comparison" in {
    var e = parseExpr("1==2")
    assertNodeIs("==",Map[String,String](),e)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),e.getBranch("left").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),e.getBranch("right").get)

    e = parseExpr("1!=2")
    assertNodeIs("!=",Map[String,String](),e)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),e.getBranch("left").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),e.getBranch("right").get)

    e = parseExpr("1<2")
    assertNodeIs("<",Map[String,String](),e)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),e.getBranch("left").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),e.getBranch("right").get)

    e = parseExpr("1>2")
    assertNodeIs(">",Map[String,String](),e)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),e.getBranch("left").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),e.getBranch("right").get)

    e = parseExpr("1<=2")
    assertNodeIs("<=",Map[String,String](),e)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),e.getBranch("left").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),e.getBranch("right").get)

    e = parseExpr("1>=2")
    assertNodeIs(">=",Map[String,String](),e)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),e.getBranch("left").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),e.getBranch("right").get)
  }

  it should "parse empty block stmt" in {
    // with closing semicolon
    var s = parseStmt("{};")
    assertNodeIs("blockStmt",Map[String,String](),s)
    assert(0==s.getBranches("stmts").size)

    // without closing semicolon
    s = parseStmt("{}")
    assertNodeIs("blockStmt",Map[String,String](),s)
    assert(0==s.getBranches("stmts").size)
  }

  it should "parse block stmt with children" in {
    var s = parseStmt("{int a; int b;};")
    assertNodeIs("blockStmt",Map[String,String](),s)
    assert(2==s.getBranches("stmts").size)
    assertNodeIs("localVarDecl",Map[String,String]("name"->"a"),s.getBranches("stmts").head);
    assertNodeIs("localVarDecl",Map[String,String]("name"->"b"),s.getBranches("stmts").tail.head);
  }

  it should "parse if without else" in {
    var s = parseStmt("if (1) return 2;")
    assertNodeIs("ifStmt",Map[String,String](),s)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),s.getBranch("condition").get)
    assertNodeIs("returnStmt",Map[String,String](),s.getBranch("then").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),s.getBranch("then").get.getBranch("value").get)
    assert(false==s.hasBranch("else"))
  }

  it should "parse if with else" in {
    var s = parseStmt("if (1) return 2; else return 3;")
    assertNodeIs("ifStmt",Map[String,String](),s)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),s.getBranch("condition").get)
    assertNodeIs("returnStmt",Map[String,String](),s.getBranch("then").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),s.getBranch("then").get.getBranch("value").get)
    assertNodeIs("returnStmt",Map[String,String](),s.getBranch("else").get)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"3"),s.getBranch("else").get.getBranch("value").get)

  }

}