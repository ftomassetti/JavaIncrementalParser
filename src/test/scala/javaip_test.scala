package codemodels.incrementalparsers.javaip

import name.lakhin.eliah.projects.papacarlo.lexis.{Matcher, Tokenizer,
Contextualizer, Token}
import name.lakhin.eliah.projects.papacarlo.{Syntax, Lexer}
import name.lakhin.eliah.projects.papacarlo.syntax.Rule
import name.lakhin.eliah.projects.papacarlo.syntax.Node
import name.lakhin.eliah.projects.papacarlo.syntax.rules.NamedRule
import scala.collection.mutable

import org.scalatest._

class ParserSpec extends PapaCarloUnitSpec {

  // Tests

  it should "parse a basic class" in {
    var classes = parseAndGetClassesList("class A { }")

    assert(1==classes.size)
    assert("A"==getValue(classes.head,"name"))
  }

  it should "parse a basic class with qualifiers" in {
    var classes = parseAndGetClassesMap("public static class A { }")

    assert(1==classes.size)
    assert(classes contains "A")
    val c = classes.get("A").get
    assert(2==c.getBranches("qualifiers").size)
    assertStaticQualifier(c)
    assertAccessQualifier("public",c)
  }

  it should "parse a basic class with comments" in {
    var classes = parseAndGetClassesMap("public /*ciao*/ static // hey! \n class /*come va?*/ A // last comment\n{ }")

    assert(1==classes.size)
    assert(classes contains "A")
    val c = classes.get("A").get
    assert(2==c.getBranches("qualifiers").size)
    assertStaticQualifier(c)
    assertAccessQualifier("public",c)
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
      imports = getBranches(node,"imports")
      val classNode = getBranch(node,"classDeclaration")
      classes += (getValue(classNode,"name") -> classNode)
    }}
    lexer.input(code)

    assert(2==imports.size)
    var import1 = imports.head
    var import2 = imports.tail.head

    assert(List("java","applet","*")==import1.getValues("part").reverse)
    assert(List("java","awt","*")==import2.getValues("part").reverse)

    assert(1==classes.size)
    assert(classes contains "A")
    val c = classes.get("A").get
    assert(0==getBranches(c,"qualifiers").size)
  }

  it should "parse a method declaration with void return type" in {
    val m = parseAndGetMethod("void foo(){}")
    assert("foo"==getValue(m,"name"))
    assert("voidType"==getBranch(m,"returnType").getKind)
    assert(false==hasValue(m,"abstractBody"))
  }

  it should "parse an abstract method declaration" in {
    val m = parseAndGetMethod("abstract void foo();")
    assert("foo"==getValue(m,"name"))
    assert("voidType"==getBranch(m,"returnType").getKind)
    assertAbstractQualifier(m);
    assert(true==hasValue(m,"abstractBody"))
  }

  it should "parse a method declaration with annotation" in {
    val m = parseAndGetMethod("@myAnnotation void foo(){}")
    assert("foo"==getValue(m,"name"))
    assert("voidType"==getBranch(m,"returnType").getKind)
    assert(1==m.getBranches.get("annotations").get.size)
    assertNodeIs("annotationUsage",Map[String,String](),getBranch(m,"annotations"))
  }

  it should "parse a method declaration with primitive return type" in {
    val m = parseAndGetMethod("int fooz(){}")
    assert("fooz"==getValue(m,"name"))
    assertIsPrimitive("int",getBranch(m,"returnType"))
  }

  it should "parse a field declaration with primitive type" in {
    val m = parseAndGetField("int foo;")
    assert("foo"==getValue(m,"name"))
    assertIsPrimitive("int",getBranch(m,"type"))
  }

  it should "parse a field declaration with qualifiers" in {
    val m = parseAndGetField("private int foo;")
    assertAccessQualifier("private",m)
    assert("foo"==getValue(m,"name"))
    assertIsPrimitive("int",getBranch(m,"type"))
  }

  it should "parse generic type usage" in {
    val f = parseAndGetField("List<String> adaptedTokens;")
    assert("adaptedTokens"==getValue(f,"name"))
  }

  it should "parse a field declaration with array type" in {
    val m = parseAndGetField("int[] foo;")
    assert("foo"==getValue(m,"name"))
    assertIsPrimitive("int",getBranch(m,"type"),1)
  }

  it should "parse a field declaration with triple array type" in {
    val m = parseAndGetField("int[][][] foo;")
    assert("foo"==getValue(m,"name"))
    assertIsPrimitive("int",getBranch(m,"type"),3)
  }

  it should "parse method parameters" in {
    val m = parseAndGetMethod("void baz(int a,String b){}")
    val p1 = m.getBranches("params").head
    val p2 = m.getBranches("params").tail.head
    assert("a"==getValue(p1,"name"))
    assertIsPrimitive("int",getBranch(p1,"type"))
    assert("b"==getValue(p2,"name"))
    assertIsClass("String",getBranch(p2,"type"))
  }

  it should "parse class extends clause" in {
    var c = parseAndGetClass("class A extends B {}")
    assertQualId(List("B"),getBranch(c,"baseClass"))
  }

  it should "parse qualified identifier" in {
    var c = parseAndGetClass("class A extends B.C.D {}")
    assertQualId(List("B","C","D"),getBranch(c,"baseClass"))
  }

  it should "parse class implements clause" in {
    var c = parseAndGetClass("class A implements C, D {}")
    assert(2==getBranches(c,"interfaces").size)
    val i1 = getBranches(c,"interfaces").head
    val i2 = getBranches(c,"interfaces").tail.head
    assertQualId(List("C"),i1)
    assertQualId(List("D"),i2)
  }

  it should "parse class extends and implements clause" in {
    var c = parseAndGetClass("class A extends B implements C, D {}")
    assert(2==getBranches(c,"interfaces").size)
    val bc = getBranch(c,"baseClass")
    val i1 = getBranches(c,"interfaces").head
    val i2 = getBranches(c,"interfaces").tail.head
    assertQualId(List("B"),bc)
    assertQualId(List("C"),i1)
    assertQualId(List("D"),i2)
  }

  it should "parse a field declaration with initializer" in {
    val m = parseAndGetField("int foo = 1;")
    assert("foo"==getValue(m,"name"))
    assertIsPrimitive("int",getBranch(m,"type"))
    assertIsIntegerLiteral(1,getBranch(m,"initializationValue"))
  }

  it should "parse sum expression" in {
    val m = parseAndGetField("int foo = 1+2;")
    assert("foo"==getValue(m,"name"))
    assertIsPrimitive("int",getBranch(m,"type"))
    assertNodeIs("+",Map[String,String](),getBranch(m,"initializationValue"))
    assertIsIntegerLiteral(1,getBranch(getBranch(m,"initializationValue"),"left"))
    assertIsIntegerLiteral(2,getBranch(getBranch(m,"initializationValue"),"right"))
  }

  it should "parse subtraction expression" in {
    val m = parseAndGetField("int foo = 1-2;")
    assert("foo"==getValue(m,"name"))
    assertIsPrimitive("int",getBranch(m,"type"))
    assertNodeIs("-",Map[String,String](),getBranch(m,"initializationValue"))
    assertIsIntegerLiteral(1,getBranch(getBranch(m,"initializationValue"),"left"))
    assertIsIntegerLiteral(2,getBranch(getBranch(m,"initializationValue"),"right"))
  }

  it should "parse multiplication expression" in {
    val m = parseAndGetField("int foo = 1*2;")
    assert("foo"==getValue(m,"name"))
    assertIsPrimitive("int",getBranch(m,"type"))
    assertNodeIs("*",Map[String,String](),getBranch(m,"initializationValue"))
    assertIsIntegerLiteral(1,getBranch(getBranch(m,"initializationValue"),"left"))
    assertIsIntegerLiteral(2,getBranch(getBranch(m,"initializationValue"),"right"))
  }

  it should "parse division expression" in {
    val m = parseAndGetField("int foo = 1/2;")
    assert("foo"==getValue(m,"name"))
    assertIsPrimitive("int",getBranch(m,"type"))
    assertNodeIs("/",Map[String,String](),getBranch(m,"initializationValue"))
    assertIsIntegerLiteral(1,getBranch(getBranch(m,"initializationValue"),"left"))
    assertIsIntegerLiteral(2,getBranch(getBranch(m,"initializationValue"),"right"))
  }

  it should "parse variable reference expression" in {
    val m = parseAndGetField("int foo = a;")
    assert("foo"==getValue(m,"name"))
    assertIsPrimitive("int",getBranch(m,"type"))
    assertNodeIs("variableReference",Map[String,String]("name"->"a"),getBranch(m,"initializationValue"))
  }

  it should "parse field access expression" in {
    val e = parseExpr("a.b")
    assertNodeIs("chainExp",Map[String,String](),e)
    assertNodeIs("variableReference",Map[String,String]("name"->"b"),getBranch(e,"chained"))
    assertNodeIs("variableReference",Map[String,String]("name"->"a"),getBranch(e,"base"))
  }

  it should "parse abstract class" in {
    val c = parseAndGetClass("abstract class A { }")
    assertAbstractQualifier(c)
  }

  it should "parse abstract method" in {
    val m = parseAndGetMethod("abstract void foo();")
    assertAbstractQualifier(m)
  }

  it should "parse this ref" in {
    val m = parseAndGetMethod("void foo(){ this; }")
    assert(1==m.getBranches("stmts").size)
    val s = m.getBranches("stmts").head
    assertNodeIs("expressionStatement",Map[String,String](),s);
    assertNodeIs("thisReference",Map[String,String](),getBranch(s,"expression"))
  }

  it should "parse function call without args" in {
    val e = parseExpr("baz()")
    assertNodeIs("expMethodCall",Map[String,String](),e)
    assertNodeIs("variableReference",Map[String,String]("name"->"baz"),getBranch(e,"base"))
  }

  it should "parse function call with args" in {
    val e = parseExpr("baz(1,2)")
    assertNodeIs("expMethodCall",Map[String,String](),e)
    assertNodeIs("variableReference",Map[String,String]("name"->"baz"),getBranch(e,"base"))
    assert(2==getBranches(getBranch(e,"invocation"),"actualParams").size)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),getBranches(getBranch(e,"invocation"),"actualParams").head)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),getBranches(getBranch(e,"invocation"),"actualParams").tail.head)
  }

  it should "parse string literal" in {
    val m = parseAndGetField("int foo = \"ciao\";")
    val v = getBranch(m,"initializationValue")
    assertNodeIs("stringLiteral",Map[String,String]("value"->"\"ciao\""),v)
  }

  it should "parse char literal" in {
    val m = parseAndGetField("int foo = 'a';")
    val v = getBranch(m,"initializationValue")
    assertNodeIs("charLiteral",Map[String,String]("value"->"'a'"),v)
  }

  it should "parse instantiation of qualified class name with args" in {
    val m = parseAndGetField("int foo = new fooz.Baz(1,2);")
    val v = getBranch(m,"initializationValue")
    assertNodeIs("instantiation",Map[String,String](),v)
    val ci = getBranch(v,"classInst")
    assertNodeIs("classInstantiation",Map[String,String](),ci)
    assertQualId(List[String]("fooz","Baz"),getBranch(ci,"className"))
    assert(2==ci.getBranches("actualParams").size)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),ci.getBranches("actualParams").head)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),ci.getBranches("actualParams").tail.head)
  }

  it should "parse assignment" in {
    val s = parseStmt("a.b = 1;")
    assertNodeIs("assignment",Map[String,String](),s);
    assertQualId(List[String]("a","b"),getBranch(s,"assigned"))
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),getBranch(s,"value"))
  }

  it should "parse return statement" in {
    val s = parseStmt("return 1;")
    assertNodeIs("returnStmt",Map[String,String](),s);
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),getBranch(s,"value"))
  }

  it should "parse variable declaration statement without initialization" in {
    val s = parseStmt("int a;")
    assertNodeIs("localVarDecl",Map[String,String]("name"->"a"),s);
    assert(false==hasBranch(s,"initializationValue"))
  }

  it should "parse variable declaration statement with initialization" in {
    val s = parseStmt("int a = 1;")
    assertNodeIs("localVarDecl",Map[String,String]("name"->"a"),s);
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),getBranch(s,"initializationValue"))
  }

  it should "parse throw statement" in {
    val s = parseStmt("throw 1")
    assertNodeIs("throwStmt",Map[String,String](),s);
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),getBranch(s,"value"))
  }

  it should "parse comparison" in {
    var e = parseExpr("1==2")
    assertNodeIs("==",Map[String,String](),e)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),getBranch(e,"left"))
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),getBranch(e,"right"))

    e = parseExpr("1!=2")
    assertNodeIs("!=",Map[String,String](),e)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),getBranch(e,"left"))
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),getBranch(e,"right"))

    e = parseExpr("1<2")
    assertNodeIs("<",Map[String,String](),e)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),getBranch(e,"left"))
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),getBranch(e,"right"))

    e = parseExpr("1>2")
    assertNodeIs(">",Map[String,String](),e)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),getBranch(e,"left"))
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),getBranch(e,"right"))

    e = parseExpr("1<=2")
    assertNodeIs("<=",Map[String,String](),e)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),getBranch(e,"left"))
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),getBranch(e,"right"))

    e = parseExpr("1>=2")
    assertNodeIs(">=",Map[String,String](),e)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),getBranch(e,"left"))
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),getBranch(e,"right"))
  }

  it should "parse empty block stmt" in {
    // with closing semicolon
    var s = parseStmt("{};")
    assertNodeIs("blockStmt",Map[String,String](),s)
    assert(0==getBranches(s,"stmts").size)

    // without closing semicolonbranch("then",statement),
    s = parseStmt("{}")
    assertNodeIs("blockStmt",Map[String,String](),s)
    assert(0==getBranches(s,"stmts").size)
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
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),getBranches(s,"condition").head)
    assertNodeIs("returnStmt",Map[String,String](),getBranches(s,"then").head)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),getBranch(getBranches(s,"then").head,"value"))
    assert(false==hasBranch(s,"else"))
  }

  it should "parse if with else" in {
    var s = parseStmt("if (1) return 2; else return 3;")
    assertNodeIs("ifStmt",Map[String,String](),s)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),getBranches(s,"condition").head)
    assertNodeIs("returnStmt",Map[String,String](),getBranches(s,"then").head)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),getBranch(getBranches(s,"then").head,"value"))
    assertNodeIs("returnStmt",Map[String,String](),getBranches(s,"else").head)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"3"),getBranch(getBranches(s,"else").head,"value"))
  }

  it should "parse assignement of this" in {
    var e = parseExpr("this")
    assertNodeIs("thisReference",Map[String,String](),e)
  }

  it should "parse method call on this without params" in {
    var e = parseExpr("this.setBackground()")
    assertNodeIs("chainExp",Map[String,String](),e)
    assertNodeIs("thisReference",Map[String,String](),getBranch(e,"base"))
    assertNodeIs("expMethodCall",Map[String,String](),getBranch(e,"chained"))
    assertNodeIs("variableReference",Map[String,String]("name"->"setBackground"),getBranch(getBranch(e,"chained"),"base"))
  }

  it should "parse method call on this with a param" in {
    var e = parseExpr("this.setBackground(Color.white)")
    assertNodeIs("chainExp",Map[String,String](),e)
    assertNodeIs("thisReference",Map[String,String](),getBranch(e,"base"))
    assertNodeIs("expMethodCall",Map[String,String](),getBranch(e,"chained"))
    assertNodeIs("variableReference",Map[String,String]("name"->"setBackground"),getBranch(getBranch(e,"chained"),"base"))
    var inv = getBranch(getBranch(e,"chained"),"invocation")
    assert(1==inv.getBranches("actualParams").size)
  }

  it should "parse only line comments with open bracket" in {
    var code = "// abc {\n// def[ \n//ghj"
    var root = parseAndGetRoot(code)
    assertNodeIs("compilationUnit",Map[String,String](),root)
  }

  it should "double line comment" in {
    var code = "//this.getToolkit().sync();  // Force it to be drawn *now*."
    var root = parseAndGetRoot(code)
  }

  it should "parse package decl" in {
    var code = "package it.polito;"
    var root = parseAndGetRoot(code)
    var packDecl = getBranch(root,"packageDecl")
    var packName = getBranch(packDecl,"packageName")
    assertQualId(List[String]("it","polito"),packName)
  }

  it should "parse not exp" in {
    var notExp = parseExpr("!1")
    assertNodeIs("!",Map[String,String](),notExp)
    assertIsIntegerLiteral(1,getBranch(notExp,"operand"))
  }

  it should "parse usage of generic type" in {
    var stmt = parseStmt("List<String> l;");
    assertNodeIs("localVarDecl",Map[String,String]("name"->"l"),stmt)
    assertNodeIs("typeUsage",Map[String,String](),getBranch(stmt,"type"))
    assertNodeIs("classType",Map[String,String]("name"->"List"),getBranch(getBranch(stmt,"type"),"baseType"))
    val genericParams = getBranch(getBranch(getBranch(stmt,"type"),"baseType"),"genericParams")
    assertNodeIs("classType",Map[String,String]("name"->"String"),getBranch(getBranch(genericParams,"params"),"baseType"))
  }

  it should "field assignment" in {
    var stmt = parseStmt("this.a = 1;")
    assertNodeIs("assignment",Map[String,String](),stmt)
    assertNodeIs("fieldAccess",Map[String,String](),getBranch(stmt,"assigned"))
    assertQualId(List[String]("a"),getBranch(getBranch(stmt,"assigned"),"field"))
  }

  it should "foreach stmt" in {
    var stmt = parseStmt("for (int i : arr) {}")
    assertNodeIs("forEachStmt",Map[String,String](),stmt)
    assertNodeIs("simpleLocalVarDecl",Map[String,String]("name"->"i"),getBranch(stmt,"iterator"))
    assertNodeIs("variableReference",Map[String,String]("name"->"arr"),getBranch(stmt,"collection"))
  }

  it should "parsing diamon operator in instantiation" in {
    var exp = parseExpr("new LinkedList<>()")
    assertNodeIs("instantiation",Map[String,String](),exp)
    assertNodeIs("classInstantiation",Map[String,String](),getBranch(exp,"classInst"))
    assertNodeIs("genericParams",Map[String,String](),getBranch(getBranch(exp,"classInst"),"genericParams"))
  }

  it should "parse chain of calls" in {
    var exp = parseExpr("a().b().c()")
    assertNodeIs("chainExp", Map[String,String](), exp)
    assertIsMethodCall("a",getBranch(exp,"base"))
    val ch1 = getBranches(exp,"chained").head
    assertIsMethodCall("b",ch1)
    val ch2 = getBranches(exp,"chained").tail.head
    assertIsMethodCall("c",ch2)
  }

  it should "parse method decl with throws clause" in {
    val m = parseAndGetMethod("void foo() throws MyException {}")
    assert("foo"==getValue(m,"name"))
    assert("voidType"==getBranch(m,"returnType").getKind)
    assert(1==m.getBranches("exceptionsThrown").size)
    assertQualId(List[String]("MyException"),getBranch(m,"exceptionsThrown"))
  }

}