package codemodels.incrementalparsers.javaip

import name.lakhin.eliah.projects.papacarlo.lexis.{Matcher, Tokenizer,
Contextualizer, Token}
import name.lakhin.eliah.projects.papacarlo.{Syntax, Lexer}
import name.lakhin.eliah.projects.papacarlo.syntax.Node

class ParserSpec extends PapaCarloUnitSpec {

  // Tests

  it should "parse a basic class" in {
    var c = parseAndGetClass("class A { }")
    assertNode(c,new ENode("classDeclaration").value("name","A"))
  }

  it should "parse a basic class with qualifiers" in {
    var c = parseAndGetClass("public static class A { }")

    assertNode(c,new ENode("classDeclaration").value("name","A"))
    println(c.prettyPrint())
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
    assertNodeIs("typeUsage",Map[String,String](),getBranch(c,"baseClass"))
    assertNodeIs("classType",Map[String,String]("name"->"B"),getBranch(getBranch(c,"baseClass"),"baseType"))
  }

  it should "parse qualified identifier" in {
    var c = parseAndGetClass("class A extends B.C.D {}")
    assertNodeIs("typeUsage",Map[String,String](),getBranch(c,"baseClass"))
    assertNodeIs("classType",Map[String,String]("name"->"B"),getBranch(getBranch(c,"baseClass"),"baseType"))
    c = getBranch(getBranch(getBranch(c,"baseClass"),"baseType"),"member")
    assertNodeIs("classType",Map[String,String]("name"->"C"),c)
    c = getBranch(c,"member")
    assertNodeIs("classType",Map[String,String]("name"->"D"),c)
  }

  it should "parse class implements clause" in {
    var c = parseAndGetClass("class A implements C, D {}")
    assert(2==getBranches(c,"interfaces").size)
    val i1 = getBranches(c,"interfaces").head
    val i2 = getBranches(c,"interfaces").tail.head
    assertNodeIs("typeUsage",Map[String,String](),i1)
    assertNodeIs("classType",Map[String,String]("name"->"C"),getBranch(i1,"baseType"))
    assertNodeIs("typeUsage",Map[String,String](),i1)
    assertNodeIs("classType",Map[String,String]("name"->"D"),getBranch(i2,"baseType"))
  }

  it should "parse class extends and implements clause" in {
    var c = parseAndGetClass("class A extends B implements C, D {}")
    assert(2==getBranches(c,"interfaces").size)
    val bc = getBranch(c,"baseClass")
    assertNodeIs("typeUsage",Map[String,String](),bc)
    assertNodeIs("classType",Map[String,String]("name"->"B"),getBranch(bc,"baseType"))
    val i1 = getBranches(c,"interfaces").head
    val i2 = getBranches(c,"interfaces").tail.head
    assertNodeIs("typeUsage",Map[String,String](),i1)
    assertNodeIs("classType",Map[String,String]("name"->"C"),getBranch(i1,"baseType"))
    assertNodeIs("typeUsage",Map[String,String](),i2)
    assertNodeIs("classType",Map[String,String]("name"->"D"),getBranch(i2,"baseType"))
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
    assertNodeIs("classInstantiation",Map[String,String](),v)
    assertQualId(List[String]("fooz","Baz"),getBranch(v,"className"))
    assert(2==v.getBranches("actualParams").size)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"1"),v.getBranches("actualParams").head)
    assertNodeIs("integerLiteral",Map[String,String]("value"->"2"),v.getBranches("actualParams").tail.head)
  }

  it should "parse assignment" in {
    var s = parseStmt("a.b = 1;")
    assertNodeIs("expressionStatement",Map[String,String](),s);
    s = getBranch(s,"expression")
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

  it should "parse field assignment" in {
    var stmt = parseStmt("this.a = 1;")
    assertNodeIs("expressionStatement",Map[String,String](),stmt);
    stmt = getBranch(stmt,"expression")
    assertNodeIs("assignment",Map[String,String](),stmt)
    assertNodeIs("fieldAccess",Map[String,String](),getBranch(stmt,"assigned"))
    assertQualId(List[String]("a"),getBranch(getBranch(stmt,"assigned"),"field"))
  }

  it should "parse empty for stmt" in {
    var s = parseStmt("for(;;) {}")
  }

  it should "parse for stmt with just variable assignment" in {
    var s = parseStmt("for(i = 0; ; ) {}")
  }

  it should "parse for stmt" in {
    var s = parseStmt("for(i = 0; i < rects.size(); i++) {}")
  }

  it should "parse foreach stmt" in {
    var stmt = parseStmt("for (int i : arr) {}")
    assertNodeIs("forEachStmt",Map[String,String](),stmt)
    assertNodeIs("simpleLocalVarDecl",Map[String,String]("name"->"i"),getBranch(stmt,"iterator"))
    assertNodeIs("variableReference",Map[String,String]("name"->"arr"),getBranch(stmt,"collection"))
  }

  it should "parse diamond operator in instantiation" in {
    var exp = parseExpr("new LinkedList<>()")
    assertNodeIs("classInstantiation",Map[String,String](),exp)
    assertNodeIs("genericParams",Map[String,String](),getBranch(exp,"genericParams"))
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

  it should "parse declaration of array of primitive type" in {
    val s = parseStmt("byte[] buffer;")
    assertNodeIs("localVarDecl",Map[String,String]("name"->"buffer"),s)
    assertNodeIs("typeUsage",Map[String,String](),getBranch(s,"type"))
    assertNodeIs("primitiveType",Map[String,String]("name"->"byte"),getBranch(getBranch(s,"type"),"baseType"))
    assertNodeIs("arrayType",Map[String,String](),getBranch(getBranch(s,"type"),"array"))
  }

  it should "parse instantiation of array of primitive type" in {
    val e = parseExpr("new byte[101]")
    assertNodeIs("arrayInstantiation",Map[String,String]("size"->"101"),e)
    assertNodeIs("primitiveType",Map[String,String]("name"->"byte"),getBranch(e,"typeName"))
  }

  it should "parse expression with assignment" in {
    val e = parseExpr("bytes_read = from.read(buffer)")
  }

  it should "parse expression with assignment in parenthesis" in {
    val e = parseExpr("(bytes_read = from.read(buffer))")
  }

  it should "parse expression with disequality check" in {
    val e = parseExpr("(bytes_read = from.read(buffer)) != -1")
  }

  it should "parse interface" in {
    val c = parseAndGetRoot("interface A {}")
  }

  it should "parse annidated classes" in {
    val c = parseAndGetRoot("class A { class B {} }")
  }

  it should "parse multiple catch clauses" in {
    val c = parseStmt("try {} catch (A a){} catch(B b){}")
  }

  it should "parsing empty return" in {
    val c = parseStmt("return;")
  }

  it should "access container this" in {
    val e = parseExpr("Applet.this")
  }

  it should "field access on access container this" in {
    val e = parseExpr("Applet.this.status")
  }

  it should "method call on access container this" in {
    val e = parseExpr("Applet.this.showStatus()")
  }

  it should "method call on access container this with params" in {
    val e = parseExpr("Applet.this.showStatus(\"a\")")
  }

  it should "parse declaration and initialization of multiple variables at once" in {
    val s = parseStmt("int i, x = e.getX(), y = e.getY();")
  }

  it should "parse postfix inc" in {
    val e = parseExpr("i++")
  }

  it should "parse annotation decl" in {
    val r = parseAndGetRoot("public @interface MyInterceptorBinding {}")
  }

  it should "parse annotation without params on annotation decl" in {
    val r = parseAndGetRoot("@A public @interface MyInterceptorBinding {}")
  }

  it should "parse annotation with single param, on annotation decl" in {
    val r = parseAndGetRoot("@A(a) public @interface MyInterceptorBinding {}")
  }

  it should "parse annotation with multiple params, on annotation decl" in {
    val r = parseAndGetRoot("@A({a,b}) public @interface MyInterceptorBinding {}")
  }

  it should "parse import static" in {
    val r = parseAndGetRoot("import static java.lang.annotation.ElementType.METHOD;")
  }

  it should "parse generic capture" in {
    val r = parseAndGetMethod("public static Archive<?> deploy(){}")
  }

  it should "parse generic capture with clause" in {
    val r = parseAndGetMethod("public static Archive<? extends A> deploy(){}")
  }

  it should "parse class access" in {
    val e = parseExpr("A.class")
  }

  it should "parse empty array of class init" in {
    val e = parseExpr("new Class[] { }")
  }

  it should "parse array of class init" in {
    val e = parseExpr("new Class[] { HttpServletRequest.class, HttpServletResponse.class }")
  }

  it should "parse array of callbacks init" in {
    val e = parseExpr("new Callback[] { new CallerPrincipalCallback(clientSubject, \"test\"), new GroupPrincipalCallback(clientSubject, new String[] { \"architect\" }) }")
  }

  it should "parse annotation on params" in {
    val m = parseAndGetMethod("public void initialize(@SuppressWarnings(\"rawtypes\") Map options);")
  }

  it should "parse catch of multiple exceptions" in {
    val s = parseStmt("try {} catch (IOException | UnsupportedCallbackException e) {}")
  }

  it should "parse long literal" in {
    val e = parseExpr("10L")
  }

  it should "parse null literal" in {
    val e = parseExpr("null")
  }

  it should "parse cast null" in {
    val e = parseExpr("(Certificate) null")
  }

  it should "parse cast to array" in {
    val e = parseExpr("(Certificate[]) null")
  }

  it should "parse instanceof exp" in {
    val e = parseExpr("permission instanceof WebRoleRefPermission")
  }

  it should "parse annotation with  on field" in {
    val m = parseAndGetMember("@Size private String name;")
  }

  it should "parse annotation with  multiple fields" in {
    val m = parseAndGetMember("@Size(1,20) private String name;")
  }

  it should "parse annotation with named fields" in {
    val m = parseAndGetMember("@Size(min = 1, max = 20) private String name;")
  }

  it should "parse try with resource stmt" in {
    val s = parseStmt("try (PrintWriter out = response.getWriter()) { }")
  }

  it should "parse synchronized stmt" in {
    val s = parseStmt("synchronized (builder) { builder.append(something); }")
  }

  it should "parse volatile field" in {
    val f = parseAndGetMember("volatile StringBuilder builder;")
  }

  it should "parse anonymous class" in {
    val e = parseExpr("new BaseMatcher<Long>() { void foo(){} }")
  }

  it should "parse final on formal param" in {
    val m = parseAndGetMethod("void withinWindow(final long timeout);")
  }

  it should "parse final local var decl" in {
    val s = parseStmt("final Long actual = (Long) item;")
  }

  it should "parse not escaped char literal" in {
    val e = parseExpr("'a'")
  }

  it should "parse escaped char literal" in {
    val e = parseExpr("'\\''")
  }

  it should "parse foreach over final iterator" in {
    val s = parseStmt("for (final String item : items) {}")
  }

  it should "parse float literal" in {
    val e = parseExpr("10f")
  }

  it should "parse annidated annotations" in {
    val r = parseAndGetClass("@NamedQueries({@NamedQuery(name = \"Employee.findAll\", query = \"SELECT e FROM Employee e\") }) public class Employee {}")
  }

  it should "parse generic params on static calls" in {
    val e = parseExpr("Collections.<String, String> emptyMap()")
  }

  it should "parse exceptions decl on constructors" in {
    val m = parseAndGetMember("Pippo() throws Hi {}")
  }

  it should "parse if-else operator" in {
    val e = parseExpr("a==b? 1 : 0")
  }

  it should "parse generic parameter for methods" in {
    val m = parseAndGetMethod("public <T> T foo(){}")
  }

  it should "parse inner class as base class" in {
    val c = parseAndGetClass("class A extends B.C {}")
  }

  it should "parse qualified annotations" in {
    val c = parseAndGetClass("@javax.ws.rs.ApplicationPath class A {}")
  }

  it should "parse instance initializer" in {
    val m = parseAndGetMember("class A { {foo();} }")
  }

  it should "parse static initializer" in {
    val m = parseAndGetMember("class A { static {foo();} }")
  }

  it should "parse break stmt" in {
    val s = parseStmt("break;")
  }

  it should "parse continue stmt" in {
    val s = parseStmt("continue;")
  }

  it should "parse annotation usage on package" in {
    val r = parseAndGetRoot("@Vetoed package a.b.c;")
  }

  it should "parse array class access" in {
    val e = parseExpr("Person[].class")
  }

  it should "parse annotation decl field" in {
    val r = parseAndGetRoot("public @interface NotNullAndNonEmptyNames { Class<?>[] groups() default {}; }")
  }

  it should "parse enum" in {
    val r = parseAndGetRoot("enum RelTypes implements RelationshipType { SPOUSE, BROTHER, SISTER }");
  }

  it should "parse static before annotation on fields" in {
    val f = parseAndGetField("static @Produces int i;");
  }

  it should "parse local var array decl with array modifier postfixed" in {
    val s = parseStmt("byte b[] = new byte[1024];")
  }

}