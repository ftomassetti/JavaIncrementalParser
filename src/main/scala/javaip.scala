package codemodels.incrementalparsers.javaip

import name.lakhin.eliah.projects.papacarlo.lexis.{Matcher, Tokenizer,
  Contextualizer, Token}
import name.lakhin.eliah.projects.papacarlo.{Syntax, Lexer}
import name.lakhin.eliah.projects.papacarlo.syntax.Rule
import name.lakhin.eliah.projects.papacarlo.syntax.Expressions._
import name.lakhin.eliah.projects.papacarlo.syntax.NodeAccessor
import name.lakhin.eliah.projects.papacarlo.syntax.rules.{ExpressionRule, NamedRule}
import name.lakhin.eliah.projects.papacarlo.syntax.Rule._

object JavaIP {
  def tokenizer = {
    val tokenizer = new Tokenizer()

    import tokenizer._
    import Matcher._

    // lexis specification here

    tokenCategory(
      "whitespace",
      oneOrMore(anyOf(" \t\f\n")) // in terms of regexp: [:space:]+
    ).skip

    tokenCategory(
      "double",
      sequence(
        choice(  // in terms of regexp: 0|([1-9][0-9]*)
          chunk("0"),
          sequence(rangeOf('1', '9'), zeroOrMore(rangeOf('0', '9')))
        ),
        chunk("."),
        oneOrMore(rangeOf('0', '9'))
      )
    )

    tokenCategory(
      "longL",
      sequence(
        choice(  // in terms of regexp: 0|([1-9][0-9]*)
          chunk("0"),
          sequence(rangeOf('1', '9'), zeroOrMore(rangeOf('0', '9')))
        ),
        chunk("L"))
    )

    tokenCategory(
      "integer",
      choice(  // in terms of regexp: 0|([1-9][0-9]*)
        chunk("0"),
        sequence(rangeOf('1', '9'), zeroOrMore(rangeOf('0', '9')))
      )
    )

    tokenCategory(
      "string",
      sequence(
        chunk("\""),
        zeroOrMore(choice(
          anyExceptOf("\n\r\\\""),
          sequence(chunk("\\"), anyExceptOf("\n\r"))
        )),
        chunk("\"")
      )
    ).mutable

    tokenCategory(
      "char",
      sequence(
        chunk("'"),
        anyExceptOf("'"),
        chunk("'")
      )
    ).mutable

    tokenCategory(
      "annotationName",sequence(
        chunk("@"),
        choice(chunk("_"),rangeOf('a', 'z'),rangeOf('A','Z')),
        zeroOrMore(
          choice(
            chunk("_"),
            rangeOf('a', 'z'),
            rangeOf('A','Z'),
            rangeOf('0', '9'))))
    ).mutable

    tokenCategory(
      "identifier",
      sequence(
        choice(chunk("_"),rangeOf('a', 'z'),rangeOf('A','Z')),
        zeroOrMore(
          choice(
            chunk("_"),
            rangeOf('a', 'z'),
            rangeOf('A','Z'),
            rangeOf('0', '9'))))
    ).mutable

    terminals("(", ")", "%", "++","--","+", "-", "*", "/", "{", "}",";",":",".",",",
      "&&","||",
      "+=","-=","*=","/=",
      "==","!=","<=",">=","<",">",
      "!",
      "=","&","|","[","]",
      "?",
      "//","/*","*/")

    keywords(
      "true", "false",
      "null",
      "byte","int", "char", "short", "long", "float", "double", "void",
      "do", "while", "for", "switch", "case", "break", "return", "throw",
      "if","else",
      "import",
      "@interface",
      "class","interface",
      "private","protected","public",
      "static","native","final", "synchronized","abstract","volatile",
      "extends","implements","throws",
      "try", "catch","finally",
      "this",
      "new",
      "package",
      "instanceof"
    )

    tokenizer
  }

  private def contextualizer = {
    val contextualizer = new Contextualizer

    import contextualizer._

    // fragment specification here
    trackContext("/*", "*/").forceSkip.topContext
    trackContext("//", Token.LineBreakKind).forceSkip.topContext
    trackContext("[", "]").allowCaching
    trackContext("{", "}").allowCaching
    trackContext("\"", "\"").topContext

    contextualizer
  }

  def lexer = new Lexer(tokenizer, contextualizer)

  def syntax(lexer: Lexer) = new {
    val syntax = new Syntax(lexer)

    import syntax._
    import Rule._

    val qualifiedIdentifier = rule("qualifiedIdentifier") {
      oneOrMore(
        capture("part",token("identifier")),
        separator = token(".")
      )
    }

    // Expressions start

    val doubleLiteral = rule("doubleLiteral") {
      capture("value",token("double"))
    }

    val integerLiteral = rule("integerLiteral") {
      capture("value",token("integer"))
    }

    val longLiteral = rule("longLiteral") {
      capture("value",token("longL"))
    }

    val stringLiteral = rule("stringLiteral") {
      capture("value",token("string"))
    }

    val charLiteral = rule("charLiteral") {
      capture("value",token("char"))
    }

    val booleanLiteral = rule("booleanLiteral") {
      capture("value",choice(token("true"),token("false")))
    }

    val thisReference = rule("thisReference") {
      token("this")
    }

    val classReference = rule("classReference") {
      token("class")
    }

    val superReference = rule("superReference") {
      sequence(
        token("super"),
        optional(sequence(
          token("("),
          zeroOrMore(branch("actualParams",exp),separator = token(",")),
          token(")")
        ))
      )
    }

    val nullReference = rule("nullReference") {
      token("null")
    }

    val variableReference = rule("variableReference") {
      capture("name",token("identifier"))
    }

    val instantiation = rule("instantiation").transform{ orig =>
      if (orig.getBranches contains "classInst"){
        orig.getBranches("classInst").head
      } else {
        orig.getBranches("arrayInst").head
      }
    } {
      choice(
        branch("classInst",classInstantiation),
        branch("arrayInst",arrayInstantiation))
    }

    val classInstantiation = rule("classInstantiation"){
      sequence(
        token("new"),
        choice(branch("className",qualifiedIdentifier),
          branch("typeName",primitiveType)),
        optional(branch("genericParams",genericParams)),
        token("("),
        zeroOrMore(branch("actualParams",exp),separator = token(",")),
        token(")")
      )
    }

    val arrayInstantiation = rule("arrayInstantiation"){
      sequence(
        token("new"),
        choice(branch("className",qualifiedIdentifier),
          branch("typeName",primitiveType)),
        choice(sequence(
            token("["),
            capture("size",exp),
            token("]")),
          sequence(
            token("["),
            token("]"),
            token("{"),
            zeroOrMore(exp,separator = token(",")),
            token("}")
          ))
      )
    }

    val paren = rule("paren").transform { orig =>
      if (orig.getBranches contains "castedExp"){
        orig.accessor.setKind("castExp").node
      } else {
        orig
      }
    }{
      choice(
        sequence(
          token("("),
          capture("value",choice(typeUsage)),
          token(")"),
          branch("castedExp",exp)),
        sequence(
          token("("),
          capture("value",choice(exp)),
          token(")")
        )
      )
      /*sequence(
        token("("),
        capture("value",choice(exp,typeUsage)),
        token(")"),
        optional(branch("castedExp",exp))
      )*/
    }

    val assignment = rule("assignment"){
      sequence(
        branch("assigned",choice(qualifiedIdentifier,fieldAccess)),
        optional(sequence(
          token("["),
          exp,
          token("]")
        )),
        token("="),
        branch("value",exp)
      )
    }

    val arrayInit = rule("arrayInit"){
      sequence(token("{"),
        zeroOrMore(branch("value",exp),separator=token(",")),
        token("}"))
    }

    val expAtom = subrule("expAtom") {
      choice(
        arrayInit,
        assignment,
        thisReference,
        classReference,
        doubleLiteral,
        longLiteral,
        integerLiteral,
        stringLiteral,
        charLiteral,
        booleanLiteral,
        instantiation,
        superReference,
        nullReference,
        paren,
        variableReference
      )
    }

    val expAccess = rule("expAccess").transform { orig =>
      if (orig.getValues contains  "fieldName"){
        orig
      } else {
        orig.getBranches("value").head
      }
    } {
      sequence(
        branch("value",expAtom),
        zeroOrMore(sequence(
          token("."),
          choice(
            capture("fieldName",token("identifier")))
        ))
      )
    }

    val invocation = rule("invocation"){
      sequence(
        token("("),
        zeroOrMore(branch("actualParams",exp),separator = token(",")),
        token(")"))
    }

    val expMethodCall = rule("expMethodCall").transform{ orig =>
      if (orig.getBranches contains "invocation"){
        orig
      } else {
        orig.getBranches("base").head
      }
    } {
      sequence(
        branch("base",sequence(expAtom)),
        optional(branch("invocation",invocation)))
    }

    val expOpElement = subrule("expOpElement"){
      choice(expMethodCall,expAccess)
    }

    val expOp : NamedRule = rule("expression") {
      val rule =
        expression(branch("operand", expOpElement))

      postfix(rule,"++",1)
      postfix(rule,"--",1)
      prefix(rule,"++",1)
      prefix(rule,"--",1)
      prefix(rule,"!",1)
      postfix(rule, "%", 1)
      prefix(rule, "+", 2)
      prefix(rule, "-", 2)
      infix(rule, "*", 3)
      infix(rule, "/", 3, rightAssociativity = true)
      infix(rule, "+", 4)
      infix(rule, "-", 4)
      infix(rule, "==", 5)
      //infix(rule,"=",8)
      infix(rule, "!=", 5)
      infix(rule, ">=", 5)
      infix(rule, "<=", 5)
      infix(rule, "+=", 5)
      infix(rule, "-=", 5)
      infix(rule, "/=", 5)
      infix(rule, "*=", 5)
      infix(rule, "<", 5)
      infix(rule, ">", 5)
      infix(rule,"&&",7)
      infix(rule,"||",7)
      infix(rule,"&",7)
      infix(rule,"|",7)

      rule
    }

    val expArrayAccess = rule("expArrayAccess").transform { orig =>
      if (orig.getBranches contains  "index"){
       orig
      } else {
        orig.getBranches("value").head
      }
    } {
      sequence(
        branch("value",expOp),
        optional(sequence(token("["),
        branch("index",exp),
        token("]"))))
    }

    val arrayAccess = rule("arrayAccess"){
      sequence(
        branch("array",expOp),
        token("["),
        branch("index",exp),
        token("]")
      )
    }

    val chainExp = rule("chainExp").transform { orig =>
      if (orig.getBranches contains "chained"){
        orig
      } else {
        orig.getBranches("base").head
      }
    } {
      sequence(
        branch("base",expArrayAccess),
        zeroOrMore(sequence(
          token("."),
          branch("chained",expArrayAccess)
        ))
      )
    }

    //DA CONSIDERARE
    val instanceOfExp = rule ("instanceOfExpSuffix").transform { orig =>
      if (orig.getBranches contains "typeUsage"){
        orig
      } else {
        orig.getBranches("base").head
      }

    }{
      sequence(
        branch("base",chainExp),
        optional(sequence(
          token("instanceof"),
          branch("type",typeUsage)
        )))
    }


    val exp : Rule = subrule("expUsage") {
       instanceOfExp
    }

    // Expressions end

    val accessQualifier = rule("accessQualifier") {
      capture("name",choice(
        token("public"),
        token("protected"),
        token("private")
      ))
    }

    val qualifier = rule("qualifier") {
      choice(
        branch("access", accessQualifier),
        capture("static", token("static")),
        capture("final", token("final")),
        capture("abstract",token("abstract")),
        capture("volatile",token("volatile"))
      )
    }

    val classType : NamedRule = rule("classType") {
      sequence(
         capture("name",token("identifier")),
         optional(branch("genericParams",genericParams))
      )
    }

    val primitiveType = rule("primitiveType") {
      capture("name",choice(
        token("int"),
        token("byte"),
        token("char"),
        token("long"),
        token("float"),
        token("double"),
        token("boolean")
      ))
    }

    val arrayTypeMod = rule("arrayType") {
      sequence(
        token("["),
        token("]")
      )
    }

    val genericCapture = rule("genericCapture"){
      sequence(
        token("?"),
        optional(sequence(
          token("extends"),
          capture("baseType",typeUsage)
        ))
      )
    }

    val genericParams : NamedRule = rule("genericParams"){
      sequence(
        token("<"),
        zeroOrMore(branch("params",choice(genericCapture,typeUsage)),separator = token(",")),
        token(">")
      )
    }

    val typeUsage = rule("typeUsage") {
      sequence(
        branch("baseType", choice(
          primitiveType,
          classType
        )),
        zeroOrMore(branch("array",arrayTypeMod))
      )
    }

    val voidType = rule("voidType") {token("void")}

    val fieldDecl = rule("fieldDecl") {
      sequence(
        branch("annotations",zeroOrMore(annotationUsage)),
        branch("qualifiers",zeroOrMore(qualifier)),
        branch("type",typeUsage),
        oneOrMore(capture("name", token("identifier")),separator=token(",")),
        optional(
          sequence(
            token("="),
            branch("initializationValue",exp)
          )
        ),
        token(";").permissive
      )
    }

    val paramDecl = rule("paramDecl") {
      sequence(
        zeroOrMore(annotationUsage),
        branch("type",typeUsage),
        capture("name",token("identifier"))
      )
    }

    val constructorDecl = rule("constructorDecl") {
      sequence(
        branch("annotations",zeroOrMore(annotationUsage)),
        branch("qualifiers",zeroOrMore(qualifier)),
        capture("name", token("identifier")),
        token("("),
        zeroOrMore(
          branch("params", paramDecl),
          separator = token(",")
        ),
        recover(token(")"),"Missing closing parenthesis"),
        choice(
          token(";"),
          sequence(
            token("{"),
            zeroOrMore(branch("stmts",statement)),
            token("}"),
            optional(token(";"))
          )
        )
      )
    }

    val annotationUsage = rule("annotationUsage"){
      sequence(
        capture("name",token("annotationName")),
        optional(sequence(
          token("("),
          oneOrMore(capture("val",choice(sequence(token("identifier"),token("="),exp),exp)),separator=token(",")),
          token(")")
        ))
      )
    }

    val methodDecl = rule("methodDecl") {
      sequence(
        branch("annotations",zeroOrMore(annotationUsage)),
        branch("qualifiers",zeroOrMore(qualifier)),
        branch("returnType",
          choice(
            typeUsage,
            voidType
          )
        ),
        capture("name", token("identifier")),
        token("("),
        zeroOrMore(
          branch("params", paramDecl),
          separator = token(",")
        ),
        recover(token(")"),"Missing closing parenthesis"),
        optional(sequence(
          token("throws"),
          oneOrMore(branch("exceptionsThrown",qualifiedIdentifier),separator = token(","))
        )),
        choice(
          capture("abstractBody",token(";")),
          sequence(
            token("{"),
            zeroOrMore(branch("stmts",statement)),
            token("}"),
            optional(token(";"))
          )
        )
      )
    }

    val memberDecl :NamedRule = rule("memberDecl") {
      choice(
        branch("constructor",constructorDecl),
        branch("method",methodDecl),
        branch("field",fieldDecl),
        branch("class",javaClass),
        branch("interface",javaInterface)
      )
    }

    val javaClass = rule("class") {
      // Consists of three sequential parts: "[" token, series of nested
      // elements separated with "," token, and losing "]" token.
      sequence(
        branch("annotations",zeroOrMore(annotationUsage)),
        branch("qualifiers",zeroOrMore(qualifier)),
        token("class"),
        capture("name", token("identifier")),
        optional(sequence(
          token("extends"),
          branch("baseClass",qualifiedIdentifier)
        )),
        optional(
          sequence(
            token("implements"),
            oneOrMore(
              branch("interfaces",qualifiedIdentifier),
              separator = token(",")
            )
          )
        ),
        token("{"),
        branch("members",zeroOrMore(memberDecl)),
        //token("}")
        recover(token("}"), "class must end with '}'")
      )
    }

    val javaInterface = rule("javaInterface") {
      sequence(
        branch("annotations",zeroOrMore(annotationUsage)),
        branch("qualifiers",zeroOrMore(qualifier)),
        token("interface"),
        capture("name", token("identifier")),
        optional(sequence(
          token("extends"),
          oneOrMore(branch("baseClass",qualifiedIdentifier),separator=token(","))
        )),
        token("{"),
        branch("members",zeroOrMore(memberDecl)),
        recover(token("}"), "interface must end with '}'")
      )
    }

    val annotationDecl = rule("annotationDecl") {
      sequence(
        branch("annotations",zeroOrMore(annotationUsage)),
        branch("qualifiers",zeroOrMore(qualifier)),
        token("@interface"),
        capture("name", token("identifier")),
        token("{"),
        recover(token("}"), "annotation declaration must end with '}'")
      )
    }


    val importDir = rule("importDir") {
      sequence(
        token("import"),
        optional(capture("static",token("static"))),
        capture("part",token("identifier")),
        zeroOrMore(
          sequence(
            token("."),
            choice(
              capture("part",token("identifier")),
              capture("part",token("*"))
            )
          )
        ),
        token(";").permissive
      )
    }

    val packageDecl = rule("packageDecl") {
      sequence(
        token("package"),
        branch("packageName",qualifiedIdentifier),
        token(";")
      )
    }

    val compilationUnit = rule("compilationUnit").main {
      sequence(
        optional(branch("packageDecl",packageDecl)),
        branch("imports",zeroOrMore(importDir)),
        zeroOrMore(branch("classDeclaration",choice(javaInterface,javaClass,annotationDecl)))
      )
    }

    val fieldAccess = rule("fieldAccess"){
      sequence(
        token("this"),
        token("."),
        branch("field",qualifiedIdentifier)
      )
    }

    // Statements

    val expressionStatement = rule("expressionStatement"){
      sequence(
        branch("expression",exp),
        recover(token(";"),"semicolon missing")
      )
    }

    val emptyStatement = rule("emptyStatement"){
      token(";")
    }

    val returnStmt = rule("returnStmt"){
      sequence(
        token("return"),
        optional(branch("value",exp)),
        recover(token(";"),"semicolon missing")
      )
    }

    val localVarDecl = rule("localVarDecl") {
      sequence(
        branch("type",typeUsage),
        oneOrMore(
          sequence(
            capture("name", token("identifier")),
            optional(
              sequence(
                token("="),
                branch("initializationValue",exp)
              )
            )),separator = token(",")),
        recover(token(";"),"semicolon missing")
      )
    }

    val simpleLocalVarDecl = rule("simpleLocalVarDecl") {
      sequence(
        branch("type",typeUsage),
        oneOrMore(capture("name", token("identifier")),separator = token(","))
      )
    }

    val blockStmt : NamedRule = rule("blockStmt") {
      sequence(
        token("{"),
        zeroOrMore(branch("stmts",statement)),
        recover(token("}"),"Missing }"),
        optional(token(";"))
      )
    }

    val ifStmt : NamedRule = rule("ifStmt") {
      sequence(
        token("if"),
        token("("),
        branch("condition",exp),
        recover(token(")"),"closing parenthesis expected"),
        branch("then",statement),
        optional(sequence(
          token("else"),
          branch("else",statement)
        ))
      )
    }

    val forStmt = rule("forStmt") {
      sequence(
        token("for"),
        token("("),
        branch("init",statement),
        optional(exp),
        recover(token(";"),"semicolon missing"),
        optional(exp),
        recover(token(")"),"closing parenthesis expected"),
        branch("body",statement)
      )
    }

    val forEachStmt = rule("forEachStmt") {
      sequence(
        token("for"),
        token("("),
        branch("iterator",simpleLocalVarDecl),
        token(":"),
        branch("collection",exp),
        recover(token(")"),"closing parenthesis expected"),
        branch("body",statement)
      )
    }

    val whileStmt = rule("whileStmt") {
      sequence(
        token("while"),
        token("("),
        optional(exp),
        recover(token(")"),"closing parenthesis expected"),
        branch("body",statement)
      )
    }

    val catchClause = rule("catchClause"){
      sequence(
        token("catch"),
        token("("),
        oneOrMore(capture("exceptions",qualifiedIdentifier),separator=token("|")),
        token("identifier"),
        recover(token(")"),"closing parenthesis expected"),
        token("{"),
        zeroOrMore(statement),
        recover(token("}"),"Missing }"))
    }

    val tryWithResourceStmt = rule("tryWithResourceStmt") {
      sequence(
        token("try"),
        token("("),
        oneOrMore(sequence(typeUsage,token("identifier"),token("="),exp),separator=token(";")),
        token(")"),
        token("{"),
        zeroOrMore(statement),
        token("}"),
        zeroOrMore(branch("catch",catchClause)))
    }

    val tryStmt = rule("tryStmt") {
      sequence(
        token("try"),
        token("{"),
        zeroOrMore(statement),
        recover(token("}"),"closing bracket expected"),
        choice(
          sequence(
            oneOrMore(branch("catch",catchClause)),
            optional(sequence(
              token("finally"),
              token("{"),
              zeroOrMore(statement),
              token("}")
            ))),
          sequence(
            token("finally"),
            token("{"),
            zeroOrMore(statement),
            token("}"))
        ))
    }

    val throwStmt = rule("throwStmt") {
      sequence(
        token("throw"),
        branch("value",exp)
      )
    }

    val synchronizedStmt = rule("synchronizedStmt") {
      sequence(
        token("synchronized"),
        token("("),
        exp,
        token(")"),
        statement
      )
    }

    val statement : NamedRule = subrule("statement") {
      choice(
        localVarDecl,
        //assignment,
        returnStmt,
        blockStmt,
        expressionStatement,
        ifStmt,
        tryWithResourceStmt,
        tryStmt,
        whileStmt,
        forStmt,
        synchronizedStmt,
        forEachStmt,
        emptyStatement,
        throwStmt
     )
    }

  }.syntax

}