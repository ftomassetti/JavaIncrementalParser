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
        oneOrMore(choice(
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
      "=","&","|","[","]",
      "//","/*","*/")

    keywords(
      "true", "false",
      "null",
      "byte","int", "char", "short", "long", "float", "double", "void",
      "do", "while", "for", "switch", "case", "break", "return", "throw",
      "if","else",
      "import",
      "class","interface",
      "private","protected","public",
      "static","native","final", "synchronized","abstract",
      "extends","implements","throws",
      "try", "catch","finally",
      "this",
      "new"
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

    val stringLiteral = rule("stringLiteral") {
      capture("value",token("string"))
    }

    val charLiteral = rule("charLiteral") {
      capture("value",token("char"))
    }

    val booleanLiteral = rule("booleanLiteral") {
      choice(token("false"),token("true"))
    }

    val thisReference = rule("thisReference") {
      token("this")
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

    val instantiation = rule("instantiation") {
      choice(
        branch("classInst",classInstantiation),
        branch("arrayInst",arrayInstantiation))
    }

    val classInstantiation = rule("classInstantiation"){
      sequence(
        token("new"),
        branch("className",qualifiedIdentifier),
        token("("),
        zeroOrMore(branch("actualParams",exp),separator = token(",")),
        token(")")
      )
    }

    val arrayInstantiation = rule("arrayInstantiation"){
      sequence(
        token("new"),
        branch("className",qualifiedIdentifier),
        token("["),
        capture("size",exp),
        token("]")
      )
    }

    val paren = rule("paren"){
      sequence(
        token("("),
        capture("value",exp),
        token(")")
      )
    }

    val expAtom = subrule("expAtom") {
      choice(
        doubleLiteral,
        integerLiteral,
        stringLiteral,
        charLiteral,
        booleanLiteral,
        instantiation,
        variableReference,
        thisReference,
        superReference,
        nullReference,
        paren
      )
    }

    val expAccess = rule("expAccess") {
      sequence(
        branch("value",expAtom),
        optional(sequence(
          token("."),
          capture("fieldName",token("identifier"))
        ))
      )
    }

    val expMethodCall = rule("expMethodCall") {
      sequence(
        optional(
          sequence(expAtom,token("."))),
        capture("name",token("identifier")),
        token("("),
        zeroOrMore(branch("actualParams",exp),separator = token(",")),
        token(")"))
    }

    val castExp = rule("castExp"){
      sequence(
        token("("),
        branch("targetType",qualifiedIdentifier),
        token(")"),
        capture("castedValue",exp)
      )
    }

    val expOpElement = subrule("expOpElement"){
      choice(expMethodCall,castExp,expAccess)
    }

    val expOp : NamedRule = rule("expression") {
      val rule =
        expression(branch("operand", expOpElement))

      postfix(rule,"++",1)
      postfix(rule,"--",1)
      prefix(rule,"++",1)
      prefix(rule,"--",1)
      postfix(rule, "%", 1)
      prefix(rule, "+", 2)
      prefix(rule, "-", 2)
      infix(rule, "*", 3)
      infix(rule, "/", 3, rightAssociativity = true)
      infix(rule, "+", 4)
      infix(rule, "-", 4)
      infix(rule, "==", 5)
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

    val expArrayAccess = rule("expArrayAccess") {
      sequence(
        branch("value",expOp),
        optional(sequence(token("["),
        branch("index",exp),
        token("]"))))
    }

    val exp : Rule = subrule("expUsage") {
      expArrayAccess
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
        capture("abstract",token("abstract"))
      )
    }

    val classType = rule("classType") {
      capture("name",token("identifier"))
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
        branch("type",typeUsage),
        capture("name",token("identifier"))
      )
    }

    val constructorDecl = rule("constructorDecl") {
      sequence(
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

    val methodDecl = rule("methodDecl") {
      sequence(
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

    val memberDecl = rule("memberDecl") {
      choice(
        branch("constructor",constructorDecl),
        branch("method",methodDecl),
        branch("field",fieldDecl)
      )
    }

    val javaClass = rule("class") {
      // Consists of three sequential parts: "[" token, series of nested
      // elements separated with "," token, and losing "]" token.
      sequence(
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
        recover(token("}"), "class must end with '}'")
      )
    }

    val importDir = rule("importDir") {
      sequence(
        token("import"),
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

    val compilationUnit = rule("compilationUnit").main {
      sequence(
        branch("imports",zeroOrMore(importDir)),
        zeroOrMore(branch("classDeclaration",javaClass))
      )
    }

    // Statements

    val assignment = rule("assignment"){
      sequence(
        branch("assigned",qualifiedIdentifier),
        optional(sequence(
          token("["),
          exp,
          token("]")
        )),
        token("="),
        branch("value",exp),
        recover(token(";"),"semicolon missing")
      )
    }

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
        branch("value",exp),
        recover(token(";"),"semicolon missing")
      )
    }

    val localVarDecl = rule("localVarDecl") {
      sequence(
        branch("type",typeUsage),
        oneOrMore(capture("name", token("identifier")),separator = token(",")),
        optional(
          sequence(
            token("="),
            branch("initializationValue",exp)
          )
        ),
        recover(token(";"),"semicolon missing")
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
        choice(
          localVarDecl, // it parses already the semicolon
          token(";")),
        optional(exp),
        recover(token(";"),"semicolon missing"),
        optional(exp),
        recover(token(")"),"closing parenthesis expected"),
        token("{"),
        zeroOrMore(statement),
        recover(token("}"),"Missing }")
      )
    }

    val whileStmt = rule("whileStmt") {
      sequence(
        token("while"),
        token("("),
        optional(exp),
        recover(token(")"),"closing parenthesis expected"),
        token("{"),
        zeroOrMore(statement),
        recover(token("}"),"Missing }"),
        optional(token(";"))
      )
    }

    val tryStmt = rule("tryStmt") {
      sequence(
        token("try"),
        token("{"),
        zeroOrMore(statement),
        recover(token("}"),"closing bracket expected"),
        token("catch"),
        token("("),
        qualifiedIdentifier,
        token("identifier"),
        recover(token(")"),"closing parenthesis expected"),
        token("{"),
        zeroOrMore(statement),
        recover(token("}"),"Missing }"),
        optional(sequence(
          token("finally"),
          token("{"),
          zeroOrMore(statement),
          token("}")
        ))
      )
    }

    val statement : NamedRule = subrule("statement") {
      choice(
        expressionStatement,
        assignment,
        returnStmt,
        localVarDecl,
        blockStmt,
        ifStmt,
        tryStmt,
        whileStmt,
        forStmt,
        emptyStatement
     )
    }

  }.syntax

}