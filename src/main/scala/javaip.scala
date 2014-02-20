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
      "integer",
      choice(  // in terms of regexp: 0|([1-9][0-9]*)
        chunk("0"),
        sequence(rangeOf('1', '9'), zeroOrMore(rangeOf('0', '9')))
      )
    )

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
    )

    terminals("(", ")", "%", "+", "-", "*", "/", "{", "}",";",":",".",",",
      "&&","||","+=","-=","*=","/=","==","=","&","|","[","]",
      "//","/*","*/")

    keywords(
      "true", "false",
      "null",
      "byte","int", "char", "short", "long", "float", "double", "void",
      "do", "while", "for", "switch", "case", "break", "return", "throw",
      "import",
      "class","interface",
      "private","protected","public",
      "static","native","final", "synchronized","abstract",
      "extends","implements","throws",
      "this"
    )

    tokenizer
  }

  private def contextualizer = {
    val contextualizer = new Contextualizer

    import contextualizer._

    // fragment specification here
    //trackContext("[", "]").allowCaching
    trackContext("{", "}").allowCaching
    trackContext("/*", "*/").forceSkip.topContext
    trackContext("//", Token.LineBreakKind).forceSkip.topContext
    //trackContext("&quot;", "&quot;").topContext

    contextualizer
  }

  def lexer = new Lexer(tokenizer, contextualizer)

  def syntax(lexer: Lexer) = new {
    val syntax = new Syntax(lexer)

    import syntax._
    import Rule._

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
        capture("name", token("identifier")),
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
        token(")"),
        optional(
          sequence(
            token("{"),
            zeroOrMore(branch("stmts",statement)),
            token("}")
          )
        ),
        optional(token(";"))
      )
    }

    val memberDecl = rule("memberDecl") {
      choice(
        branch("method",methodDecl),
        branch("field",fieldDecl)
      )
    }

    val qualifiedIdentifier = rule("qualifiedIdentifier") {
      oneOrMore(
        capture("part",token("identifier")),
        separator = token(".")
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
        token("}"),
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
        branch("classDeclaration",javaClass)
      )
    }

    val integerLiteral = rule("integerLiteral") {
      capture("value",token("integer"))
    }

    val thisReference = rule("thisReference") {
      token("this")
    }

    val variableReference = rule("variableReference") {
      capture("name",token("identifier"))
    }

    val expElement = subrule("expElement") {
      choice(
        integerLiteral,
        variableReference,
        thisReference
      )
    }

    val expressionStatement = rule("expressionStatement"){
      sequence(
        branch("expression",exp),
        token(";")
      )
    }

    val emptyStatement = rule("emptyStatement"){
      token(";")
    }

    val statement = subrule("statement") {
      choice(expressionStatement,emptyStatement)
    }

    val expComp : NamedRule = rule("expression") {
      val rule =
        expression(branch("operand", expElement))

      group(rule, "(", ")")
      postfix(rule, "%", 1)
      prefix(rule, "+", 2)
      prefix(rule, "-", 2)
      infix(rule, "*", 3)
      infix(rule, "/", 3, rightAssociativity = true)
      infix(rule, "+", 4)
      infix(rule, "-", 4)

      rule
    }

    val fieldAccess = rule("fieldAccess") {
      sequence(
        branch("container",expComp),
        token("."),
        capture("fieldName",token("identifier"))
      )
    }

    val exp = subrule("expUsage") {
      choice(fieldAccess,expComp)
    }

  }.syntax
}