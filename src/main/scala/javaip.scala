package codemodels.incrementalparsers.javaip

import name.lakhin.eliah.projects.papacarlo.lexis.{Matcher, Tokenizer,
  Contextualizer, Token}
import name.lakhin.eliah.projects.papacarlo.{Syntax, Lexer}
import name.lakhin.eliah.projects.papacarlo.syntax.Rule
import name.lakhin.eliah.projects.papacarlo.syntax.rules.NamedRule

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
      "number",
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

    terminals("(", ")", "%", "+", "-", "*", "/", "{", "}",";",":",
      "&&","||","+=","-=","*=","/=","&","|")

    keywords("true", "false", "null", 
      "byte","int", "char", "short", "long", "float", "double", "void",
      "do", "while", "for", "switch", "case", "break", "return",
      "class","private","protected","public","static","native",
      "synchronized")

    tokenizer
  }

  private def contextualizer = {
    val contextualizer = new Contextualizer

    import contextualizer._

    // fragment specification here

    contextualizer
  }

  def lexer = new Lexer(tokenizer, contextualizer)

  def syntax(lexer: Lexer) = new {
    val syntax = new Syntax(lexer)

    import syntax._
    import Rule._

    // syntax rule specification here

  }.syntax
}