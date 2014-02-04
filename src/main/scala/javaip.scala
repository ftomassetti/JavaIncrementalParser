package codemodels.incrementalparsers.javaip

import name.lakhin.eliah.projects.papacarlo.lexis.{Matcher, Tokenizer,
  Contextualizer}
import name.lakhin.eliah.projects.papacarlo.{Syntax, Lexer}
import name.lakhin.eliah.projects.papacarlo.syntax.Rule
import name.lakhin.eliah.projects.papacarlo.syntax.rules.NamedRule

object JavaIP {
  private def tokenizer = {
    val tokenizer = new Tokenizer()

    import tokenizer._
    import Matcher._

    // lexis specification here

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