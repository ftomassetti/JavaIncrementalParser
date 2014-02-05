package codemodels.incrementalparsers.javaip

import name.lakhin.eliah.projects.papacarlo.lexis.{Matcher, Tokenizer,
  Contextualizer}
import name.lakhin.eliah.projects.papacarlo.{Syntax, Lexer}
import name.lakhin.eliah.projects.papacarlo.syntax.Rule
import name.lakhin.eliah.projects.papacarlo.syntax.rules.NamedRule

import org.scalatest._

abstract class UnitSpec extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors

class MySpec extends UnitSpec {

  it should "parse a basic class" in {
    val code = "class A { }"
    val lexer = JavaIP.tokenizer
    val tokens = lexer.tokenize(code)
    assert(7==tokens.length)
    assert("class"==tokens(0).kind)
    assert("class"==tokens(0).value)
    assert("whitespace"==tokens(1).kind)
    assert(" "==tokens(1).value)
    assert("identifier"==tokens(2).kind)
    assert("A"==tokens(2).value)
    assert("whitespace"==tokens(3).kind)
    assert(" "==tokens(3).value)
    assert("{"==tokens(4).kind)
    assert("{"==tokens(4).value)
    assert("whitespace"==tokens(5).kind)
    assert(" "==tokens(5).value)
    assert("}"==tokens(6).kind)
    assert("}"==tokens(6).value)
  }

}
