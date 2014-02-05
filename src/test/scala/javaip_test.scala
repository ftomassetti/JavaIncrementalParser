package codemodels.incrementalparsers.javaip

import name.lakhin.eliah.projects.papacarlo.lexis.{Matcher, Tokenizer,
  Contextualizer, Token}
import name.lakhin.eliah.projects.papacarlo.{Syntax, Lexer}
import name.lakhin.eliah.projects.papacarlo.syntax.Rule
import name.lakhin.eliah.projects.papacarlo.syntax.rules.NamedRule

import org.scalatest._

abstract class UnitSpec extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors

class MySpec extends UnitSpec {

  private def check_token(kind:String,value:String,token:Token) : Unit = {
    assert(kind==token.kind)
    assert(value==token.value)
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

}
