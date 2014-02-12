package codemodels.incrementalparsers.javaip

import name.lakhin.eliah.projects.papacarlo.lexis.{Matcher, Tokenizer,
  Contextualizer, Token}
import name.lakhin.eliah.projects.papacarlo.{Syntax, Lexer}
import name.lakhin.eliah.projects.papacarlo.syntax.Rule
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

  it should "parse a basic class" in {
    val code = "class A { }"
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var classes = List[String]()
    syntax.onNodeMerge.bind {node => {
      classes ::= node.getValue("name")
    }}
    lexer.input(code)

    assert(1==classes.size)
    assert("A"==classes.head)
  }

}