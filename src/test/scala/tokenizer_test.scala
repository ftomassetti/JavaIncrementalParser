package codemodels.incrementalparsers.javaip

import name.lakhin.eliah.projects.papacarlo.lexis.Token

class TokenizerSpec extends PapaCarloUnitSpec {

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

  /*def getTokens(code:String) : Unit = {
    val lexer = JavaIP.lexer // here you are initializing Tokenizer and Contextualizer both
    lexer.input(code)
    val tokens = lexer.
    println("TOKENS "+tokens)
    //.getTokens.filterNot(node => node.kind=="whitespace" || node.kind=="comment")
  }

  it should "parse annidated line comments inside method" in {
     val code = "class Animator {\n\n    void run() {\n            //this.getToolkit().sync();  // Force it to be drawn *now*.\n    }\n}"
     getTokens(code)
     val lexer = JavaIP.tokenizer
     val tokens = lexer.tokenize(code).filterNot(node => node.kind=="whitespace" || node.kind=="comment")
     println("Number of tokens: "+tokens.length)
     tokens.foreach(t => println("* "+t.kind+", '"+t.value+"'"))
     assert(10==tokens.length)
   }

   it should "parse line comments inside method" in {
     val code = "class Animator {\n\n    void run() {\n            //this.getToolkit().sync();\n    }\n}"
     val lexer = JavaIP.tokenizer
     val tokens = lexer.tokenize(code).filterNot(node => node.kind=="whitespace" || node.kind=="comment")
     //println("Number of tokens: "+tokens.length)
     //tokens.foreach(t => println("* "+t.kind+", '"+t.value+"'"))
     assert(10==tokens.length)
   }*/


}