import codemodels.incrementalparsers.javaip.{PapaCarloUnitSpec, JavaIP}

import name.lakhin.eliah.projects.papacarlo.syntax.Node
import name.lakhin.eliah.projects.papacarlo.test.utils.ErrorMonitor

class javaip_codebase_test extends PapaCarloUnitSpec {

  def parseWithoutErrors(name:String) {
    val filename = "src/test/scala/codebase/"+name+".java.code"
    val code = scala.io.Source.fromFile(filename).mkString

    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    val m = new ErrorMonitor(lexer,syntax)
    lexer.input(code)
    assert(0==syntax.getErrors.size,m.getResult)
  }

 it should "parse typical compilation unit " in {
    val code = """
                 // This example is from the book _Java in a Nutshell_ by David Flanagan.
                 // Written by David Flanagan.  Copyright (c) 1996 O'Reilly & Associates.
                 // You may study, use, modify, and distribute this example for any purpose.
                 // This example is provided WITHOUT WARRANTY either expressed or implied.

                  import java.applet.*;
                  import java.awt.*;

                  public class Scribble extends Applet {
                  }"""
    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    var classes = Map[String,Node]()
    var imports = List[Node]()
    syntax.onNodeMerge.bind {node => {
      imports = node.getBranches("imports")
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
    assert(classes contains "Scribble")
    assert(1==classes.get("Scribble").get.getBranches("qualifiers").size)
  }

  it should "parse Scribble.java partial 1" in {
    val filename = "src/test/scala/codebase/Scribble_partial_1.java.code"
    val code = scala.io.Source.fromFile(filename).mkString

    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    val m = new ErrorMonitor(lexer,syntax)
    lexer.input(code)
    assert(0==syntax.getErrors.size)
  }

  it should "parse Scribble.java partial 2" in {
    val filename = "src/test/scala/codebase/Scribble_partial_2.java.code"
    val code = scala.io.Source.fromFile(filename).mkString

    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    val m = new ErrorMonitor(lexer,syntax)
    lexer.input(code)
    assert(0==syntax.getErrors.size)
  }

  it should "parse Scribble.java partial 3a" in {
    val filename = "src/test/scala/codebase/Scribble_partial_3a.java.code"
    val code = scala.io.Source.fromFile(filename).mkString

    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    val m = new ErrorMonitor(lexer,syntax)
    lexer.input(code)
    assert(0==syntax.getErrors.size,m.getResult)
  }

  it should "parse Scribble.java partial 3b" in {
    val filename = "src/test/scala/codebase/Scribble_partial_3b.java.code"
    val code = scala.io.Source.fromFile(filename).mkString

    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    val m = new ErrorMonitor(lexer,syntax)
    lexer.input(code)
    assert(0==syntax.getErrors.size,m.getResult)
  }

  it should "parse Scribble.java partial 3" in {
    val filename = "src/test/scala/codebase/Scribble_partial_3.java.code"
    val code = scala.io.Source.fromFile(filename).mkString

    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    val m = new ErrorMonitor(lexer,syntax)
    lexer.input(code)
    assert(0==syntax.getErrors.size,m.getResult)
  }

  it should "parse Scribble.java partial 4" in {
    val filename = "src/test/scala/codebase/Scribble_partial_4.java.code"
    val code = scala.io.Source.fromFile(filename).mkString

    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    val m = new ErrorMonitor(lexer,syntax)
    lexer.input(code)
    assert(0==syntax.getErrors.size,m.getResult)
  }

  it should "parse Scribble.java partial 5" in {
    val filename = "src/test/scala/codebase/Scribble_partial_5.java.code"
    val code = scala.io.Source.fromFile(filename).mkString

    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    val m = new ErrorMonitor(lexer,syntax)
    lexer.input(code)
    assert(0==syntax.getErrors.size,m.getResult)
  }

  it should "parse Scribble.java" in {
    val filename = "src/test/scala/codebase/Scribble.java.code"
    val code = scala.io.Source.fromFile(filename).mkString

    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    val m = new ErrorMonitor(lexer,syntax)
    lexer.input(code)
    assert(0==syntax.getErrors.size,m.getResult)
  }

  it should "parse Animator.java partial 1a" in {
    parseWithoutErrors("Animator_partial_1a")
  }

  it should "parse Animator.java partial 1b" in {
    parseWithoutErrors("Animator_partial_1b")
  }

  it should "parse Animator.java partial 1c" in {
    parseWithoutErrors("Animator_partial_1c")
  }

  it should "parse Animator.java partial 1d" in {
    parseWithoutErrors("Animator_partial_1d")
  }

  it should "parse Animator.java partial 1e" in {
    parseWithoutErrors("Animator_partial_1e")
  }

  it should "parse Animator.java partial 1" in {
    parseWithoutErrors("Animator_partial_1")
  }

  it should "parse Animator.java partial 2" in {
    parseWithoutErrors("Animator_partial_2")
  }

  it should "parse Animator.java partial 3" in {
    parseWithoutErrors("Animator_partial_3")
  }

  it should "parse Animator.java partial 4a" in {
    parseWithoutErrors("Animator_partial_4a")
  }

  it should "parse Animator.java partial 4b" in {
    parseWithoutErrors("Animator_partial_4b")
  }

  it should "parse Animator.java partial 4c" in {
    parseWithoutErrors("Animator_partial_4c")
  }

  it should "parse Animator.java partial 4d" in {
    parseWithoutErrors("Animator_partial_4d")
  }

  it should "parse Animator.java partial 4e" in {
    parseWithoutErrors("Animator_partial_4e")
  }

  it should "parse Animator.java partial 4e var 1" in {
    parseWithoutErrors("Animator_partial_4e_var1")
  }

  it should "parse Animator.java partial 4e var 2" in {
    parseWithoutErrors("Animator_partial_4e_var2")
  }

  it should "parse Animator.java partial 4e var 4" in {
    parseWithoutErrors("Animator_partial_4e_var4")
  }

  it should "parse Animator.java partial 4e var 4b" in {
    parseWithoutErrors("Animator_partial_4e_var4b")
  }

  it should "parse Animator.java partial 4e var 4c" in {
    parseWithoutErrors("Animator_partial_4e_var4c")
  }

  it should "parse Animator.java partial 4e var 4d" in {
    parseWithoutErrors("Animator_partial_4e_var4d")
  }

  it should "parse Animator.java partial 4e var 5" in {
    parseWithoutErrors("Animator_partial_4e_var5")
  }

  it should "parse Animator.java partial 4f" in {
    parseWithoutErrors("Animator_partial_4f")
  }

  it should "parse Animator.java partial 4g" in {
    parseWithoutErrors("Animator_partial_4g")
  }

  /*it should "parse Animator.java" in {
    parseWithoutErrors("Animator")
  }*/

  it should "parse AllComponents.java partial 1" in {
    parseWithoutErrors("AllComponents_partial_1")
  }

  it should "parse AllComponents.java partial 2a" in {
    parseWithoutErrors("AllComponents_partial_2a")
  }

  it should "parse AllComponents.java partial 2b" in {
    parseWithoutErrors("AllComponents_partial_2b")
  }

  it should "parse AllComponents.java partial 2c" in {
    parseWithoutErrors("AllComponents_partial_2c")
  }

  it should "parse AllComponents.java partial 2d" in {
    parseWithoutErrors("AllComponents_partial_2d")
  }

  it should "parse AllComponents.java partial 2e" in {
    parseWithoutErrors("AllComponents_partial_2e")
  }

  it should "parse AllComponents.java partial 2f" in {
    parseWithoutErrors("AllComponents_partial_2f")
  }

  it should "parse AllComponents.java partial 2g" in {
    parseWithoutErrors("AllComponents_partial_2g")
  }

  it should "parse AllComponents.java partial 2" in {
    parseWithoutErrors("AllComponents_partial_2")
  }

  it should "parse AllComponents.java" in {
    parseWithoutErrors("AllComponents")
  }

  it should "parse AbstactWekaProgrammingLanguageTokenizer partial 1.java" in {
    parseWithoutErrors("AbstactWekaProgrammingLanguageTokenizer_partial_1")
  }

  it should "parse AbstactWekaProgrammingLanguageTokenizer partial 2.java" in {
    parseWithoutErrors("AbstactWekaProgrammingLanguageTokenizer_partial_2")
  }

  it should "parse AbstactWekaProgrammingLanguageTokenizer partial 2a.java" in {
    parseWithoutErrors("AbstactWekaProgrammingLanguageTokenizer_partial_2a")
  }

  it should "parse AbstactWekaProgrammingLanguageTokenizer partial 3.java" in {
    parseWithoutErrors("AbstactWekaProgrammingLanguageTokenizer_partial_3")
  }

  it should "parse AbstactWekaProgrammingLanguageTokenizer partial 3a.java" in {
    parseWithoutErrors("AbstactWekaProgrammingLanguageTokenizer_partial_3a")
  }

  it should "parse AbstactWekaProgrammingLanguageTokenizer partial 4.java" in {
    parseWithoutErrors("AbstactWekaProgrammingLanguageTokenizer_partial_4")
  }

  it should "parse AbstactWekaProgrammingLanguageTokenizer.java" in {
    parseWithoutErrors("AbstactWekaProgrammingLanguageTokenizer")
  }

}
