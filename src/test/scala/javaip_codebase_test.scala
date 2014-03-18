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

  it should "parse Scribble.java" in {
    parseWithoutErrors("Scribble")
  }

  it should "parse Animator.java" in {
    parseWithoutErrors("Animator")
  }

  it should "parse AllComponents.java" in {
    parseWithoutErrors("AllComponents")
  }

  it should "parse AbstactWekaProgrammingLanguageTokenizer.java" in {
    parseWithoutErrors("AbstactWekaProgrammingLanguageTokenizer")
  }

  it should "parse FileCopy partial 1.java.code" in {
    parseWithoutErrors("FileCopy_partial_1")
  }

  it should "parse FileCopy partial 2.java.code" in {
    parseWithoutErrors("FileCopy_partial_2")
  }

  it should "parse FileCopy.java.code" in {
    parseWithoutErrors("FileCopy")
  }

  it should "parse Mud partial 1.java.code" in {
    parseWithoutErrors("Mud_partial_1")
  }

  /*it should "parse Mud.java.code" in {
    parseWithoutErrors("Mud")
  }*/
/*
  it should "parse Soundmap.java.code" in {
    parseWithoutErrors("Soundmap")
  }*/

}
