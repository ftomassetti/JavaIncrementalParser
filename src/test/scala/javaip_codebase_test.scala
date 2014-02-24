import codemodels.incrementalparsers.javaip.{PapaCarloUnitSpec, JavaIP}

import name.lakhin.eliah.projects.papacarlo.syntax.Node
import name.lakhin.eliah.projects.papacarlo.test.utils.ErrorMonitor

class javaip_codebase_test extends PapaCarloUnitSpec {

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


  it should "parse Scribble.java" in {
    val filename = "src/test/scala/codebase/Scribble.java.code"
    val code = scala.io.Source.fromFile(filename).mkString

    val lexer = JavaIP.lexer
    val syntax = JavaIP.syntax(lexer)
    val m = new ErrorMonitor(lexer,syntax)
    lexer.input(code)
    assert(0==syntax.getErrors.size)
  }

}
