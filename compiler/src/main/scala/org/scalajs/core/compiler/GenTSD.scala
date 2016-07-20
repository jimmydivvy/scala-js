package org.scalajs.core.compiler


import scala.tools.nsc.{Global, Phase, plugins}
import scala.tools.nsc._
import org.scalajs.core.ir

//import ir.{ClassKind, Hashers, Trees => js, Types => jstpe}
//import ir.Trees.OptimizerHints

import org.scalajs.core.ir

/*with TypeKinds
//  with JSEncoding
//  with GenJSExports
//  with GenJSFiles*/

abstract class GenTSD extends GenJSCode {

  //plugins.PluginComponent with PluginComponent210Compat {

  import global._
  //import jsAddons._
  import rootMirror._
  import definitions._
//  import jsDefinitions._
//  import jsInterop.{jsNameOf, fullJSNameOf}
  import JSTreeExtractors._

  override val phaseName: String = "tsdgen"


//  class TSDPhase(prev:Phase) extends JSCodePhase(prev) {
//
//    override def apply(cunit: global.CompilationUnit): Unit = {
//
//      println(cunit.defined)
//
//      //super.apply(cunit)
//
//
//
//
//    }
//  }
//
//  override def newPhase(p: Phase): StdPhase = new TSDPhase(p)

    //new JSCodePhase(p)

  /*
  //                val tpeEnteringPosterasure =
//                  enteringPhase(currentRun.erasurePhase)(sym.tpe.resultType)



//                println("Method: " + methodName.name)
//                println("JVM Type: " + sym.tpe.resultType)
//                println("Erase Type: " + tpeEnteringPosterasure)
//
//                tpeEnteringPosterasure match {
//                  case tpe: ErasedValueType => println("Erased: " + tpe.valueClazz)
//                  case _ => println("Other: " + tpeEnteringPosterasure.getClass)
//                }
//
////                println("IR Type: " + resultIRType)
//
//                sym.tpe.resultType match {
//                  case ErasedValueType(_,_) => println("ERASED")
//                  case _ => println("NOT ERASED")
//                }

   */

  def renderTSDType( tpe:Type ):String = {
    if( tpe.typeArgs.isEmpty ) {
      tpe.typeSymbol.fullName
    } else {
      tpe.typeSymbol.fullName + "<" + tpe.typeArgs.map(renderTSDType).mkString(",") + ">"
    }
  }

  def renderTSDMethodName( methodSymbol: MethodSymbol ):String = {
    if( methodSymbol.isConstructor ) "constructor"
    else methodSymbol.nameString
  }

  def renderTSDMethod( method:MethodSymbol ):String = {
    val methodName = renderTSDMethodName(method)

    val unerasedReturnType = enteringPhase(currentRun.erasurePhase)(method.tpe.resultType)

    val paramList = method.paramLists
      .flatten
      .map { param =>

        val paramName = param.nameString
        val paramType = enteringPhase(currentRun.erasurePhase)(param.tpe)
        val tsdType   = renderTSDType(paramType)

        s"${paramName}: ${tsdType}"
      }
      .mkString(", ")


    s"${methodName}(${paramList}): ${renderTSDType(unerasedReturnType)}"

  }

  def goClass( cls:ir.Trees.ClassDef ):Unit = {

    val className = ir.Definitions.decodeClassName(cls.name.name)

    //println("Class Name: " + className)

    val clsSymbol = global.rootMirror.getClassIfDefined(className)

    clsSymbol match {
      case _: NoSymbol => ()
      case clsSymbol: ClassSymbol => {
        println(renderTSDClass(clsSymbol, cls))
      }
    }
  }

  def renderTSDClass( cls:ClassSymbol, jsClass:ir.Trees.ClassDef ):String = {



    val memberStrings = jsClass.defs.map {

      case field:ir.Trees.FieldDef => {
        "(FIELD)"
      }

      case method: ir.Trees.MethodDef => {
        //println("Method: " + method)

        val realName = method.name match {
          case ir.Trees.Ident(name, _) => ir.Definitions.decodeMethodName(name)._1
          case ir.Trees.StringLiteral(name) => name
        }

        val methodSymbol = cls.info.member(TermName(realName))

        methodSymbol match {
          case methodSymbol: MethodSymbol => {
            renderTSDMethod(methodSymbol)
          }
        }
      }
    }


    val className = ir.Definitions.decodeClassName(jsClass.name.name)

    s"""
      |class ${className} {
      | ${memberStrings.map("\t" + _).mkString("\n")}
      |}
    """.stripMargin
  }

  def go(tree: List[ir.Trees.Tree]):Unit = {

//    tree.map { t =>
//      enteringPhase(currentRun.erasurePhase)(t) match {
//        case cls: ir.Trees.ClassDef => {
//          goClass(cls)
//        }
//
//        case other => {
//          //println("OTHER: " + other)
//        }
//      }
//    }



    tree.map {


      case cls: ir.Trees.ClassDef => {
        goClass(cls)
      }

      case other => {
        //println("OTHER: " + other)
      }
    }

  }


  def generatedJSAST(clDefs: List[ir.Trees.Tree]): Unit = {


    //global.rootMirror.findMemberFromRoot()

    go(clDefs)


    //ScalaJSPlugin.this.generatedJSAST(clDefs)
  }



//  override val runsAfter: List[String] = List("jscode")

//  override def newPhase(prev: Phase): StdPhase = new TSDPhase(prev)


//  class TSDPhase(prev:Phase) extends StdPhase(prev) {
//    override def apply(cunit: global.CompilationUnit): Unit = {
//
//
//
//
//      def collectClassDefs(tree: Tree): List[ClassDef] = {
//        tree match {
//          case EmptyTree => Nil
//          case PackageDef(_, stats) => stats flatMap collectClassDefs
//          case cd: ClassDef => cd :: Nil
//        }
//      }
//      val allClassDefs = collectClassDefs(cunit.body)
//
//      println("GenTSD")
//      //println(prev)
//      println("All Class Defs: " + allClassDefs)
//
//      ()
//
//    }
//  }


}