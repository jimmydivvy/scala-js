package org.scalajs.core.compiler


import scala.tools.nsc.{Global, Phase, plugins}
import scala.tools.nsc._
import org.scalajs.core.ir

//import ir.{ClassKind, Hashers, Trees => js, Types => jstpe}
//import ir.Trees.OptimizerHints

import org.scalajs.core.ir

/*with JSGlobalAddons*/

abstract class GenTSD extends plugins.PluginComponent with PluginComponent210Compat  {

  //val global: Global
  override val phaseName: String = "tsgen"
  override val runsAfter: List[String] = List("jscode","jsinterop")


  val jsAddons: JSGlobalAddons {
    val global: GenTSD.this.global.type
  }

  import global._
  import rootMirror._
  import definitions._

//  import jsAddons._
//  import jsDefinitions._
//  import jsInterop.{jsNameOf, fullJSNameOf}
//  import JSTreeExtractors._

  override def newPhase(prev: Phase): Phase = new TSDPhase(prev)

  protected def getSJSPlugin = global.plugins.collectFirst {
    case scalaJS:ScalaJSPlugin => scalaJS
  }

  //protected lazy val jsInterop = getSJSPlugin.get.jsAddons.jsInterop




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

//  def goClass( cls:ir.Trees.ClassDef ):Unit = {
//
//    val className = ir.Definitions.decodeClassName(cls.name.name)
//
//    //println("Class Name: " + className)
//
//    val clsSymbol = global.rootMirror.getClassIfDefined(className)
//
//    clsSymbol match {
//      case _: NoSymbol => ()
//      case clsSymbol: ClassSymbol => {
//
//        clsSymbol.tpe.members.collect {
//          case s if s.nameString.startsWith("$js$exported$") => println(s.nameString)
//        }
//
//        //clsSymbol.tpe.members.foreach(println)
//
//        //println(renderTSDClass(clsSymbol, cls))
//      }
//    }
//  }

  def renderTSDClass( cls:ClassSymbol ):String = {


    val exports = cls.info.members.collect {
      case m if jsAddons.jsInterop.isExport(m) => {
        val (realName, isProp) = jsAddons.jsInterop.jsExportInfo(m.name)
        val realMember = cls.info.member(TermName(realName)).asInstanceOf[MethodSymbol]

        renderTSDMethod(realMember)

      }
    }



    //cls.getAnn jsAddons.jsDefinitions.JSNameAnnotation

    val annotatedName = cls.getAnnotation(jsAddons.jsDefinitions.JSNameAnnotation).flatMap(_.stringArg(0))

    val fullJsClassName = annotatedName.getOrElse(cls.fullName)


    val (Seq(jsClassName), nsParts) = fullJsClassName.split("\\.").view.reverse.splitAt(1)

    val namespace = nsParts.reverse.mkString(".")



    //val jsClassName = jsAddons.jsInterop.fullJSNameOf(cls)

    s"""
       |declare module ${namespace} {
       |  class ${jsClassName} {
       |    ${exports.mkString("\n")}
       |  }
       |}
     """.stripMargin

  }

  lazy val prepComponent = getSJSPlugin.get.PrepInteropComponent

  /*val prep = getSJSPlugin.get.PrepInteropComponent*/

  class TSDPhase(prev:Phase) extends StdPhase(prev) {



    def parents(phase:Phase):Stream[Phase] = phase match {
      case null  => Stream.empty
      case phase => phase #:: parents(phase.prev)
    }

    override def apply(cunit: CompilationUnit): Unit = {

      //prepComponent.jsAddons.jsInterop.exportedSymbols


//      for( (key, value) <- prepComponent.jsAddons.jsInterop.exportedSymbols ) {
//        println(key)
//        println(value)
//      }
//

      def collectClassDefs(tree: Tree): List[ClassDef] = {
        tree match {
          case EmptyTree => Nil
          case PackageDef(_, stats) => stats flatMap collectClassDefs
          case cd: ClassDef => cd :: Nil
        }
      }
      val allClassDefs = collectClassDefs(cunit.body)

      allClassDefs
        .map(cdef => renderTSDClass(cdef.symbol.asInstanceOf[ClassSymbol]))
          .foreach(println)


      ()
    }
  }

}


/*with TypeKinds
//  with JSEncoding
//  with GenJSExports
//  with GenJSFiles*/
//
//abstract class GenTSD extends GenJSCode {
//
//  //plugins.PluginComponent with PluginComponent210Compat {
//
//  import global._
//  //import jsAddons._
//  import rootMirror._
//  import definitions._
////  import jsDefinitions._
////  import jsInterop.{jsNameOf, fullJSNameOf}
//  import JSTreeExtractors._
//
//  override val phaseName: String = "tsdgen"
//
//
////  class TSDPhase(prev:Phase) extends JSCodePhase(prev) {
////
////    override def apply(cunit: global.CompilationUnit): Unit = {
////
////      println(cunit.defined)
////
////      //super.apply(cunit)
////
////
////
////
////    }
////  }
////
////  override def newPhase(p: Phase): StdPhase = new TSDPhase(p)
//
//    //new JSCodePhase(p)
//
//  /*
//  //                val tpeEnteringPosterasure =
////                  enteringPhase(currentRun.erasurePhase)(sym.tpe.resultType)
//
//
//
////                println("Method: " + methodName.name)
////                println("JVM Type: " + sym.tpe.resultType)
////                println("Erase Type: " + tpeEnteringPosterasure)
////
////                tpeEnteringPosterasure match {
////                  case tpe: ErasedValueType => println("Erased: " + tpe.valueClazz)
////                  case _ => println("Other: " + tpeEnteringPosterasure.getClass)
////                }
////
//////                println("IR Type: " + resultIRType)
////
////                sym.tpe.resultType match {
////                  case ErasedValueType(_,_) => println("ERASED")
////                  case _ => println("NOT ERASED")
////                }
//
//   */
//

//
//  def go(tree: List[ir.Trees.Tree]):Unit = {
//
////    tree.map { t =>
////      enteringPhase(currentRun.erasurePhase)(t) match {
////        case cls: ir.Trees.ClassDef => {
////          goClass(cls)
////        }
////
////        case other => {
////          //println("OTHER: " + other)
////        }
////      }
////    }
//
//
//
//    tree.map {
//
//
//      case cls: ir.Trees.ClassDef => {
//
//
//
//        goClass(cls)
//      }
//
//      case other => {
//        //println("OTHER: " + other)
//      }
//    }
//
//  }
//
//
//  def generatedJSAST(clDefs: List[ir.Trees.Tree]): Unit = {
//
//
//    //global.rootMirror.findMemberFromRoot()
//
//    go(clDefs)
//
//
//    //ScalaJSPlugin.this.generatedJSAST(clDefs)
//  }
//
//
//
////  override val runsAfter: List[String] = List("jscode")
//
////  override def newPhase(prev: Phase): StdPhase = new TSDPhase(prev)
//
//
////  class TSDPhase(prev:Phase) extends StdPhase(prev) {
////    override def apply(cunit: global.CompilationUnit): Unit = {
////
////
////
////
////      def collectClassDefs(tree: Tree): List[ClassDef] = {
////        tree match {
////          case EmptyTree => Nil
////          case PackageDef(_, stats) => stats flatMap collectClassDefs
////          case cd: ClassDef => cd :: Nil
////        }
////      }
////      val allClassDefs = collectClassDefs(cunit.body)
////
////      println("GenTSD")
////      //println(prev)
////      println("All Class Defs: " + allClassDefs)
////
////      ()
////
////    }
////  }
//
//
//}