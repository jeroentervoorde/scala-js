/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.backend.closure

import com.google.javascript.jscomp.CompilerOptions.Reach

import scala.collection.JavaConverters._
import com.google.javascript.jscomp.{
  Compiler => ClosureCompiler,
  CompilerOptions => ClosureOptions,
  SourceFile => ClosureSource,
  _
}
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.javascript.ESLevel
import org.scalajs.core.tools.logging.Logger
import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.linker.LinkingUnit
import org.scalajs.core.tools.linker.analyzer.SymbolRequirement
import org.scalajs.core.tools.linker.backend._
import org.scalajs.core.tools.linker.backend.emitter.{CoreJSLibs, Emitter}

import scala.util.Properties

/** The Closure backend of the Scala.js linker.
 *
 *  Runs a the Google Closure Compiler in advanced mode on the emitted code.
 *  Use this for production builds.
 */
final class ClosureLinkerBackend(
    semantics: Semantics,
    moduleKind: ModuleKind,
    withSourceMap: Boolean,
    config: LinkerBackend.Config
) extends LinkerBackend(semantics, ESLevel.ES5, moduleKind, withSourceMap,
    config) {

  @deprecated("Use the overload with an explicit ModuleKind", "0.6.13")
  def this(semantics: Semantics, withSourceMap: Boolean,
      config: LinkerBackend.Config) {
    this(semantics, ModuleKind.NoModule, withSourceMap, config)
  }

  private[this] val emitter = {
    new Emitter(semantics, OutputMode.ECMAScript51Isolated, moduleKind)
      .withOptimizeBracketSelects(false)
  }

  val symbolRequirements: SymbolRequirement = emitter.symbolRequirements

  private val needsIIFEWrapper = moduleKind match {
    case ModuleKind.NoModule       => true
    case ModuleKind.CommonJSModule => false
  }

  private def toClosureSource(file: VirtualJSFile) =
    ClosureSource.fromReader(file.toURI.toString(), file.reader)

  /** Emit the given [[LinkingUnit]] to the target output
   *
   *  @param unit [[LinkingUnit]] to emit
   *  @param output File to write to
   */
  def emit(unit: LinkingUnit, output: WritableVirtualJSFile,
      logger: Logger): Unit = {
    verifyUnit(unit)

    val builder = new ClosureAstBuilder(config.relativizeSourceMapBase)

    // Build Closure IR
    logger.time("Emitter (create Closure trees)") {
      emitter.emit(unit, builder, logger)
    }

    // Build a Closure JSModule which includes the core libs
    val module = new JSModule("Scala.js")

    module.add(new CompilerInput(toClosureSource(
        CoreJSLibs.lib(semantics, OutputMode.ECMAScript51Isolated, moduleKind))))

    val ast = builder.closureAST
    module.add(new CompilerInput(ast, ast.getInputId(), false))

    // Compile the module
    val closureExterns = List(
        toClosureSource(ClosureLinkerBackend.ScalaJSExternsFile),
        toClosureSource(makeExternsForExports(unit)))
    val options = closureOptions(output.name)
    val compiler = closureCompiler(logger)



    logger.time("Closure: Write result") {

      if (Properties.envOrNone("AST_ONLY").exists(_.toBoolean)) {

        writeAst(compiler, module, output)

      } else {

        val result = logger.time("Closure: Compiler pass") {
          compiler.compileModules(
            closureExterns.asJava, List(module).asJava, options)
        }
        writeResult(result, compiler, output)
      }

    }
  }

  /** Constructs an externs file listing all exported properties in a linking
   *  unit.
   *
   *  This is necessary to avoid name clashes with renamed properties (#2491).
   */
  private def makeExternsForExports(linkingUnit: LinkingUnit): VirtualJSFile = {
    import org.scalajs.core.ir.Trees._

    def exportName(tree: Tree): Option[String] = (tree: @unchecked) match {
      case MethodDef(_, StringLiteral(name), _, _, _) => Some(name)
      case PropertyDef(_, StringLiteral(name), _, _)  => Some(name)
      case _                                          => None
    }

    val exportedPropertyNames = for {
      classDef <- linkingUnit.classDefs
      member <- classDef.exportedMembers
      name <- exportName(member.tree)
      if isValidIdentifier(name)
    } yield {
      name
    }

    val content = new java.lang.StringBuilder
    for (exportedPropertyName <- exportedPropertyNames.distinct)
      content.append(s"Object.prototype.$exportedPropertyName = 0;\n")

    new MemVirtualJSFile("ScalaJSExportExterns.js")
      .withContent(content.toString())
  }

  private def closureCompiler(logger: Logger) = {
    val compiler = new ClosureCompiler
    compiler.setErrorManager(new LoggerErrorManager(logger))
    compiler
  }

  private def writeResult(result: Result, compiler: ClosureCompiler,
      output: WritableVirtualJSFile): Unit = {
    def withNewLine(str: String): String = if (str == "") "" else str + "\n"
    def ifIIFE(str: String): String = if (needsIIFEWrapper) str else ""

    val (header0, footer0) = config.customOutputWrapper
    val header = withNewLine(header0) + ifIIFE("(function(){") + "'use strict';\n"
    val footer = ifIIFE("}).call(this);\n") + withNewLine(footer0)

    val outputContent =
      if (result.errors.nonEmpty) "// errors while producing source\n"
      else compiler.toSource + "\n"

    val sourceMap = Option(compiler.getSourceMap())

    // Write optimized code
    val w = output.contentWriter
    try {
      w.write(header)
      w.write(outputContent)
      w.write(footer)
      if (sourceMap.isDefined)
        w.write("//# sourceMappingURL=" + output.name + ".map\n")
    } finally w.close()

    // Write source map (if available)
    sourceMap.foreach { sm =>
      sm.setWrapperPrefix(header)
      val w = output.sourceMapWriter
      try sm.appendTo(w, output.name)
      finally w.close()
    }
  }

  private def closureOptions(outputName: String) = {
    val options = new ClosureOptions

//    CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(options)
//    options.setPrettyPrint(config.prettyPrint)
//    options.setCheckTypes(false)
//    options.setInferTypes(false)

    if (Properties.envOrNone("DCE_ONLY").exists(_.toBoolean)) {
      setDCEOnlyOptions(options)
    } else {
      setAdvancedOptions(options)
    }

    options.setLanguageIn(ClosureOptions.LanguageMode.ECMASCRIPT5)
    options.setCheckGlobalThisLevel(CheckLevel.OFF)

    if (withSourceMap) {
      options.setSourceMapOutputPath(outputName + ".map")
      options.setSourceMapDetailLevel(SourceMap.DetailLevel.ALL)
    }

    options
  }

  private def writeAst(compiler: ClosureCompiler, module: JSModule, output: WritableVirtualJSFile) = {
    val source = compiler.toSource(module)
    val w = output.contentWriter
    w.write(source)
    w.close()
  }

  private def setAdvancedOptions(options: ClosureOptions): Unit = {
    CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(options)
    options.setPrettyPrint(config.prettyPrint)
    options.setCheckTypes(false)  // This defaults to true and triggers an NPE in closure.
    options.setInferTypes(false)
  }

  private def setDCEOnlyOptions(options: ClosureOptions): Unit = {
    // Do not call applySafeCompilationOptions(options) because the call can
    // create possible conflicts between multiple diagnostic groups.
    options.setCheckSymbols(false)
    options.setCheckTypes(false)

    // All the safe optimizations.
    options.setDependencyOptions(new DependencyOptions().setDependencySorting(true))
    options.setClosurePass(false)
    options.setFoldConstants(false)
    options.setCoalesceVariableNames(false)
    options.setDeadAssignmentElimination(false)
    options.setExtractPrototypeMemberDeclarations(false)
    options.setCollapseVariableDeclarations(false)
    options.setConvertToDottedProperties(false)
    options.setLabelRenaming(false)
    options.setRemoveDeadCode(true)
    options.setOptimizeArgumentsArray(false)
    options.setCollapseObjectLiterals(false)
    options.setProtectHiddenSideEffects(false)

    // All the advanced optimizations.
    options.setRemoveClosureAsserts(false)
    options.setRemoveAbstractMethods(false)
    //options.setRemoveSuperMethods(false)
    options.setReserveRawExports(false)
    options.setRenamingPolicy(VariableRenamingPolicy.ALL, PropertyRenamingPolicy.ALL_UNQUOTED)
    options.setShadowVariables(false)
    options.setRemoveUnusedPrototypeProperties(false)
    options.setRemoveUnusedPrototypePropertiesInExterns(false)
    options.setRemoveUnusedClassProperties(false)
    options.setCollapseAnonymousFunctions(false)
    options.setCollapseProperties(false)
    options.setCheckGlobalThisLevel(CheckLevel.OFF)
    options.setRewriteFunctionExpressions(false)
    options.setSmartNameRemoval(false)
    options.setExtraSmartNameRemoval(false)
    options.setInlineConstantVars(false)
    options.setInlineFunctions(Reach.LOCAL_ONLY)
    options.setAssumeClosuresOnlyCaptureReferences(false)
    options.setInlineVariables(Reach.LOCAL_ONLY)
    options.setFlowSensitiveInlineVariables(false)
    options.setComputeFunctionSideEffects(false)
    options.setAssumeStrictThis(false)

    // Remove unused vars also removes unused functions.
    options.setRemoveUnusedVariables(Reach.ALL)

    // Move code around based on the defined modules.
    options.setCrossModuleCodeMotion(false)
    options.setCrossModuleMethodMotion(false)

    // Call optimizations
    options.setDevirtualizePrototypeMethods(false)
    options.setOptimizeParameters(false)
    options.setOptimizeReturns(false)
    options.setOptimizeCalls(false)

  }
}

private object ClosureLinkerBackend {
  /** Minimal set of externs to compile Scala.js-emitted code with Closure. */
  private val ScalaJSExterns = """
    /** @constructor */
    function Object() {}
    Object.prototype.toString = function() {};
    Object.prototype.$classData = {};
    /** @constructor */
    function Array() {}
    Array.prototype.length = 0;
    /** @constructor */
    function Function() {}
    Function.prototype.constructor = function() {};
    Function.prototype.call = function() {};
    Function.prototype.apply = function() {};
    function require() {}
    var global = {};
    var exports = {};
    var __ScalaJSEnv = {};
    var NaN = 0.0/0.0, Infinity = 1.0/0.0, undefined = void 0;
    """

  private val ScalaJSExternsFile = new MemVirtualJSFile("ScalaJSExterns.js").
    withContent(ScalaJSExterns)
}
