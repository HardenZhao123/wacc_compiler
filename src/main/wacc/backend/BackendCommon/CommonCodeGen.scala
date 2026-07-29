package wacc.backend.BackendCommon

import scala.collection.mutable

import wacc.backend.midir.TAC.*
import wacc.backend.BackendCommon.AsmEntity.*
import wacc.backend.BackendCommon.BackendConstant.*

abstract class CommonGenerator[
  InstrType >: AsmEntity,
  FrameType,
  StateType,
  GenResType <: CommonGenRes[InstrType]
] {

  protected val ExceptionFlagLabel = "wacc_exception_flag"
  protected val ExceptionValueLabel = "wacc_exception_value"

  protected def newFrame(locals: Seq[Temp]): FrameType

  protected def newCodegenState(frame: FrameType): StateType

  protected def newGenRes(): GenResType

  protected def addFrame(frame: FrameType)(using StateType): List[InstrType]

  protected def emitMainReturn(frame: FrameType)(using StateType, GenResType): Unit

  protected def initParams(frame: FrameType, params: List[Temp])(using StateType, GenResType): Unit

  protected def genInstr(instr: Instr)(using StateType, GenResType): Unit

  protected def exceptionGlobals: Seq[DataItem] =
    Seq(
      WordData(Label(ExceptionFlagLabel), 0),
      WordData(Label(ExceptionValueLabel), 0)
    )

  protected def directivesBeforeText: Seq[AsmEntity] = Nil

  protected def afterMain(frame: FrameType)(using StateType, GenResType): Unit = ()

  protected def afterFunction(frame: FrameType)(using StateType, GenResType): Unit = ()

  def genProgram(program: TACProgram): List[InstrType] = {
    val strs = program.strs.zipWithIndex.map { case (str, id) =>
      val label = Label(s"$STRING_LABEL_PREFIX$id")
      StringData(label, str)
    }

    val header: List[InstrType] =
      List(DataSeg(strs ++ exceptionGlobals), Raw(ASM_DIRECTIVE_ALIGN_4)) ++
        directivesBeforeText ++
        List(Raw(ASM_DIRECTIVE_TEXT), Raw(ASM_DIRECTIVE_GLOBAL_MAIN))

    val mainFrame = newFrame(program.locals)

    given gr: GenResType = newGenRes()
    given cs: StateType = newCodegenState(mainFrame)

    gr.emit(Label(LABEL_MAIN))
    addFrame(mainFrame).foreach(gr.emit)

    program.body.foreach(instr => genInstr(instr))

    emitMainReturn(mainFrame)
    afterMain(mainFrame)

    program.funcs.foreach(f => emitFunctionInto(f))

    val result: mutable.Builder[InstrType, List[InstrType]] = List.newBuilder
    result ++= header
    result ++= gr.instrs
    result ++= gr.preDefsErrs

    result.result()
  }

  protected def emitFunctionInto(f: TACFunction)(using gr: GenResType): Unit = {
    val frame = newFrame(f.locals)
    given cs: StateType = newCodegenState(frame)

    gr.emit(Label(f.name.name))
    addFrame(frame).foreach(gr.emit)

    initParams(frame, f.params)
    f.body.foreach(instr => genInstr(instr))

    afterFunction(frame)
  }
}
