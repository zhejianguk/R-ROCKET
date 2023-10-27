// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import chisel3.withClock
import chisel3.experimental.{chiselName, NoChiselNamePrefix}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property
import freechips.rocketchip.scie._
import scala.collection.mutable.ArrayBuffer

//===== GuardianCouncil Function: Start ====//
import freechips.rocketchip.r._
import freechips.rocketchip.guardiancouncil._
//===== GuardianCouncil Function: End   ====//


case class RocketCoreParams(
  bootFreqHz: BigInt = 0,
  useVM: Boolean = true,
  useUser: Boolean = false,
  useSupervisor: Boolean = false,
  useHypervisor: Boolean = false,
  useDebug: Boolean = true,
  useAtomics: Boolean = true,
  useAtomicsOnlyForIO: Boolean = false,
  useCompressed: Boolean = true,
  useRVE: Boolean = false,
  useSCIE: Boolean = false,
  nLocalInterrupts: Int = 0,
  useNMI: Boolean = false,
  nBreakpoints: Int = 1,
  useBPWatch: Boolean = false,
  mcontextWidth: Int = 0,
  scontextWidth: Int = 0,
  nPMPs: Int = 8,
  nPerfCounters: Int = 0,
  haveBasicCounters: Boolean = true,
  haveCFlush: Boolean = false,
  misaWritable: Boolean = true,
  nL2TLBEntries: Int = 0,
  nL2TLBWays: Int = 1,
  nPTECacheEntries: Int = 8,
  mtvecInit: Option[BigInt] = Some(BigInt(0)),
  mtvecWritable: Boolean = true,
  fastLoadWord: Boolean = true,
  fastLoadByte: Boolean = false,
  branchPredictionModeCSR: Boolean = false,
  clockGate: Boolean = false,
  mvendorid: Int = 0, // 0 means non-commercial implementation
  mimpid: Int = 0x20181004, // release date in BCD
  mulDiv: Option[MulDivParams] = Some(MulDivParams()),
  fpu: Option[FPUParams] = Some(FPUParams())
) extends CoreParams {
  val lgPauseCycles = 5
  val haveFSDirty = false
  val pmpGranularity: Int = if (useHypervisor) 4096 else 4
  val fetchWidth: Int = if (useCompressed) 2 else 1
  //  fetchWidth doubled, but coreInstBytes halved, for RVC:
  val decodeWidth: Int = fetchWidth / (if (useCompressed) 2 else 1)
  val retireWidth: Int = 1
  val instBits: Int = if (useCompressed) 16 else 32
  val lrscCycles: Int = 80 // worst case is 14 mispredicted branches + slop
  override def minFLen: Int = fpu.map(_.minFLen).getOrElse(32)
  override def customCSRs(implicit p: Parameters) = new RocketCustomCSRs
}

trait HasRocketCoreParameters extends HasCoreParameters {
  lazy val rocketParams: RocketCoreParams = tileParams.core.asInstanceOf[RocketCoreParams]

  val fastLoadWord = rocketParams.fastLoadWord
  val fastLoadByte = rocketParams.fastLoadByte

  val mulDivParams = rocketParams.mulDiv.getOrElse(MulDivParams()) // TODO ask andrew about this

  require(!fastLoadByte || fastLoadWord)
}

class RocketCustomCSRs(implicit p: Parameters) extends CustomCSRs with HasRocketCoreParameters {
  override def bpmCSR = {
    rocketParams.branchPredictionModeCSR.option(CustomCSR(bpmCSRId, BigInt(1), Some(BigInt(0))))
  }

  private def haveDCache = tileParams.dcache.get.scratch.isEmpty

  override def chickenCSR = {
    val mask = BigInt(
      tileParams.dcache.get.clockGate.toInt << 0 |
      rocketParams.clockGate.toInt << 1 |
      rocketParams.clockGate.toInt << 2 |
      1 << 3 | // disableSpeculativeICacheRefill
      haveDCache.toInt << 9 | // suppressCorruptOnGrantData
      tileParams.icache.get.prefetch.toInt << 17
    )
    Some(CustomCSR(chickenCSRId, mask, Some(mask)))
  }

  def disableICachePrefetch = getOrElse(chickenCSR, _.value(17), true.B)

  def marchid = CustomCSR.constant(CSRs.marchid, BigInt(1))

  def mvendorid = CustomCSR.constant(CSRs.mvendorid, BigInt(rocketParams.mvendorid))

  // mimpid encodes a release version in the form of a BCD-encoded datestamp.
  def mimpid = CustomCSR.constant(CSRs.mimpid, BigInt(rocketParams.mimpid))

  override def decls = super.decls :+ marchid :+ mvendorid :+ mimpid
}

@chiselName
class Rocket(tile: RocketTile)(implicit p: Parameters) extends CoreModule()(p)
    with HasRocketCoreParameters
    with HasCoreIO {

  val clock_en_reg = RegInit(true.B)
  val long_latency_stall = Reg(Bool())
  val id_reg_pause = Reg(Bool())
  val imem_might_request_reg = Reg(Bool())
  val clock_en = Wire(init=true.B)
  val gated_clock =
    if (!rocketParams.clockGate) clock
    else ClockGate(clock, clock_en, "rocket_clock_gate")

  @chiselName class RocketImpl extends NoChiselNamePrefix { // entering gated-clock domain

  // performance counters
  def pipelineIDToWB[T <: Data](x: T): T =
    RegEnable(RegEnable(RegEnable(x, !ctrl_killd), ex_pc_valid), mem_pc_valid)
  val perfEvents = new EventSets(Seq(
    new EventSet((mask, hits) => Mux(wb_xcpt, mask(0), wb_valid && pipelineIDToWB((mask & hits).orR)), Seq(
      ("exception", () => false.B),
      ("load", () => id_ctrl.mem && id_ctrl.mem_cmd === M_XRD && !id_ctrl.fp),
      ("store", () => id_ctrl.mem && id_ctrl.mem_cmd === M_XWR && !id_ctrl.fp),
      ("amo", () => Bool(usingAtomics) && id_ctrl.mem && (isAMO(id_ctrl.mem_cmd) || id_ctrl.mem_cmd.isOneOf(M_XLR, M_XSC))),
      ("system", () => id_ctrl.csr =/= CSR.N),
      ("arith", () => id_ctrl.wxd && !(id_ctrl.jal || id_ctrl.jalr || id_ctrl.mem || id_ctrl.fp || id_ctrl.mul || id_ctrl.div || id_ctrl.csr =/= CSR.N)),
      ("branch", () => id_ctrl.branch),
      ("jal", () => id_ctrl.jal),
      ("jalr", () => id_ctrl.jalr))
      ++ (if (!usingMulDiv) Seq() else Seq(
        ("mul", () => if (pipelinedMul) id_ctrl.mul else id_ctrl.div && (id_ctrl.alu_fn & ALU.FN_DIV) =/= ALU.FN_DIV),
        ("div", () => if (pipelinedMul) id_ctrl.div else id_ctrl.div && (id_ctrl.alu_fn & ALU.FN_DIV) === ALU.FN_DIV)))
      ++ (if (!usingFPU) Seq() else Seq(
        ("fp load", () => id_ctrl.fp && io.fpu.dec.ldst && io.fpu.dec.wen),
        ("fp store", () => id_ctrl.fp && io.fpu.dec.ldst && !io.fpu.dec.wen),
        ("fp add", () => id_ctrl.fp && io.fpu.dec.fma && io.fpu.dec.swap23),
        ("fp mul", () => id_ctrl.fp && io.fpu.dec.fma && !io.fpu.dec.swap23 && !io.fpu.dec.ren3),
        ("fp mul-add", () => id_ctrl.fp && io.fpu.dec.fma && io.fpu.dec.ren3),
        ("fp div/sqrt", () => id_ctrl.fp && (io.fpu.dec.div || io.fpu.dec.sqrt)),
        ("fp other", () => id_ctrl.fp && !(io.fpu.dec.ldst || io.fpu.dec.fma || io.fpu.dec.div || io.fpu.dec.sqrt))))),
    new EventSet((mask, hits) => (mask & hits).orR, Seq(
      ("load-use interlock", () => id_ex_hazard && ex_ctrl.mem || id_mem_hazard && mem_ctrl.mem || id_wb_hazard && wb_ctrl.mem),
      ("long-latency interlock", () => id_sboard_hazard),
      ("csr interlock", () => id_ex_hazard && ex_ctrl.csr =/= CSR.N || id_mem_hazard && mem_ctrl.csr =/= CSR.N || id_wb_hazard && wb_ctrl.csr =/= CSR.N),
      ("I$ blocked", () => icache_blocked),
      ("D$ blocked", () => id_ctrl.mem && dcache_blocked),
      ("branch misprediction", () => take_pc_mem && mem_direction_misprediction),
      ("control-flow target misprediction", () => take_pc_mem && mem_misprediction && mem_cfi && !mem_direction_misprediction && !icache_blocked),
      ("flush", () => wb_reg_flush_pipe),
      ("replay", () => replay_wb))
      ++ (if (!usingMulDiv) Seq() else Seq(
        ("mul/div interlock", () => id_ex_hazard && (ex_ctrl.mul || ex_ctrl.div) || id_mem_hazard && (mem_ctrl.mul || mem_ctrl.div) || id_wb_hazard && wb_ctrl.div)))
      ++ (if (!usingFPU) Seq() else Seq(
        ("fp interlock", () => id_ex_hazard && ex_ctrl.fp || id_mem_hazard && mem_ctrl.fp || id_wb_hazard && wb_ctrl.fp || id_ctrl.fp && id_stall_fpu)))),
    new EventSet((mask, hits) => (mask & hits).orR, Seq(
      ("I$ miss", () => io.imem.perf.acquire),
      ("D$ miss", () => io.dmem.perf.acquire),
      ("D$ release", () => io.dmem.perf.release),
      ("ITLB miss", () => io.imem.perf.tlbMiss),
      ("DTLB miss", () => io.dmem.perf.tlbMiss),
      ("L2 TLB miss", () => io.ptw.perf.l2miss)))))

  val pipelinedMul = usingMulDiv && mulDivParams.mulUnroll == xLen
  val decode_table = {
    require(!usingRoCC || !rocketParams.useSCIE)
    (if (usingMulDiv) new MDecode(pipelinedMul) +: (xLen > 32).option(new M64Decode(pipelinedMul)).toSeq else Nil) ++:
    (if (usingAtomics) new ADecode +: (xLen > 32).option(new A64Decode).toSeq else Nil) ++:
    (if (fLen >= 32)    new FDecode +: (xLen > 32).option(new F64Decode).toSeq else Nil) ++:
    (if (fLen >= 64)    new DDecode +: (xLen > 32).option(new D64Decode).toSeq else Nil) ++:
    (if (minFLen == 16) new HDecode +: (xLen > 32).option(new H64Decode).toSeq ++: (fLen >= 64).option(new HDDecode).toSeq else Nil) ++:
    (usingRoCC.option(new RoCCDecode)) ++:
    (rocketParams.useSCIE.option(new SCIEDecode)) ++:
    (if (xLen == 32) new I32Decode else new I64Decode) +:
    (usingVM.option(new SVMDecode)) ++:
    (usingSupervisor.option(new SDecode)) ++:
    (usingHypervisor.option(new HypervisorDecode)) ++:
    ((usingHypervisor && (xLen == 64)).option(new Hypervisor64Decode)) ++:
    (usingDebug.option(new DebugDecode)) ++:
    (usingNMI.option(new NMIDecode)) ++:
    Seq(new FenceIDecode(tile.dcache.flushOnFenceI)) ++:
    coreParams.haveCFlush.option(new CFlushDecode(tile.dcache.canSupportCFlushLine)) ++:
    Seq(new IDecode)
  } flatMap(_.table)

  val ex_ctrl = Reg(new IntCtrlSigs)
  val mem_ctrl = Reg(new IntCtrlSigs)
  val wb_ctrl = Reg(new IntCtrlSigs)

  val ex_reg_xcpt_interrupt  = Reg(Bool())
  val ex_reg_valid           = Reg(Bool())
  val ex_reg_rvc             = Reg(Bool())
  val ex_reg_btb_resp        = Reg(new BTBResp)
  val ex_reg_xcpt            = Reg(Bool())
  val ex_reg_flush_pipe      = Reg(Bool())
  val ex_reg_load_use        = Reg(Bool())
  val ex_reg_cause           = Reg(UInt())
  val ex_reg_replay = Reg(Bool())
  val ex_reg_pc = Reg(UInt())
  val ex_reg_mem_size = Reg(UInt())
  val ex_reg_hls = Reg(Bool())
  val ex_reg_inst = Reg(Bits())
  val ex_reg_raw_inst = Reg(UInt())
  val ex_scie_unpipelined = Reg(Bool())
  val ex_scie_pipelined = Reg(Bool())
  val ex_reg_wphit            = Reg(Vec(nBreakpoints, Bool()))

  val mem_reg_xcpt_interrupt  = Reg(Bool())
  val mem_reg_valid           = Reg(Bool())
  val mem_reg_rvc             = Reg(Bool())
  val mem_reg_btb_resp        = Reg(new BTBResp)
  val mem_reg_xcpt            = Reg(Bool())
  val mem_reg_replay          = Reg(Bool())
  val mem_reg_flush_pipe      = Reg(Bool())
  val mem_reg_cause           = Reg(UInt())
  val mem_reg_slow_bypass     = Reg(Bool())
  val mem_reg_load            = Reg(Bool())
  val mem_reg_store           = Reg(Bool())
  val mem_reg_sfence = Reg(Bool())
  val mem_reg_pc = Reg(UInt())
  val mem_reg_inst = Reg(Bits())
  val mem_reg_mem_size = Reg(UInt())
  val mem_reg_hls_or_dv = Reg(Bool())
  val mem_reg_raw_inst = Reg(UInt())
  val mem_scie_unpipelined = Reg(Bool())
  val mem_scie_pipelined = Reg(Bool())
  val mem_reg_wdata = Reg(Bits())
  val mem_reg_rs2 = Reg(Bits())
  val mem_br_taken = Reg(Bool())
  val take_pc_mem = Wire(Bool())
  val mem_reg_wphit          = Reg(Vec(nBreakpoints, Bool()))

  val wb_reg_valid           = Reg(Bool())
  val wb_reg_xcpt            = Reg(Bool())
  val wb_reg_replay          = Reg(Bool())
  val wb_reg_flush_pipe      = Reg(Bool())
  val wb_reg_cause           = Reg(UInt())
  val wb_reg_sfence = Reg(Bool())
  val wb_reg_pc = Reg(UInt())
  val wb_reg_mem_size = Reg(UInt())
  val wb_reg_hls_or_dv = Reg(Bool())
  val wb_reg_hfence_v = Reg(Bool())
  val wb_reg_hfence_g = Reg(Bool())
  val wb_reg_inst = Reg(Bits())
  val wb_reg_raw_inst = Reg(UInt())
  val wb_reg_wdata = Reg(Bits())
  val wb_reg_rs2 = Reg(Bits())
  val take_pc_wb = Wire(Bool())
  val wb_reg_wphit = Reg(Vec(nBreakpoints, Bool()))

  val take_pc_mem_wb = take_pc_wb || take_pc_mem
  val take_pc = take_pc_mem_wb

  // decode stage
  val ibuf = Module(new IBuf)
  val id_expanded_inst = ibuf.io.inst.map(_.bits.inst)
  val id_raw_inst = ibuf.io.inst.map(_.bits.raw)
  val id_inst = id_expanded_inst.map(_.bits)
  ibuf.io.imem <> io.imem.resp
  ibuf.io.kill := take_pc

  require(decodeWidth == 1 /* TODO */ && retireWidth == decodeWidth)
  require(!(coreParams.useRVE && coreParams.fpu.nonEmpty), "Can't select both RVE and floating-point")
  val id_ctrl = Wire(new IntCtrlSigs()).decode(id_inst(0), decode_table)
  val lgNXRegs = if (coreParams.useRVE) 4 else 5
  val regAddrMask = (1 << lgNXRegs) - 1

  def decodeReg(x: UInt) = (x.extract(x.getWidth-1, lgNXRegs).asBool, x(lgNXRegs-1, 0))
  val (id_raddr3_illegal, id_raddr3) = decodeReg(id_expanded_inst(0).rs3)
  val (id_raddr2_illegal, id_raddr2) = decodeReg(id_expanded_inst(0).rs2)
  val (id_raddr1_illegal, id_raddr1) = decodeReg(id_expanded_inst(0).rs1)
  val (id_waddr_illegal,  id_waddr)  = decodeReg(id_expanded_inst(0).rd)

  val id_load_use = Wire(Bool())
  val id_reg_fence = Reg(init=Bool(false))
  val id_ren = IndexedSeq(id_ctrl.rxs1, id_ctrl.rxs2)
  val id_raddr = IndexedSeq(id_raddr1, id_raddr2)
  val rf = new RegFile(regAddrMask, xLen)
  val id_rs = id_raddr.map(rf.read _)
  val ctrl_killd = Wire(Bool())
  val id_npc = (ibuf.io.pc.asSInt + ImmGen(IMM_UJ, id_inst(0))).asUInt

  val csr = Module(new CSRFile(perfEvents, coreParams.customCSRs.decls))
  val id_csr_en = id_ctrl.csr.isOneOf(CSR.S, CSR.C, CSR.W)
  val id_system_insn = id_ctrl.csr === CSR.I
  val id_csr_ren = id_ctrl.csr.isOneOf(CSR.S, CSR.C) && id_expanded_inst(0).rs1 === UInt(0)
  val id_csr = Mux(id_system_insn && id_ctrl.mem, CSR.N, Mux(id_csr_ren, CSR.R, id_ctrl.csr))
  val id_csr_flush = id_system_insn || (id_csr_en && !id_csr_ren && csr.io.decode(0).write_flush)

  //===== GuardianCouncil Function: Start ====//
  io.pc := wb_reg_pc
  io.inst := wb_reg_inst
  io.new_commit := csr.io.trace(0).valid && !csr.io.trace(0).exception
  io.csr_rw_wdata := csr.io.rw.wdata

  /* R Features */
  val rsu_pc = Reg(UInt(width=40))
  val checker_mode = Wire(UInt(width=1))
  ibuf.io.checker_mode := checker_mode

  val lsl_req_ready     = Wire(UInt(width=1))
  val lsl_req_valid     = Wire(UInt(width=1))
  val lsl_req_addr      = Wire(UInt(width=40))
  val lsl_req_tag       = Wire(UInt(width=8))
  val lsl_req_cmd       = Wire(UInt(width=2))
  val lsl_req_data      = Wire(UInt(width=xLen))

  val lsl_resp_valid    = Wire(UInt(width=1))
  val lsl_resp_tag      = Wire(UInt(width=8))
  val lsl_resp_size     = Wire(UInt(width=2))
  val lsl_resp_addr     = Wire(UInt(width=40))
  val lsl_resp_data     = Wire(UInt(width=xLen))
  val lsl_resp_has_data = Wire(UInt(width=1))
  val lsl_resp_replay   = Wire(UInt(width=1))
  val lsl_req_size      = Wire(UInt(width=2))
  val lsl_req_kill      = Wire(UInt(width=1))
  
  val lsl_req_valid_csr = Wire(UInt(width=1))
  val lsl_resp_data_csr = Wire(UInt(width=xLen))
  val lsl_resp_replay_csr = Wire(UInt(width=1))
  val lsl_req_ready_csr = Wire(UInt(width=1))

  val icsl_if_overtaking = Wire(UInt(width=1))
  val icsl_just_overtaking = Wire(UInt(width=1))
  val icsl_if_ret_special_pc = Wire(UInt(width=1))
  //===== GuardianCouncil Function: End   ====//

  val id_scie_decoder = if (!rocketParams.useSCIE) Wire(new SCIEDecoderInterface) else {
    val d = Module(new SCIEDecoder)
    assert(!io.imem.resp.valid || PopCount(d.io.unpipelined :: d.io.pipelined :: d.io.multicycle :: Nil) <= 1)
    d.io.insn := id_raw_inst(0)
    d.io
  }
  val id_illegal_insn = !id_ctrl.legal ||
    (id_ctrl.mul || id_ctrl.div) && !csr.io.status.isa('m'-'a') ||
    id_ctrl.amo && !csr.io.status.isa('a'-'a') ||
    id_ctrl.fp && (csr.io.decode(0).fp_illegal || io.fpu.illegal_rm) ||
    id_ctrl.dp && !csr.io.status.isa('d'-'a') ||
    ibuf.io.inst(0).bits.rvc && !csr.io.status.isa('c'-'a') ||
    id_raddr2_illegal && !id_ctrl.scie && id_ctrl.rxs2 ||
    id_raddr1_illegal && !id_ctrl.scie && id_ctrl.rxs1 ||
    id_waddr_illegal && !id_ctrl.scie && id_ctrl.wxd ||
    id_ctrl.rocc && csr.io.decode(0).rocc_illegal ||
    id_ctrl.scie && !(id_scie_decoder.unpipelined || id_scie_decoder.pipelined) ||
    id_csr_en && (csr.io.decode(0).read_illegal || !id_csr_ren && csr.io.decode(0).write_illegal) ||
    !ibuf.io.inst(0).bits.rvc && (id_system_insn && csr.io.decode(0).system_illegal)
  val id_virtual_insn = id_ctrl.legal &&
    ((id_csr_en && !(!id_csr_ren && csr.io.decode(0).write_illegal) && csr.io.decode(0).virtual_access_illegal) ||
     (!ibuf.io.inst(0).bits.rvc && id_system_insn && csr.io.decode(0).virtual_system_illegal))
  // stall decode for fences (now, for AMO.rl; later, for AMO.aq and FENCE)
  val id_amo_aq = id_inst(0)(26)
  val id_amo_rl = id_inst(0)(25)
  val id_fence_pred = id_inst(0)(27,24)
  val id_fence_succ = id_inst(0)(23,20)
  val id_fence_next = id_ctrl.fence || id_ctrl.amo && id_amo_aq
  /* R Feature --- LSL */
  val id_mem_busy = Mux(checker_mode === 1.U, lsl_req_valid.asBool, (!io.dmem.ordered || io.dmem.req.valid))

  when (!id_mem_busy) { id_reg_fence := false }
  val id_rocc_busy = Bool(usingRoCC) &&
    (io.rocc.busy || ex_reg_valid && ex_ctrl.rocc ||
     mem_reg_valid && mem_ctrl.rocc || wb_reg_valid && wb_ctrl.rocc)
  val id_do_fence = Wire(init = id_rocc_busy && id_ctrl.fence ||
    id_mem_busy && (id_ctrl.amo && id_amo_rl || id_ctrl.fence_i || id_reg_fence && (id_ctrl.mem || id_ctrl.rocc)))

  val bpu = Module(new BreakpointUnit(nBreakpoints))
  bpu.io.status := csr.io.status
  bpu.io.bp := csr.io.bp
  bpu.io.pc := ibuf.io.pc
  bpu.io.ea := mem_reg_wdata
  bpu.io.mcontext := csr.io.mcontext
  bpu.io.scontext := csr.io.scontext

  val id_xcpt0 = ibuf.io.inst(0).bits.xcpt0
  val id_xcpt1 = ibuf.io.inst(0).bits.xcpt1
  val (id_xcpt, id_cause) = checkExceptions(List(
    (csr.io.interrupt, csr.io.interrupt_cause),
    (bpu.io.debug_if,  UInt(CSR.debugTriggerCause)),
    (bpu.io.xcpt_if,   UInt(Causes.breakpoint)),
    (id_xcpt0.pf.inst, UInt(Causes.fetch_page_fault)),
    (id_xcpt0.gf.inst, UInt(Causes.fetch_guest_page_fault)),
    (id_xcpt0.ae.inst, UInt(Causes.fetch_access)),
    (id_xcpt1.pf.inst, UInt(Causes.fetch_page_fault)),
    (id_xcpt1.gf.inst, UInt(Causes.fetch_guest_page_fault)),
    (id_xcpt1.ae.inst, UInt(Causes.fetch_access)),
    (id_virtual_insn,  UInt(Causes.virtual_instruction)),
    (id_illegal_insn,  UInt(Causes.illegal_instruction))))

  val idCoverCauses = List(
    (CSR.debugTriggerCause, "DEBUG_TRIGGER"),
    (Causes.breakpoint, "BREAKPOINT"),
    (Causes.fetch_access, "FETCH_ACCESS"),
    (Causes.illegal_instruction, "ILLEGAL_INSTRUCTION")
  ) ++ (if (usingVM) List(
    (Causes.fetch_page_fault, "FETCH_PAGE_FAULT")
  ) else Nil)
  coverExceptions(id_xcpt, id_cause, "DECODE", idCoverCauses)

  //===== GuardianCouncil Function: Start ====//
  // Enabling data bypass for RoCC commands
  val dcache_bypass_data =
    if (fastLoadByte) Mux(wb_ctrl.rocc, io.rocc.resp.bits.data, Mux((checker_mode === 1.U), lsl_resp_data, io.dmem.resp.bits.data(xLen-1, 0)))
    else if (fastLoadWord) Mux(wb_ctrl.rocc, io.rocc.resp.bits.data, Mux((checker_mode === 1.U), lsl_resp_data, io.dmem.resp.bits.data_word_bypass(xLen-1, 0)))
    else wb_reg_wdata
  //===== GuardianCouncil Function: End ====//

  // detect bypass opportunities
  val ex_waddr = ex_reg_inst(11,7) & regAddrMask
  val mem_waddr = mem_reg_inst(11,7) & regAddrMask
  val wb_waddr = wb_reg_inst(11,7) & regAddrMask
  val bypass_sources = IndexedSeq(
    (Bool(true), UInt(0), UInt(0)), // treat reading x0 as a bypass
    (ex_reg_valid && ex_ctrl.wxd, ex_waddr, mem_reg_wdata),
    (mem_reg_valid && mem_ctrl.wxd && (!mem_ctrl.mem && !mem_ctrl.rocc), mem_waddr, wb_reg_wdata), // avoding RoCC path
    (mem_reg_valid && mem_ctrl.wxd, mem_waddr, dcache_bypass_data))
  val id_bypass_src = id_raddr.map(raddr => bypass_sources.map(s => s._1 && s._2 === raddr))

  // execute stage
  val bypass_mux = bypass_sources.map(_._3)
  val ex_reg_rs_bypass = Reg(Vec(id_raddr.size, Bool()))
  val ex_reg_rs_lsb = Reg(Vec(id_raddr.size, UInt(width = log2Ceil(bypass_sources.size))))
  val ex_reg_rs_msb = Reg(Vec(id_raddr.size, UInt()))
  val ex_rs = for (i <- 0 until id_raddr.size)
    yield Mux(ex_reg_rs_bypass(i), bypass_mux(ex_reg_rs_lsb(i)), Cat(ex_reg_rs_msb(i), ex_reg_rs_lsb(i)))
  val ex_imm = ImmGen(ex_ctrl.sel_imm, ex_reg_inst)
  val ex_op1 = MuxLookup(ex_ctrl.sel_alu1, SInt(0), Seq(
    A1_RS1 -> ex_rs(0).asSInt,
    A1_PC -> ex_reg_pc.asSInt))
  val ex_op2 = MuxLookup(ex_ctrl.sel_alu2, SInt(0), Seq(
    A2_RS2 -> ex_rs(1).asSInt,
    A2_IMM -> ex_imm,
    A2_SIZE -> Mux(ex_reg_rvc, SInt(2), SInt(4))))

  val alu = Module(new ALU)
  alu.io.dw := ex_ctrl.alu_dw
  alu.io.fn := ex_ctrl.alu_fn
  alu.io.in2 := ex_op2.asUInt
  alu.io.in1 := ex_op1.asUInt

  //===== GuardianCouncil Function: Start ====//
  val alu_1cycle_delay_reg = Reg(UInt())
  val alu_2cycle_delay_reg = Reg(UInt())
  alu_1cycle_delay_reg := alu.io.out
  alu_2cycle_delay_reg := alu_1cycle_delay_reg
  io.alu_2cycle_delay := alu_2cycle_delay_reg
  val record_pc = Reg(UInt())
  val pc_special = Reg(UInt())
  record_pc := io.record_pc
  pc_special := Mux(record_pc === 1.U, wb_reg_pc + 4.U, pc_special)
  //===== GuardianCouncil Function: Start ====//

  val ex_scie_unpipelined_wdata = if (!rocketParams.useSCIE) 0.U else {
    val u = Module(new SCIEUnpipelined(xLen))
    u.io.insn := ex_reg_inst
    u.io.rs1 := ex_rs(0)
    u.io.rs2 := ex_rs(1)
    u.io.rd
  }
  val mem_scie_pipelined_wdata = if (!rocketParams.useSCIE) 0.U else {
    val u = Module(new SCIEPipelined(xLen))
    u.io.clock := Module.clock
    u.io.valid := ex_reg_valid && ex_scie_pipelined
    u.io.insn := ex_reg_inst
    u.io.rs1 := ex_rs(0)
    u.io.rs2 := ex_rs(1)
    u.io.rd
  }

  // multiplier and divider
  val div = Module(new MulDiv(if (pipelinedMul) mulDivParams.copy(mulUnroll = 0) else mulDivParams, width = xLen))
  div.io.req.valid := ex_reg_valid && ex_ctrl.div
  div.io.req.bits.dw := ex_ctrl.alu_dw
  div.io.req.bits.fn := ex_ctrl.alu_fn
  div.io.req.bits.in1 := ex_rs(0)
  div.io.req.bits.in2 := ex_rs(1)
  div.io.req.bits.tag := ex_waddr
  val mul = pipelinedMul.option {
    val m = Module(new PipelinedMultiplier(xLen, 2))
    m.io.req.valid := ex_reg_valid && ex_ctrl.mul
    m.io.req.bits := div.io.req.bits
    m
  }

  ex_reg_valid := !ctrl_killd
  ex_reg_replay := !take_pc && ibuf.io.inst(0).valid && ibuf.io.inst(0).bits.replay
  ex_reg_xcpt := !ctrl_killd && id_xcpt
  ex_reg_xcpt_interrupt := !take_pc && ibuf.io.inst(0).valid && csr.io.interrupt

  when (!ctrl_killd) {
    ex_ctrl := id_ctrl
    ex_reg_rvc := ibuf.io.inst(0).bits.rvc
    ex_ctrl.csr := id_csr
    ex_scie_unpipelined := id_ctrl.scie && id_scie_decoder.unpipelined
    ex_scie_pipelined := id_ctrl.scie && id_scie_decoder.pipelined
    when (id_ctrl.fence && id_fence_succ === 0) { id_reg_pause := true }
    when (id_fence_next) { id_reg_fence := true }
    when (id_xcpt) { // pass PC down ALU writeback pipeline for badaddr
      ex_ctrl.alu_fn := ALU.FN_ADD
      ex_ctrl.alu_dw := DW_XPR
      ex_ctrl.sel_alu1 := A1_RS1 // badaddr := instruction
      ex_ctrl.sel_alu2 := A2_ZERO
      when (id_xcpt1.asUInt.orR) { // badaddr := PC+2
        ex_ctrl.sel_alu1 := A1_PC
        ex_ctrl.sel_alu2 := A2_SIZE
        ex_reg_rvc := true
      }
      when (bpu.io.xcpt_if || id_xcpt0.asUInt.orR) { // badaddr := PC
        ex_ctrl.sel_alu1 := A1_PC
        ex_ctrl.sel_alu2 := A2_ZERO
      }
    }
    ex_reg_flush_pipe := id_ctrl.fence_i || id_csr_flush
    ex_reg_load_use := id_load_use
    ex_reg_hls := usingHypervisor && id_system_insn && id_ctrl.mem_cmd.isOneOf(M_XRD, M_XWR, M_HLVX)
    ex_reg_mem_size := Mux(usingHypervisor && id_system_insn, id_inst(0)(27, 26), id_inst(0)(13, 12))
    when (id_ctrl.mem_cmd.isOneOf(M_SFENCE, M_HFENCEV, M_HFENCEG, M_FLUSH_ALL)) {
      ex_reg_mem_size := Cat(id_raddr2 =/= UInt(0), id_raddr1 =/= UInt(0))
    }
    when (id_ctrl.mem_cmd === M_SFENCE && csr.io.status.v) {
      ex_ctrl.mem_cmd := M_HFENCEV
    }
    if (tile.dcache.flushOnFenceI) {
      when (id_ctrl.fence_i) {
        ex_reg_mem_size := 0
      }
    }

    for (i <- 0 until id_raddr.size) {
      val do_bypass = id_bypass_src(i).reduce(_||_)
      val bypass_src = PriorityEncoder(id_bypass_src(i))
      ex_reg_rs_bypass(i) := do_bypass
      ex_reg_rs_lsb(i) := bypass_src
      when (id_ren(i) && !do_bypass) {
        ex_reg_rs_lsb(i) := id_rs(i)(log2Ceil(bypass_sources.size)-1, 0)
        ex_reg_rs_msb(i) := id_rs(i) >> log2Ceil(bypass_sources.size)
      }
    }
    when (id_illegal_insn || id_virtual_insn) {
      val inst = Mux(ibuf.io.inst(0).bits.rvc, id_raw_inst(0)(15, 0), id_raw_inst(0))
      ex_reg_rs_bypass(0) := false
      ex_reg_rs_lsb(0) := inst(log2Ceil(bypass_sources.size)-1, 0)
      ex_reg_rs_msb(0) := inst >> log2Ceil(bypass_sources.size)
    }
  }
  when (!ctrl_killd || csr.io.interrupt || ibuf.io.inst(0).bits.replay) {
    ex_reg_cause := id_cause
    ex_reg_inst := id_inst(0)
    ex_reg_raw_inst := id_raw_inst(0)
    ex_reg_pc := ibuf.io.pc
    ex_reg_btb_resp := ibuf.io.btb_resp
    ex_reg_wphit := bpu.io.bpwatch.map { bpw => bpw.ivalid(0) }
  }

  // replay inst in ex stage?
  val ex_pc_valid = ex_reg_valid || ex_reg_replay || ex_reg_xcpt_interrupt
  val wb_dcache_miss = Mux((checker_mode === 1.U), false.B, wb_ctrl.mem && !io.dmem.resp.valid)

  /* R Feature --- LSL */
  // val replay_ex_structural = ex_ctrl.mem && !io.dmem.req.ready ||
  //                            ex_ctrl.div && !div.io.req.ready
  
  // In checker mode, the LSL should be always ready, 
  // The non-ready directly hangs the pipeline
  val replay_ex_structural = Mux(checker_mode === 1.U, (ex_ctrl.div && !div.io.req.ready), ((ex_ctrl.mem && !io.dmem.req.ready) || (ex_ctrl.div && !div.io.req.ready)))

  val replay_ex_load_use = wb_dcache_miss && ex_reg_load_use
  val replay_ex = ex_reg_replay || (ex_reg_valid && (replay_ex_structural || replay_ex_load_use))
  val ctrl_killx = take_pc_mem_wb || replay_ex || !ex_reg_valid
  // detect 2-cycle load-use delay for LB/LH/SC
  val ex_slow_bypass = ex_ctrl.mem_cmd === M_XSC || ex_reg_mem_size < 2
  val ex_sfence = Bool(usingVM) && ex_ctrl.mem && (ex_ctrl.mem_cmd === M_SFENCE || ex_ctrl.mem_cmd === M_HFENCEV || ex_ctrl.mem_cmd === M_HFENCEG)

  val (ex_xcpt, ex_cause) = checkExceptions(List(
    (ex_reg_xcpt_interrupt || ex_reg_xcpt, ex_reg_cause)))

  val exCoverCauses = idCoverCauses
  coverExceptions(ex_xcpt, ex_cause, "EXECUTE", exCoverCauses)

  // memory stage
  val mem_pc_valid = mem_reg_valid || mem_reg_replay || mem_reg_xcpt_interrupt
  val mem_br_target = mem_reg_pc.asSInt +
    Mux(mem_ctrl.branch && mem_br_taken, ImmGen(IMM_SB, mem_reg_inst),
    Mux(mem_ctrl.jal, ImmGen(IMM_UJ, mem_reg_inst),
    Mux(mem_reg_rvc, SInt(2), SInt(4))))
  val mem_npc = (Mux(mem_ctrl.jalr || mem_reg_sfence, encodeVirtualAddress(mem_reg_wdata, mem_reg_wdata).asSInt, mem_br_target) & SInt(-2)).asUInt
  val mem_wrong_npc =
    Mux(ex_pc_valid, mem_npc =/= ex_reg_pc,
    Mux(ibuf.io.inst(0).valid || ibuf.io.imem.valid, mem_npc =/= ibuf.io.pc, Bool(true)))
  val mem_npc_misaligned = !csr.io.status.isa('c'-'a') && mem_npc(1) && !mem_reg_sfence
  val mem_int_wdata = Mux(!mem_reg_xcpt && (mem_ctrl.jalr ^ mem_npc_misaligned), mem_br_target, mem_reg_wdata.asSInt).asUInt
  val mem_cfi = mem_ctrl.branch || mem_ctrl.jalr || mem_ctrl.jal
  val mem_cfi_taken = (mem_ctrl.branch && mem_br_taken) || mem_ctrl.jalr || mem_ctrl.jal
  val mem_direction_misprediction = mem_ctrl.branch && mem_br_taken =/= (usingBTB && mem_reg_btb_resp.taken)
  val mem_misprediction = if (usingBTB) mem_wrong_npc else mem_cfi_taken
  take_pc_mem := mem_reg_valid && !mem_reg_xcpt && (mem_misprediction || mem_reg_sfence)

  mem_reg_valid := !ctrl_killx
  mem_reg_replay := !take_pc_mem_wb && replay_ex
  mem_reg_xcpt := !ctrl_killx && ex_xcpt
  mem_reg_xcpt_interrupt := !take_pc_mem_wb && ex_reg_xcpt_interrupt

  // on pipeline flushes, cause mem_npc to hold the sequential npc, which
  // will drive the W-stage npc mux
  when (mem_reg_valid && mem_reg_flush_pipe) {
    mem_reg_sfence := false
  }.elsewhen (ex_pc_valid) {
    mem_ctrl := ex_ctrl
    mem_scie_unpipelined := ex_scie_unpipelined
    mem_scie_pipelined := ex_scie_pipelined
    mem_reg_rvc := ex_reg_rvc
    mem_reg_load := ex_ctrl.mem && isRead(ex_ctrl.mem_cmd)
    mem_reg_store := ex_ctrl.mem && isWrite(ex_ctrl.mem_cmd)
    mem_reg_sfence := ex_sfence
    mem_reg_btb_resp := ex_reg_btb_resp
    mem_reg_flush_pipe := ex_reg_flush_pipe
    mem_reg_slow_bypass := ex_slow_bypass
    mem_reg_wphit := ex_reg_wphit

    mem_reg_cause := ex_cause
    mem_reg_inst := ex_reg_inst
    mem_reg_raw_inst := ex_reg_raw_inst
    mem_reg_mem_size := ex_reg_mem_size
    mem_reg_hls_or_dv := Mux(checker_mode === 1.U, 0.U, io.dmem.req.bits.dv)
    mem_reg_pc := ex_reg_pc
    // mem_reg_wdata := Mux(ex_scie_unpipelined, ex_scie_unpipelined_wdata, alu.io.out)
    mem_reg_wdata := Mux((ex_ctrl.jalr && ex_reg_inst(12) === 1.U), rsu_pc, Mux(ex_scie_unpipelined, ex_scie_unpipelined_wdata, alu.io.out))
    mem_br_taken := alu.io.cmp_out

    when (ex_ctrl.rxs2 && (ex_ctrl.mem || ex_ctrl.rocc || ex_sfence)) {
      val size = Mux(ex_ctrl.rocc, log2Ceil(xLen/8).U, ex_reg_mem_size)
      mem_reg_rs2 := new StoreGen(size, 0.U, ex_rs(1), coreDataBytes).data
    }
    when (ex_ctrl.jalr && csr.io.status.debug) {
      // flush I$ on D-mode JALR to effect uncached fetch without D$ flush
      mem_ctrl.fence_i := true
      mem_reg_flush_pipe := true
    }
  }

  val mem_breakpoint = (mem_reg_load && bpu.io.xcpt_ld) || (mem_reg_store && bpu.io.xcpt_st)
  val mem_debug_breakpoint = (mem_reg_load && bpu.io.debug_ld) || (mem_reg_store && bpu.io.debug_st)
  val (mem_ldst_xcpt, mem_ldst_cause) = checkExceptions(List(
    (mem_debug_breakpoint, UInt(CSR.debugTriggerCause)),
    (mem_breakpoint,       UInt(Causes.breakpoint))))

  val (mem_xcpt, mem_cause) = checkExceptions(List(
    (mem_reg_xcpt_interrupt || mem_reg_xcpt, mem_reg_cause),
    (mem_reg_valid && mem_npc_misaligned,    UInt(Causes.misaligned_fetch)),
    (mem_reg_valid && mem_ldst_xcpt,         mem_ldst_cause)))

  val memCoverCauses = (exCoverCauses ++ List(
    (CSR.debugTriggerCause, "DEBUG_TRIGGER"),
    (Causes.breakpoint, "BREAKPOINT"),
    (Causes.misaligned_fetch, "MISALIGNED_FETCH")
  )).distinct
  coverExceptions(mem_xcpt, mem_cause, "MEMORY", memCoverCauses)

  val dcache_kill_mem = Mux(checker_mode === 1.U, false.B, (mem_reg_valid && mem_ctrl.wxd && io.dmem.replay_next)) // structural hazard on writeback port
  val fpu_kill_mem = mem_reg_valid && mem_ctrl.fp && io.fpu.nack_mem
  val replay_mem  = dcache_kill_mem || mem_reg_replay || fpu_kill_mem
  val killm_common = dcache_kill_mem || take_pc_wb || mem_reg_xcpt || !mem_reg_valid
  val ctrl_killm = killm_common || mem_xcpt || fpu_kill_mem

  val if_kill_div_r = Mux(checker_mode === 0.U, false.B, Mux(!ctrl_killm && mem_ctrl.div && (icsl_if_overtaking.asBool || icsl_just_overtaking.asBool), true.B, false.B))
  div.io.kill := ((killm_common && Reg(next = div.io.req.fire())) || if_kill_div_r)


  // writeback stage
  wb_reg_valid := !ctrl_killm
  wb_reg_replay := replay_mem && !take_pc_wb
  wb_reg_xcpt := mem_xcpt && !take_pc_wb
  wb_reg_flush_pipe := !ctrl_killm && mem_reg_flush_pipe
  when (mem_pc_valid) {
    wb_ctrl := mem_ctrl
    wb_reg_sfence := mem_reg_sfence
    wb_reg_wdata := Mux(mem_scie_pipelined, mem_scie_pipelined_wdata,
      Mux(!mem_reg_xcpt && mem_ctrl.fp && mem_ctrl.wxd, io.fpu.toint_data, mem_int_wdata))
    when (mem_ctrl.rocc || mem_reg_sfence) {
      wb_reg_rs2 := mem_reg_rs2
    }
    wb_reg_cause := mem_cause
    wb_reg_inst := mem_reg_inst
    wb_reg_raw_inst := mem_reg_raw_inst
    wb_reg_mem_size := mem_reg_mem_size
    wb_reg_hls_or_dv := mem_reg_hls_or_dv
    wb_reg_hfence_v := mem_ctrl.mem_cmd === M_HFENCEV
    wb_reg_hfence_g := mem_ctrl.mem_cmd === M_HFENCEG
    wb_reg_pc := mem_reg_pc
    wb_reg_wphit := mem_reg_wphit | bpu.io.bpwatch.map { bpw => (bpw.rvalid(0) && mem_reg_load) || (bpw.wvalid(0) && mem_reg_store) }

  }

  val (wb_xcpt, wb_cause) = checkExceptions(List(
    (wb_reg_xcpt,  wb_reg_cause),
    (wb_reg_valid && wb_ctrl.mem && (Mux(checker_mode === 1.U, false.B, io.dmem.s2_xcpt.pf.st)), UInt(Causes.store_page_fault)),
    (wb_reg_valid && wb_ctrl.mem && (Mux(checker_mode === 1.U, false.B, io.dmem.s2_xcpt.pf.ld)), UInt(Causes.load_page_fault)),
    (wb_reg_valid && wb_ctrl.mem && (Mux(checker_mode === 1.U, false.B, io.dmem.s2_xcpt.gf.st)), UInt(Causes.store_guest_page_fault)),
    (wb_reg_valid && wb_ctrl.mem && (Mux(checker_mode === 1.U, false.B, io.dmem.s2_xcpt.gf.ld)), UInt(Causes.load_guest_page_fault)),
    (wb_reg_valid && wb_ctrl.mem && (Mux(checker_mode === 1.U, false.B, io.dmem.s2_xcpt.ae.st)), UInt(Causes.store_access)),
    (wb_reg_valid && wb_ctrl.mem && (Mux(checker_mode === 1.U, false.B, io.dmem.s2_xcpt.ae.ld)), UInt(Causes.load_access)),
    (wb_reg_valid && wb_ctrl.mem && (Mux(checker_mode === 1.U, false.B, io.dmem.s2_xcpt.ma.st)), UInt(Causes.misaligned_store)),
    (wb_reg_valid && wb_ctrl.mem && (Mux(checker_mode === 1.U, false.B, io.dmem.s2_xcpt.ma.ld)), UInt(Causes.misaligned_load))
  ))

  val wbCoverCauses = List(
    (Causes.misaligned_store, "MISALIGNED_STORE"),
    (Causes.misaligned_load, "MISALIGNED_LOAD"),
    (Causes.store_access, "STORE_ACCESS"),
    (Causes.load_access, "LOAD_ACCESS")
  ) ++ (if(usingVM) List(
    (Causes.store_page_fault, "STORE_PAGE_FAULT"),
    (Causes.load_page_fault, "LOAD_PAGE_FAULT")
  ) else Nil) ++ (if (usingHypervisor) List(
    (Causes.store_guest_page_fault, "STORE_GUEST_PAGE_FAULT"),
    (Causes.load_guest_page_fault, "LOAD_GUEST_PAGE_FAULT"),
  ) else Nil)
  coverExceptions(wb_xcpt, wb_cause, "WRITEBACK", wbCoverCauses)

  val wb_pc_valid = wb_reg_valid || wb_reg_replay || wb_reg_xcpt
  //===== GuardianCouncil Function: Start ====//
  // Original design:
  // val wb_wxd = wb_reg_valid && wb_ctrl.wxd
  // val replay_wb_rocc = wb_reg_valid && wb_ctrl.rocc && !io.rocc.cmd.ready
  // In GuardianCouncil, RoCC response can be replied in a single cycle, therefore !io.rocc.resp.valid is added
  val wb_wxd = wb_reg_valid && wb_ctrl.wxd && !io.rocc.resp.valid
  val replay_wb_rocc = wb_reg_valid && wb_ctrl.rocc && Bool(false) // in guardian council, rocc.cmd.ready is always ready
  val replay_wb_lsl = Mux((checker_mode === 1.U), lsl_resp_replay.asBool || lsl_resp_replay_csr.asBool , false.B)
  val wb_csr = (wb_reg_inst(6,0) === 0x73.U) && ((wb_reg_inst(14,12) === 0x2.U) || (wb_reg_inst(14,12) === 0x1.U)) && wb_reg_valid
  lsl_resp_replay_csr := Mux(checker_mode.asBool, wb_csr && !lsl_req_ready_csr, false.B)

  val wb_set_sboard = wb_ctrl.div || wb_dcache_miss || wb_ctrl.rocc
  val replay_wb_common = Mux(checker_mode === 1.U, false.B, io.dmem.s2_nack) || wb_reg_replay
  val replay_wb_without_overtaken = replay_wb_common || replay_wb_rocc
  val wb_should_be_valid_but_be_overtaken = Mux(checker_mode.asBool, icsl_if_overtaking.asBool && wb_reg_valid && !replay_wb_without_overtaken && !replay_wb_lsl && !wb_xcpt && !io.rocc.resp.valid, false.B)
  val let_ret_s_commit = wb_reg_valid && !wb_xcpt && !io.rocc.resp.valid && (wb_reg_pc === pc_special)
  val wb_r_replay = ((wb_should_be_valid_but_be_overtaken || replay_wb_lsl) && !let_ret_s_commit)
  val replay_wb = replay_wb_without_overtaken || wb_r_replay
  take_pc_wb := replay_wb || wb_xcpt || csr.io.eret || wb_reg_flush_pipe

  if (GH_GlobalParams.GH_DEBUG == 1) {
    when (replay_wb && io.core_trace.asBool) {
      printf(midas.targetutils.SynthesizePrintf("C%d: re-wb[%x], [%x], [%x], [%x].\n",
          io.hartid, replay_wb_common.asUInt, wb_should_be_valid_but_be_overtaken.asUInt, replay_wb_lsl.asUInt, let_ret_s_commit.asUInt))
    }
  }
  //===== GuardianCouncil Function: End   ====//

  // writeback arbitration
  val dmem_resp_xpu = Mux((checker_mode === 1.U), !lsl_resp_tag(0).asBool, !io.dmem.resp.bits.tag(0).asBool)
  val dmem_resp_fpu = Mux((checker_mode === 1.U), lsl_resp_tag(0).asBool, io.dmem.resp.bits.tag(0).asBool)
  val dmem_resp_waddr = Mux((checker_mode === 1.U), lsl_resp_tag(5,1), io.dmem.resp.bits.tag(5, 1))
  val dmem_resp_valid = Mux((checker_mode === 1.U), lsl_resp_valid.asBool && lsl_resp_has_data.asBool, io.dmem.resp.valid && io.dmem.resp.bits.has_data)
  val dmem_resp_replay = Mux((checker_mode === 1.U), dmem_resp_valid && lsl_resp_replay.asBool, dmem_resp_valid && io.dmem.resp.bits.replay)

  div.io.resp.ready := !wb_wxd
  val ll_wdata = Wire(init = div.io.resp.bits.data)
  val ll_waddr = Wire(init = div.io.resp.bits.tag)
  val ll_wen = Wire(init = div.io.resp.fire())
  if (usingRoCC) {
    io.rocc.resp.ready := Bool(true)
    when (io.rocc.resp.fire()) {
      div.io.resp.ready := Bool(false)
      ll_wdata := io.rocc.resp.bits.data
      ll_waddr := io.rocc.resp.bits.rd
      ll_wen := Bool(true)
    }
  }
  when (dmem_resp_replay && dmem_resp_xpu) {
    div.io.resp.ready := Bool(false)
    if (usingRoCC)
      io.rocc.resp.ready := Bool(false)
    ll_waddr := dmem_resp_waddr
    ll_wen := Bool(true)
  }

  //===== GuardianCouncil Function: Start ====//
  // Original design:
  // val wb_valid = wb_reg_valid && !replay_wb && !wb_xcpt
  // In GuardianCouncil, RoCC response can be replied in a single cycle, therefore !io.rocc.resp.valid is added
  val wb_valid = wb_reg_valid && !replay_wb && !wb_xcpt && !io.rocc.resp.valid
  if (GH_GlobalParams.GH_DEBUG == 1) {
    when (wb_reg_valid && !wb_valid && io.core_trace.asBool) {
      printf(midas.targetutils.SynthesizePrintf("C%d: bl-wb[%x], [%x].\n",
          io.hartid, replay_wb.asUInt, wb_xcpt.asUInt))
    }
  }
  
  //===== GuardianCouncil Function: End   ====//
  val wb_wen = wb_valid && wb_ctrl.wxd && !lsl_resp_replay_csr
  val rf_wen = wb_wen || ll_wen
  val rf_waddr = Mux(ll_wen, ll_waddr, wb_waddr)
  val rf_wdata = Mux(dmem_resp_valid && dmem_resp_xpu, Mux(checker_mode === 1.U, lsl_resp_data, io.dmem.resp.bits.data(xLen-1, 0)),
                 Mux(ll_wen, ll_wdata,
                 Mux(wb_ctrl.csr =/= CSR.N, Mux(checker_mode.asBool && wb_csr, lsl_resp_data_csr, csr.io.rw.rdata),
                 Mux(wb_ctrl.mul, mul.map(_.io.resp.bits.data).getOrElse(wb_reg_wdata),
                 wb_reg_wdata))))


  lsl_req_valid_csr := Mux(rf_wen, 
                       Mux(dmem_resp_valid && dmem_resp_xpu, false.B,
                       Mux(ll_wen, false.B,
                       Mux(wb_ctrl.csr =/= CSR.N, Mux(checker_mode.asBool && wb_csr, true.B, false.B), false.B))), false.B)

  //===== GuardianCouncil Function: Start ====//
  /* R Features */
  val rsu_slave = Module(new R_RSUSL(R_RSUSLParams(xLen, 32)))
  val lsl = Module(new R_LSL(R_LSLParams(500, xLen)))
  val icsl = Module(new R_ICSL(R_ICSLParams(16)))
  val arfs_shadow = Reg(Vec(32, UInt(xLen.W)))


  // Instantiate RSU
  rsu_slave.io.arfs_if_CPS := io.arfs_if_CPS
  rsu_slave.io.arfs_if_ARFS := Mux((io.packet_arfs(138, 136) === 0x07.U), 1.U, 0.U)
  rsu_slave.io.arfs_index := io.packet_arfs(135, 128)
  rsu_slave.io.arfs_merge := io.packet_arfs(127, 0)
  val rf_wen_rsu = Wire(0.U(1.W))
  rf_wen_rsu := rsu_slave.io.arfs_valid_out
  rsu_pc := rsu_slave.io.pcarf_out
  io.rsu_status := rsu_slave.io.rsu_status
  rsu_slave.io.do_cp_check := icsl.io.if_rh_cp_pc & rsu_slave.io.rsu_status(1)

  for (i <-0 until 31){
    rsu_slave.io.core_arfs_in(i) := rf.read(i)
    rsu_slave.io.core_farfs_in(i) := io.fpu.farfs(i)
  }
  rsu_slave.io.elu_cp_deq := Mux(io.elu_sel.asBool && io.elu_deq.asBool, 1.U, 0.U)
  rsu_slave.io.core_trace := io.core_trace
  csr.io.core_trace := io.core_trace

  csr.io.pfarf_valid := rsu_slave.io.pfarf_valid_out
  csr.io.fcsr_in := rsu_slave.io.fcsr_out

  // Added one cycle delay to ensure the RCU being operated at commited stage 
  // Avodiing uninteded reg write after arf_copy
  val arf_paste_reg = Reg(0.U(1.W))
  arf_paste_reg := io.arf_copy_in
  rsu_slave.io.paste_arfs := arf_paste_reg
  rsu_slave.io.clear_ic_status := icsl.io.clear_ic_status
  rsu_slave.io.record_context := io.record_and_store(1)
  rsu_slave.io.store_from_checker := io.record_and_store(0)

  // Instantiate ICSL
  val r_exception_record = RegInit(0.U(1.W))
  r_exception_record := Mux(csr.io.r_exception.asBool, 1.U, Mux(csr.io.trace(0).valid && !csr.io.trace(0).exception && r_exception_record.asBool, 0.U, r_exception_record))


  icsl.io.ic_counter := io.ic_counter
  icsl.io.icsl_run := arf_paste_reg & (~io.record_and_store(0))
  icsl.io.new_commit := csr.io.trace(0).valid && !csr.io.trace(0).exception
  icsl.io.if_correct_process := io.if_correct_process
  checker_mode := icsl.io.icsl_checkermode
  io.clear_ic_status := icsl.io.clear_ic_status
  icsl_if_overtaking := (icsl.io.if_overtaking | rsu_slave.io.core_hang_up) & !r_exception_record
  icsl_just_overtaking := (icsl.io.if_just_overtaking) & !r_exception_record
  icsl_if_ret_special_pc := icsl.io.if_ret_special_pc
  val returned_to_special_address_valid = Wire(Bool())
  icsl.io.returned_to_special_address_valid := returned_to_special_address_valid
  icsl.io.if_check_completed := rsu_slave.io.if_cp_check_completed
  icsl.io.core_trace := io.core_trace

  // Instantiate LSL
  lsl.io.m_st_valid := Mux((io.packet_lsl(138, 136) === 2.U), 1.U, 0.U)
  lsl.io.m_ld_valid := Mux((io.packet_lsl(138, 136) === 1.U), 1.U, 0.U)
  lsl.io.m_csr_valid := Mux((io.packet_lsl(138, 136) === 3.U), 1.U, 0.U)
  lsl.io.m_csr_data := io.packet_lsl(65, 2)
  lsl.io.m_ldst_data := io.packet_lsl(127,64)
  lsl.io.m_ldst_addr := io.packet_lsl(63,0)

  lsl_req_ready := lsl.io.req_ready
  lsl.io.req_valid := lsl_req_valid
  lsl.io.req_addr := lsl_req_addr
  lsl.io.req_tag := lsl_req_tag
  lsl.io.req_cmd := lsl_req_cmd
  lsl.io.req_data := lsl_req_data
  lsl.io.req_size := lsl_req_size
  lsl.io.req_kill := lsl_req_kill
  lsl.io.req_valid_csr := lsl_req_valid_csr

  lsl_resp_valid := lsl.io.resp_valid
  lsl_resp_tag := lsl.io.resp_tag
  lsl_resp_size := lsl.io.resp_size
  lsl_resp_addr := lsl.io.resp_addr
  lsl_resp_data := lsl.io.resp_data
  lsl_resp_has_data := lsl.io.resp_has_data
  lsl_resp_replay := lsl.io.resp_replay
  io.lsl_near_full := lsl.io.near_full
  lsl_resp_data_csr := lsl.io.resp_data_csr
  lsl_req_ready_csr := lsl.io.req_ready_csr

  // Instantiate ELU
  val elu = Module(new R_ELU(R_ELUParams(4, xLen, 40)))
  elu.io.lsl_req_valid := lsl_req_valid
  elu.io.lsl_req_addr := lsl_req_addr
  elu.io.lsl_req_cmd := lsl_req_cmd
  elu.io.lsl_req_data := lsl_req_data
  elu.io.lsl_req_ready := lsl_req_ready
  elu.io.lsl_req_kill := lsl_req_kill

  elu.io.lsl_resp_valid := lsl_resp_valid
  elu.io.lsl_resp_addr := lsl_resp_addr
  elu.io.lsl_resp_data := lsl_resp_data
  elu.io.wb_pc := Mux(wb_reg_valid, wb_reg_pc, 0.U)
  elu.io.wb_inst := Mux(wb_reg_valid, wb_reg_inst, 0.U)
  io.elu_data := Mux(io.elu_sel.asBool, rsu_slave.io.elu_cp_data, elu.io.elu_data)
  io.elu_status := Cat(rsu_slave.io.elu_status, elu.io.elu_status)
  elu.io.elu_deq := Mux(!io.elu_sel.asBool && io.elu_deq.asBool, 1.U, 0.U)
  elu.io.lsl_resp_addr := lsl_resp_addr
  elu.io.core_trace := io.core_trace


  when (rf_wen_rsu === 1.U) {
    rf.write(rsu_slave.io.arfs_idx_out, rsu_slave.io.arfs_out)
    arfs_shadow(rsu_slave.io.arfs_idx_out) := rsu_slave.io.arfs_out
  } .elsewhen (rf_wen) {
    rf.write(rf_waddr, rf_wdata)
    arfs_shadow(rf_waddr) := rf_wdata
  }


  io.packet_cdc_ready := rsu_slave.io.cdc_ready | lsl.io.cdc_ready
  //===== GuardianCouncil Function: End   ====//

  // hook up control/status regfile
  csr.io.ungated_clock := clock
  csr.io.decode(0).inst := id_inst(0)
  csr.io.exception := wb_xcpt
  csr.io.cause := wb_cause
  csr.io.retire := wb_valid || io.rocc.resp.valid // In GC, rocc_resp is always delivered with an instruction commitment
  csr.io.inst(0) := (if (usingCompressed) Cat(Mux(wb_reg_raw_inst(1, 0).andR, wb_reg_inst >> 16, 0.U), wb_reg_raw_inst(15, 0)) else wb_reg_inst)
  csr.io.interrupts := io.interrupts
  csr.io.hartid := io.hartid
  io.fpu.fcsr_rm := csr.io.fcsr_rm
  csr.io.fcsr_flags := io.fpu.fcsr_flags
  io.fpu.time := csr.io.time(31,0)
  io.fpu.hartid := io.hartid
  csr.io.rocc_interrupt := io.rocc.interrupt
  csr.io.pc := wb_reg_pc
  val tval_dmem_addr = !wb_reg_xcpt
  val tval_any_addr = tval_dmem_addr ||
    wb_reg_cause.isOneOf(Causes.breakpoint, Causes.fetch_access, Causes.fetch_page_fault, Causes.fetch_guest_page_fault)
  val tval_inst = wb_reg_cause === Causes.illegal_instruction
  val tval_valid = wb_xcpt && (tval_any_addr || tval_inst)
  csr.io.gva := wb_xcpt && (tval_any_addr && csr.io.status.v || tval_dmem_addr && wb_reg_hls_or_dv)
  csr.io.tval := Mux(tval_valid, encodeVirtualAddress(wb_reg_wdata, wb_reg_wdata), 0.U)
  csr.io.htval := {
    val htval_valid_imem = wb_reg_xcpt && wb_reg_cause === Causes.fetch_guest_page_fault
    val htval_imem = Mux(htval_valid_imem, io.imem.gpa.bits, 0.U)
    assert(!htval_valid_imem || io.imem.gpa.valid)

    val htval_valid_dmem = wb_xcpt && tval_dmem_addr && io.dmem.s2_xcpt.gf.asUInt.orR && !io.dmem.s2_xcpt.pf.asUInt.orR
    val htval_dmem = Mux(htval_valid_dmem, io.dmem.s2_gpa, 0.U)

    (htval_dmem | htval_imem) >> hypervisorExtraAddrBits
  }
  io.ptw.ptbr := csr.io.ptbr
  io.ptw.hgatp := csr.io.hgatp
  io.ptw.vsatp := csr.io.vsatp
  (io.ptw.customCSRs.csrs zip csr.io.customCSRs).map { case (lhs, rhs) => lhs := rhs }
  io.ptw.status := csr.io.status
  io.ptw.hstatus := csr.io.hstatus
  io.ptw.gstatus := csr.io.gstatus
  io.ptw.pmp := csr.io.pmp
  // Here, our overtaking should not cause any write-back of the CSR registers
  // If, we do not have below 'filtering', an exception could be still generated
  csr.io.rw.addr := wb_reg_inst(31,20)
  csr.io.rw.cmd := CSR.maskCmd(wb_reg_valid, wb_ctrl.csr)
  csr.io.rw.wdata := wb_reg_wdata
  io.trace := csr.io.trace
  for (((iobpw, wphit), bp) <- io.bpwatch zip wb_reg_wphit zip csr.io.bp) {
    iobpw.valid(0) := wphit
    iobpw.action := bp.control.action
  }

  val hazard_targets = Seq((id_ctrl.rxs1 && id_raddr1 =/= UInt(0), id_raddr1),
                           (id_ctrl.rxs2 && id_raddr2 =/= UInt(0), id_raddr2),
                           (id_ctrl.wxd  && id_waddr  =/= UInt(0), id_waddr))
  val fp_hazard_targets = Seq((io.fpu.dec.ren1, id_raddr1),
                              (io.fpu.dec.ren2, id_raddr2),
                              (io.fpu.dec.ren3, id_raddr3),
                              (io.fpu.dec.wen, id_waddr))

  val sboard = new Scoreboard(32, true)
  sboard.clear(ll_wen, ll_waddr)
  def id_sboard_clear_bypass(r: UInt) = {
    // ll_waddr arrives late when D$ has ECC, so reshuffle the hazard check
    if (!tileParams.dcache.get.dataECC.isDefined) ll_wen && ll_waddr === r
    else div.io.resp.fire() && div.io.resp.bits.tag === r || dmem_resp_replay && dmem_resp_xpu && dmem_resp_waddr === r
  }
  val id_sboard_hazard = checkHazards(hazard_targets, rd => sboard.read(rd) && !id_sboard_clear_bypass(rd))
  sboard.set(wb_set_sboard && wb_wen, wb_waddr)

  // stall for RAW/WAW hazards on CSRs, loads, AMOs, and mul/div in execute stage.
  val ex_cannot_bypass = ex_ctrl.csr =/= CSR.N || ex_ctrl.jalr || ex_ctrl.mem || ex_ctrl.mul || ex_ctrl.div || ex_ctrl.fp || ex_ctrl.rocc || ex_scie_pipelined
  val data_hazard_ex = ex_ctrl.wxd && checkHazards(hazard_targets, _ === ex_waddr)
  val fp_data_hazard_ex = ex_ctrl.wfd && checkHazards(fp_hazard_targets, _ === ex_waddr)
  val id_ex_hazard = ex_reg_valid && (data_hazard_ex && ex_cannot_bypass || fp_data_hazard_ex)

  // stall for RAW/WAW hazards on CSRs, LB/LH, and mul/div in memory stage.
  val mem_mem_cmd_bh =
    if (fastLoadWord) Bool(!fastLoadByte) && mem_reg_slow_bypass
    else Bool(true)
  val mem_cannot_bypass = mem_ctrl.csr =/= CSR.N || mem_ctrl.mem && mem_mem_cmd_bh || mem_ctrl.mul || mem_ctrl.div || mem_ctrl.fp
  val data_hazard_mem = mem_ctrl.wxd && checkHazards(hazard_targets, _ === mem_waddr)
  val fp_data_hazard_mem = mem_ctrl.wfd && checkHazards(fp_hazard_targets, _ === mem_waddr)
  val id_mem_hazard = mem_reg_valid && (data_hazard_mem && mem_cannot_bypass || fp_data_hazard_mem)
  id_load_use := mem_reg_valid && data_hazard_mem && mem_ctrl.mem

  // stall for RAW/WAW hazards on load/AMO misses and mul/div in writeback.
  val data_hazard_wb = wb_ctrl.wxd && checkHazards(hazard_targets, _ === wb_waddr)
  val fp_data_hazard_wb = wb_ctrl.wfd && checkHazards(fp_hazard_targets, _ === wb_waddr)
  val id_wb_hazard = wb_reg_valid && (data_hazard_wb && wb_set_sboard || fp_data_hazard_wb)

  val id_stall_fpu = if (usingFPU) {
    val fp_sboard = new Scoreboard(32)
    fp_sboard.set((wb_dcache_miss && wb_ctrl.wfd || io.fpu.sboard_set) && wb_valid, wb_waddr)
    fp_sboard.clear(dmem_resp_replay && dmem_resp_fpu, dmem_resp_waddr)
    fp_sboard.clear(io.fpu.sboard_clr, io.fpu.sboard_clra)

    checkHazards(fp_hazard_targets, fp_sboard.read _)
  } else Bool(false)

  val dcache_blocked = {
    // speculate that a blocked D$ will unblock the cycle after a Grant
    val blocked = Reg(Bool())
    blocked := !io.dmem.req.ready && io.dmem.clock_enabled && !io.dmem.perf.grant && (blocked || io.dmem.req.valid || io.dmem.s2_nack)
    blocked && !io.dmem.perf.grant
  }
  val rocc_blocked = Reg(Bool())
  rocc_blocked := !wb_xcpt && !io.rocc.cmd.ready && (io.rocc.cmd.valid || rocc_blocked)

  val ctrl_stalld =
    //===== GuardianCouncil Function: Start ====//
    // Original design:
    // id_ex_hazard || id_mem_hazard || id_wb_hazard || id_sboard_hazard ||
    // In GuardianCouncil, RoCC response can be replied in a single cycle, therefore RoCC does not cause a hazzard
    rsu_slave.io.core_hang_up.asBool || id_ex_hazard || id_mem_hazard || (id_wb_hazard && !wb_ctrl.rocc) || id_sboard_hazard ||
    //===== GuardianCouncil Function: End   ====//
    csr.io.singleStep && (ex_reg_valid || mem_reg_valid || wb_reg_valid) ||
    id_csr_en && csr.io.decode(0).fp_csr && !io.fpu.fcsr_rdy ||
    id_ctrl.fp && id_stall_fpu ||
    id_ctrl.mem && dcache_blocked || // reduce activity during D$ misses
    id_ctrl.rocc && rocc_blocked || // reduce activity while RoCC is busy
    id_ctrl.div && (!(div.io.req.ready || (div.io.resp.valid && !wb_wxd)) || div.io.req.valid) || // reduce odds of replay
    !clock_en ||
    id_do_fence ||
    csr.io.csr_stall ||
    id_reg_pause ||
    io.traceStall ||
    !io.clk_enable_gh
    // ((io.s_or_r === 1.U) && (checker_mode === 1.U) && (lsl_req_ready === 0.U)) // hang the pipeline, when the lsl is not reqdy
  ctrl_killd := !ibuf.io.inst(0).valid || ibuf.io.inst(0).bits.replay || take_pc_mem_wb || ctrl_stalld || csr.io.interrupt

  if (GH_GlobalParams.GH_DEBUG == 1) {
    when (ctrl_killd && io.core_trace.asBool) {
      printf(midas.targetutils.SynthesizePrintf("C%d: kd [%x], [%x], [%x], [%x], [%x].\n",
          io.hartid, ibuf.io.inst(0).valid.asUInt, ibuf.io.inst(0).bits.replay.asUInt, take_pc_mem_wb.asUInt, ctrl_stalld.asUInt, csr.io.interrupt.asUInt))
    }
    when (ctrl_stalld && io.core_trace.asBool) {
      printf(midas.targetutils.SynthesizePrintf("C%d: sd [%x], [%x], [%x], [%x], [%x], [%x], [%x].\n",
          io.hartid, rsu_slave.io.core_hang_up.asUInt, id_ex_hazard.asUInt, id_mem_hazard.asUInt, id_wb_hazard.asUInt, id_sboard_hazard.asUInt, (csr.io.singleStep && (ex_reg_valid || mem_reg_valid || wb_reg_valid)).asUInt, csr.io.csr_stall.asUInt))
    }
  }

  returned_to_special_address_valid := (wb_valid || io.rocc.resp.valid) && (wb_reg_pc === pc_special)

  io.imem.req.valid := take_pc
  io.imem.req.bits.speculative := !take_pc_wb
  io.imem.req.bits.pc :=
    Mux(wb_xcpt || csr.io.eret, csr.io.evec, // exception or [m|s]ret
    Mux(replay_wb,              Mux(icsl_if_ret_special_pc.asBool && wb_r_replay.asBool, pc_special, wb_reg_pc),   // replay
                                mem_npc))    // flush or branch misprediction
  io.imem.flush_icache := wb_reg_valid && wb_ctrl.fence_i && (Mux(checker_mode === 1.U, false.B, !io.dmem.s2_nack))
  io.imem.might_request := {
    imem_might_request_reg := ex_pc_valid || mem_pc_valid || io.ptw.customCSRs.disableICacheClockGate || true.B  // We do not wish the IMM to sleep as it can have a replay at any time!
    imem_might_request_reg
  }
  io.imem.sfence.valid := wb_reg_valid && wb_reg_sfence
  io.imem.sfence.bits.rs1 := wb_reg_mem_size(0)
  io.imem.sfence.bits.rs2 := wb_reg_mem_size(1)
  io.imem.sfence.bits.addr := wb_reg_wdata
  io.imem.sfence.bits.asid := wb_reg_rs2
  io.imem.sfence.bits.hv := wb_reg_hfence_v
  io.imem.sfence.bits.hg := wb_reg_hfence_g
  io.ptw.sfence := io.imem.sfence

  ibuf.io.inst(0).ready := !ctrl_stalld

  io.imem.btb_update.valid := mem_reg_valid && !take_pc_wb && mem_wrong_npc && (!mem_cfi || mem_cfi_taken)
  io.imem.btb_update.bits.isValid := mem_cfi
  io.imem.btb_update.bits.cfiType :=
    Mux((mem_ctrl.jal || mem_ctrl.jalr) && mem_waddr(0), CFIType.call,
    Mux(mem_ctrl.jalr && (mem_reg_inst(19,15) & regAddrMask) === BitPat("b00?01"), CFIType.ret,
    Mux(mem_ctrl.jal || mem_ctrl.jalr, CFIType.jump,
    CFIType.branch)))
  io.imem.btb_update.bits.target := io.imem.req.bits.pc
  io.imem.btb_update.bits.br_pc := (if (usingCompressed) mem_reg_pc + Mux(mem_reg_rvc, UInt(0), UInt(2)) else mem_reg_pc)
  io.imem.btb_update.bits.pc := ~(~io.imem.btb_update.bits.br_pc | (coreInstBytes*fetchWidth-1))
  io.imem.btb_update.bits.prediction := mem_reg_btb_resp

  io.imem.bht_update.valid := mem_reg_valid && !take_pc_wb
  io.imem.bht_update.bits.pc := io.imem.btb_update.bits.pc
  io.imem.bht_update.bits.taken := mem_br_taken
  io.imem.bht_update.bits.mispredict := mem_wrong_npc
  io.imem.bht_update.bits.branch := mem_ctrl.branch
  io.imem.bht_update.bits.prediction := mem_reg_btb_resp.bht

  io.fpu.valid := !ctrl_killd && id_ctrl.fp
  io.fpu.killx := ctrl_killx
  io.fpu.killm := killm_common
  io.fpu.inst := id_inst(0)
  io.fpu.fromint_data := ex_rs(0)
  /* R Features */
  io.fpu.dmem_resp_val := dmem_resp_valid && dmem_resp_fpu
  io.fpu.dmem_resp_data := Mux(checker_mode === 1.U, lsl_resp_data, io.dmem.resp.bits.data_word_bypass)
  io.fpu.dmem_resp_type := Mux(checker_mode === 1.U, lsl_resp_size, io.dmem.resp.bits.size)
  io.fpu.dmem_resp_tag := dmem_resp_waddr
  io.fpu.keep_clock_enabled := io.ptw.customCSRs.disableCoreClockGate
  
  io.fpu.r_farf_bits := rsu_slave.io.farfs_out
  io.fpu.r_farf_idx := rsu_slave.io.arfs_idx_out
  io.fpu.r_farf_valid := rsu_slave.io.arfs_valid_out
  io.fpu.retire := wb_valid || io.rocc.resp.valid
  io.fpu.checker_mode := checker_mode
  io.fpu.core_trace := io.core_trace.asBool
  io.fpu.if_overtaking := icsl.io.if_overtaking
  io.fpu.if_just_overtaking := icsl.io.if_just_overtaking
  icsl.io.something_inflight := !div.io.req.ready || io.fpu.fpu_inflight

  // io.fpu.r_if_overtaking := Mux(checker_mode.asBool, icsl_if_overtaking.asBool, false.B)

  /* R Feature --- LSL */
  /*
  io.dmem.req.valid     := ex_reg_valid && ex_ctrl.mem
  val ex_dcache_tag = Cat(ex_waddr, ex_ctrl.fp)
  require(coreParams.dcacheReqTagBits >= ex_dcache_tag.getWidth)
  io.dmem.req.bits.tag  := ex_dcache_tag
  io.dmem.req.bits.cmd  := ex_ctrl.mem_cmd
  io.dmem.req.bits.size := ex_reg_mem_size
  io.dmem.req.bits.signed := !Mux(ex_reg_hls, ex_reg_inst(20), ex_reg_inst(14))
  io.dmem.req.bits.phys := Bool(false)
  io.dmem.req.bits.addr := encodeVirtualAddress(ex_rs(0), alu.io.adder_out)
  io.dmem.req.bits.idx.foreach(_ := io.dmem.req.bits.addr)
  io.dmem.req.bits.dprv := Mux(ex_reg_hls, csr.io.hstatus.spvp, csr.io.status.dprv)
  io.dmem.req.bits.dv := ex_reg_hls || csr.io.status.dv
  io.dmem.s1_data.data := (if (fLen == 0) mem_reg_rs2 else Mux(mem_ctrl.fp, Fill((xLen max fLen) / fLen, io.fpu.store_data), mem_reg_rs2))
  io.dmem.s1_kill := killm_common || mem_ldst_xcpt || fpu_kill_mem
  io.dmem.s2_kill := false
  */
  
  // Simply tied-off the signals sent to D$, when the core is in the checker mode.
  // It might be fine only mask the io.dmem.req.valid, but for safety -- let us amsk all dmem.req signals.
  io.dmem.req.valid     := Mux(checker_mode === 1.U, 0.U, ex_reg_valid && ex_ctrl.mem)
  val ex_dcache_tag      = Mux(checker_mode === 1.U, 0.U, Cat(ex_waddr, ex_ctrl.fp))
  require(coreParams.dcacheReqTagBits >= ex_dcache_tag.getWidth)
  io.dmem.req.bits.tag  := Mux(checker_mode === 1.U, 0.U, ex_dcache_tag)
  io.dmem.req.bits.cmd  := Mux(checker_mode === 1.U, 0.U, ex_ctrl.mem_cmd)
  io.dmem.req.bits.size := Mux(checker_mode === 1.U, 0.U, ex_reg_mem_size)
  io.dmem.req.bits.signed := Mux(checker_mode === 1.U, 0.U, !Mux(ex_reg_hls, ex_reg_inst(20), ex_reg_inst(14)))
  io.dmem.req.bits.phys := Bool(false)
  io.dmem.req.bits.addr := Mux(checker_mode === 1.U, 0.U, encodeVirtualAddress(ex_rs(0), alu.io.adder_out))
  io.dmem.req.bits.idx.foreach(_ := Mux(checker_mode === 1.U, 0.U, io.dmem.req.bits.addr))
  io.dmem.req.bits.dprv := Mux(checker_mode === 1.U, 0.U, Mux(ex_reg_hls, csr.io.hstatus.spvp, csr.io.status.dprv))
  io.dmem.req.bits.dv := Mux(checker_mode === 1.U, 0.U, ex_reg_hls || csr.io.status.dv)
  io.dmem.s1_data.data := Mux(checker_mode === 1.U, 0.U, (if (fLen == 0) mem_reg_rs2 else Mux(mem_ctrl.fp, Fill((xLen max fLen) / fLen, io.fpu.store_data), mem_reg_rs2)))
  io.dmem.s1_kill := Mux(checker_mode === 1.U, 0.U, killm_common || mem_ldst_xcpt || fpu_kill_mem)
  io.dmem.s2_kill := false

  lsl_req_valid             := Mux(checker_mode === 1.U, (mem_reg_valid && mem_ctrl.mem), 0.U)
  val mem_dcache_tag         = Mux(checker_mode === 1.U, Cat(mem_waddr, mem_ctrl.fp), 0.U)
  lsl_req_tag               := mem_dcache_tag
  val alu_adder_out          = Reg(UInt())
  val mem_rs0                = Reg(UInt())
  alu_adder_out             := alu.io.adder_out
  mem_rs0                   := ex_rs(0)
  lsl_req_addr              := Mux(checker_mode === 1.U, encodeVirtualAddress(mem_rs0, alu_adder_out), 0.U)
  lsl_req_cmd               := Mux(checker_mode === 1.U, Cat(isWrite(mem_ctrl.mem_cmd).asUInt, isRead(mem_ctrl.mem_cmd).asUInt), 0.U)
  lsl_req_size              := Mux(checker_mode === 1.U, mem_reg_mem_size, 0.U)
  lsl_req_data              := Mux(checker_mode === 1.U, (if (fLen == 0) mem_reg_rs2 else Mux(mem_ctrl.fp, Fill((xLen max fLen) / fLen, io.fpu.store_data), mem_reg_rs2)), 0.U)
  lsl_req_kill              := Mux(checker_mode === 1.U, (killm_common || mem_ldst_xcpt || fpu_kill_mem), 0.U)
  io.icsl_status            := Mux((icsl.io.icsl_status === 1.U) && (rsu_slave.io.rsu_status === 0.U) && (lsl.io.if_empty === 1.U), 1.U, 0.U)

  // don't let D$ go to sleep if we're probably going to use it soon
  io.dmem.keep_clock_enabled := ibuf.io.inst(0).valid && id_ctrl.mem && !csr.io.csr_stall

  //===== GuardianCouncil Function: Start ====//
  when (mem_pc_valid) {
    io.rocc.cmd.valid := mem_reg_valid && mem_ctrl.rocc
    io.rocc.exception := mem_xcpt && csr.io.status.xs.orR
    io.rocc.cmd.bits.status := csr.io.status
    io.rocc.cmd.bits.inst := new RoCCInstruction().fromBits(mem_reg_inst)
    io.rocc.cmd.bits.rs1 := mem_reg_wdata
    io.rocc.cmd.bits.rs2 := mem_reg_rs2
  }

  io.ght_prv := csr.io.status.prv
  //===== GuardianCouncil Function: End  ====//

  // gate the clock
  val unpause = csr.io.time(rocketParams.lgPauseCycles-1, 0) === 0 || csr.io.inhibit_cycle || io.dmem.perf.release || take_pc
  when (unpause) { id_reg_pause := false }
  io.cease := csr.io.status.cease && !clock_en_reg
  io.wfi := csr.io.status.wfi
  // Below should not be used, as we do not enable the clock gating for the core
  if (rocketParams.clockGate) {
    long_latency_stall := csr.io.csr_stall || io.dmem.perf.blocked || id_reg_pause && !unpause
    clock_en := clock_en_reg || ex_pc_valid || (!long_latency_stall && io.imem.resp.valid)
    clock_en_reg :=
      ex_pc_valid || mem_pc_valid || wb_pc_valid || // instruction in flight
      io.ptw.customCSRs.disableCoreClockGate || // chicken bit
      !div.io.req.ready || // mul/div in flight
      usingFPU && !io.fpu.fcsr_rdy || // long-latency FPU in flight
      io.dmem.replay_next || // long-latency load replaying
      (!long_latency_stall && (ibuf.io.inst(0).valid || io.imem.resp.valid)) // instruction pending
    println("#### Jessica #### Creating clock gating for Rocekt ...!!!")
    assert(!(ex_pc_valid || mem_pc_valid || wb_pc_valid) || clock_en)
  }

  // evaluate performance counters
  val icache_blocked = !(io.imem.resp.valid || RegNext(io.imem.resp.valid))
  csr.io.counters foreach { c => c.inc := RegNext(perfEvents.evaluate(c.eventSel)) }

  val coreMonitorBundle = Wire(new CoreMonitorBundle(xLen, fLen))

  coreMonitorBundle.clock := clock
  coreMonitorBundle.reset := reset
  coreMonitorBundle.hartid := io.hartid
  coreMonitorBundle.timer := csr.io.time(31,0)
  coreMonitorBundle.valid := csr.io.trace(0).valid && !csr.io.trace(0).exception
  coreMonitorBundle.pc := csr.io.trace(0).iaddr(vaddrBitsExtended-1, 0).sextTo(xLen)
  coreMonitorBundle.wrenx := wb_wen && !wb_set_sboard
  coreMonitorBundle.wrenf := false.B
  coreMonitorBundle.wrdst := wb_waddr
  coreMonitorBundle.wrdata := rf_wdata
  coreMonitorBundle.rd0src := wb_reg_inst(19,15)
  coreMonitorBundle.rd0val := Reg(next=Reg(next=ex_rs(0)))
  coreMonitorBundle.rd1src := wb_reg_inst(24,20)
  coreMonitorBundle.rd1val := Reg(next=Reg(next=ex_rs(1)))
  coreMonitorBundle.inst := csr.io.trace(0).insn
  coreMonitorBundle.excpt := csr.io.trace(0).exception
  coreMonitorBundle.priv_mode := csr.io.trace(0).priv

  if (GH_GlobalParams.GH_DEBUG == 1) {
    when (csr.io.trace(0).valid && io.core_trace.asBool) {
      printf(midas.targetutils.SynthesizePrintf("C%d: [%d] pc=[%x] W[r%d=%x][%d] R[r%d=%x] R[r%d=%x] inst=[%x] sl_counter=[%x], rf_sl_counter=[%x]\n",
          io.hartid, coreMonitorBundle.valid,
          coreMonitorBundle.pc,
          Mux(wb_ctrl.wxd || wb_ctrl.wfd, coreMonitorBundle.wrdst, 0.U),
          Mux(coreMonitorBundle.wrenx, coreMonitorBundle.wrdata, 0.U),
          coreMonitorBundle.wrenx,
          Mux(wb_ctrl.rxs1 || wb_ctrl.rfs1, coreMonitorBundle.rd0src, 0.U),
          Mux(wb_ctrl.rxs1 || wb_ctrl.rfs1, coreMonitorBundle.rd0val, 0.U),
          Mux(wb_ctrl.rxs2 || wb_ctrl.rfs2, coreMonitorBundle.rd1src, 0.U),
          Mux(wb_ctrl.rxs2 || wb_ctrl.rfs2, coreMonitorBundle.rd1val, 0.U),
          coreMonitorBundle.inst, (icsl.io.debug_sl_counter + 1.U), io.ic_counter))
    }
  } else {
    /*
    when (csr.io.trace(0).valid) {
      printf("C%d: %d [%d] pc=[%x] W[r%d=%x][%d] R[r%d=%x] R[r%d=%x] inst=[%x] DASM(%x) sl_counter=[%x]\n",
          io.hartid, coreMonitorBundle.timer, coreMonitorBundle.valid,
          coreMonitorBundle.pc,
          Mux(wb_ctrl.wxd || wb_ctrl.wfd, coreMonitorBundle.wrdst, 0.U),
          Mux(coreMonitorBundle.wrenx, coreMonitorBundle.wrdata, 0.U),
          coreMonitorBundle.wrenx,
          Mux(wb_ctrl.rxs1 || wb_ctrl.rfs1, coreMonitorBundle.rd0src, 0.U),
          Mux(wb_ctrl.rxs1 || wb_ctrl.rfs1, coreMonitorBundle.rd0val, 0.U),
          Mux(wb_ctrl.rxs2 || wb_ctrl.rfs2, coreMonitorBundle.rd1src, 0.U),
          Mux(wb_ctrl.rxs2 || wb_ctrl.rfs2, coreMonitorBundle.rd1val, 0.U),
          coreMonitorBundle.inst, coreMonitorBundle.inst, (icsl.io.debug_sl_counter + 1.U))
    }
    */
  }

  // CoreMonitorBundle for late latency writes
  val xrfWriteBundle = Wire(new CoreMonitorBundle(xLen, fLen))

  xrfWriteBundle.clock := clock
  xrfWriteBundle.reset := reset
  xrfWriteBundle.hartid := io.hartid
  xrfWriteBundle.timer := csr.io.time(31,0)
  xrfWriteBundle.valid := false.B
  xrfWriteBundle.pc := 0.U
  xrfWriteBundle.wrdst := rf_waddr
  xrfWriteBundle.wrenx := rf_wen && !(csr.io.trace(0).valid && wb_wen && (wb_waddr === rf_waddr))
  xrfWriteBundle.wrenf := false.B
  xrfWriteBundle.wrdata := rf_wdata
  xrfWriteBundle.rd0src := 0.U
  xrfWriteBundle.rd0val := 0.U
  xrfWriteBundle.rd1src := 0.U
  xrfWriteBundle.rd1val := 0.U
  xrfWriteBundle.inst := 0.U
  xrfWriteBundle.excpt := false.B
  xrfWriteBundle.priv_mode := csr.io.trace(0).priv

  PlusArg.timeout(
    name = "max_core_cycles",
    docstring = "Kill the emulation after INT rdtime cycles. Off if 0."
  )(csr.io.time)

  } // leaving gated-clock domain
  val rocketImpl = withClock (gated_clock) { new RocketImpl }

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))

  def coverExceptions(exceptionValid: Bool, cause: UInt, labelPrefix: String, coverCausesLabels: Seq[(Int, String)]): Unit = {
    for ((coverCause, label) <- coverCausesLabels) {
      property.cover(exceptionValid && (cause === UInt(coverCause)), s"${labelPrefix}_${label}")
    }
  }

  def checkHazards(targets: Seq[(Bool, UInt)], cond: UInt => Bool) =
    targets.map(h => h._1 && cond(h._2)).reduce(_||_)

  def encodeVirtualAddress(a0: UInt, ea: UInt) = if (vaddrBitsExtended == vaddrBits) ea else {
    // efficient means to compress 64-bit VA into vaddrBits+1 bits
    // (VA is bad if VA(vaddrBits) != VA(vaddrBits-1))
    val a = a0.asSInt >> vaddrBits
    val msb = Mux(a === 0.S || a === -1.S, ea(vaddrBits), !ea(vaddrBits-1))
    Cat(msb, ea(vaddrBits-1,0))
  }

  class Scoreboard(n: Int, zero: Boolean = false)
  {
    def set(en: Bool, addr: UInt): Unit = update(en, _next | mask(en, addr))
    def clear(en: Bool, addr: UInt): Unit = update(en, _next & ~mask(en, addr))
    def read(addr: UInt): Bool = r(addr)
    def readBypassed(addr: UInt): Bool = _next(addr)

    private val _r = Reg(init=Bits(0, n))
    private val r = if (zero) (_r >> 1 << 1) else _r
    private var _next = r
    private var ens = Bool(false)
    private def mask(en: Bool, addr: UInt) = Mux(en, UInt(1) << addr, UInt(0))
    private def update(en: Bool, update: UInt) = {
      _next = update
      ens = ens || en
      when (ens) { _r := _next }
    }
  }
}

class RegFile(n: Int, w: Int, zero: Boolean = false) {
  val rf = Mem(n, UInt(width = w))
  private def access(addr: UInt) = rf(~addr(log2Up(n)-1,0))
  private val reads = ArrayBuffer[(UInt,UInt)]()
  private var canRead = true
  def read(addr: UInt) = {
    require(canRead)
    reads += addr -> Wire(UInt())
    reads.last._2 := Mux(Bool(zero) && addr === UInt(0), UInt(0), access(addr))
    reads.last._2
  }
  def write(addr: UInt, data: UInt) = {
    canRead = false
    when (addr =/= UInt(0)) {
      access(addr) := data
      for ((raddr, rdata) <- reads)
        when (addr === raddr) { rdata := data }
    }
  }
}

object ImmGen {
  def apply(sel: UInt, inst: UInt) = {
    val sign = Mux(sel === IMM_Z, SInt(0), inst(31).asSInt)
    val b30_20 = Mux(sel === IMM_U, inst(30,20).asSInt, sign)
    val b19_12 = Mux(sel =/= IMM_U && sel =/= IMM_UJ, sign, inst(19,12).asSInt)
    val b11 = Mux(sel === IMM_U || sel === IMM_Z, SInt(0),
              Mux(sel === IMM_UJ, inst(20).asSInt,
              Mux(sel === IMM_SB, inst(7).asSInt, sign)))
    val b10_5 = Mux(sel === IMM_U || sel === IMM_Z, Bits(0), inst(30,25))
    val b4_1 = Mux(sel === IMM_U, Bits(0),
               Mux(sel === IMM_S || sel === IMM_SB, inst(11,8),
               Mux(sel === IMM_Z, inst(19,16), inst(24,21))))
    val b0 = Mux(sel === IMM_S, inst(7),
             Mux(sel === IMM_I, inst(20),
             Mux(sel === IMM_Z, inst(15), Bits(0))))

    Cat(sign, b30_20, b19_12, b11, b10_5, b4_1, b0).asSInt
  }
}
