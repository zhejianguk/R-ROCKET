package freechips.rocketchip.guardiancouncil


import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, HierarchicalLocation, HasTiles, TLBusWrapperLocation}
import freechips.rocketchip.diplomacy._
//===== GuardianCouncil Function: Start ====//
import freechips.rocketchip.guardiancouncil._
//===== GuardianCouncil Function: End   ====//


case class GHMParams(
  number_of_little_cores: Int,
  width_GH_packet: Int
)


class GHMIO(params: GHMParams) extends Bundle {
  val ghm_packet_in                              = Input(UInt(params.width_GH_packet.W))
  val ghm_packet_dest                            = Input(UInt((params.number_of_little_cores+1).W))
  val ghm_status_in                              = Input(UInt(32.W))
  val ghm_packet_outs                            = Output(Vec(params.number_of_little_cores, UInt(params.width_GH_packet.W)))
  val ghm_status_outs                            = Output(Vec(params.number_of_little_cores, UInt(32.W)))
  val ghe_event_in                               = Input(Vec(params.number_of_little_cores, UInt(6.W)))
  val clear_ic_status                            = Input(Vec(params.number_of_little_cores, UInt(1.W)))
  val clear_ic_status_tomain                     = Output(UInt(GH_GlobalParams.GH_NUM_CORES.W))
  val bigcore_hang                               = Output(UInt(1.W))
  val bigcore_comp                               = Output(UInt(3.W))
  val debug_bp                                   = Output(UInt(2.W))
  val ic_counter                                 = Input(UInt((16*GH_GlobalParams.GH_NUM_CORES).W))
  val icsl_counter                               = Output(Vec(params.number_of_little_cores, UInt(16.W)))
  val ghe_revent_in                              = Input(Vec(params.number_of_little_cores, UInt(1.W)))
  val icsl_na                                    = Output(UInt((GH_GlobalParams.GH_NUM_CORES).W))

  val debug_gcounter                             = Output(UInt(64.W))
  val if_agg_free                                = Input(UInt(1.W))
}

trait HasGHMIO extends BaseModule {
  val params: GHMParams
  val io = IO(new GHMIO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHM (val params: GHMParams)(implicit p: Parameters) extends LazyModule
{
  lazy val module = new LazyModuleImp(this) {
    val io                                         = IO(new GHMIO(params))

    // Adding a register to avoid the critical path
    val packet_dest                                = WireInit(0.U((params.number_of_little_cores+1).W))
    val packet_out_wires                           = WireInit(VecInit(Seq.fill(params.number_of_little_cores)(0.U(params.width_GH_packet.W))))
    val cdc_busy                                   = WireInit(VecInit(Seq.fill(params.number_of_little_cores)(0.U(1.W))))
    val cdc_empty                                  = WireInit(VecInit(Seq.fill(params.number_of_little_cores)(0.U(1.W))))

    val u_cdc                                      = Seq.fill(params.number_of_little_cores) {Module(new GH_CDCH2LFIFO_HandShake(GH_CDCH2L_Params (0, params.width_GH_packet, 32)))}


    packet_dest                                   := io.ghm_packet_dest

    // CDC
    for (i <- 0 to params.number_of_little_cores - 1) {      
      u_cdc(i).io.cdc_data_in                     := io.ghm_packet_in
      u_cdc(i).io.cdc_push                        := packet_dest(i)
      packet_out_wires(i)                         := Cat(u_cdc(i).io.cdc_flag, u_cdc(i).io.cdc_data_out(GH_GlobalParams.GH_WIDITH_PACKETS-2, 0))
      u_cdc(i).io.cdc_pull                        := io.ghe_event_in(i)(4)
      u_cdc(i).io.cdc_slave_busy                  := io.ghe_event_in(i)(0)
      u_cdc(i).io.cdc_ack                         := io.ghe_event_in(i)(5)
      cdc_busy(i)                                 := u_cdc(i).io.cdc_busy
      cdc_empty(i)                                := u_cdc(i).io.cdc_empty
    }
    val zero                                       = WireInit(0.U(1.W))
    io.clear_ic_status_tomain                     := Cat(io.clear_ic_status.reverse.reduce(Cat(_,_)), zero)

    
    var warning                                    = WireInit(0.U(1.W))

    val num_of_activated_cores                     = io.ghm_status_in(30, 23)
    val ghe_event_reg                              = RegInit(VecInit(Seq.fill(params.number_of_little_cores)(0.U(3.W))))
    val ghe_event                                  = WireInit(0.U(3.W))
    val initalised                                 = WireInit(0.U(1.W))

    ghe_event                                     := ghe_event_reg(num_of_activated_cores-1.U)
    val u_and_gates                                = Seq.fill(params.number_of_little_cores) {Module(new GH_ANDGATE(ANDGATEParams (3, params.number_of_little_cores)))}

    for (i <- 0 to params.number_of_little_cores - 1){
      for (j <- 0 to params.number_of_little_cores - 1){
        if (j > i){
          u_and_gates(i).io.in(j)                  := 7.U // 3'b111
        } else {
          u_and_gates(i).io.in(j)                  := io.ghe_event_in(j)(3,1)
        }
      }
      ghe_event_reg(i)                             := u_and_gates(i).io.out
    }




    val debug_gcounter                             = RegInit (0.U(64.W))

    // 2-cyecle delays are added to ensure all filtering activities are completed.
    val ghm_status_delay1_cycle                    = RegInit(0.U(5.W))
    val ghm_status_delay2_cycle                    = RegInit(0.U(5.W))
    val ghm_status_delay3_cycle                    = RegInit(0.U(5.W))
    val ghm_status_delay4_cycle                    = RegInit(0.U(5.W))
    ghm_status_delay1_cycle                       := io.ghm_status_in(4,0)
    ghm_status_delay2_cycle                       := ghm_status_delay1_cycle
    ghm_status_delay3_cycle                       := ghm_status_delay2_cycle
    ghm_status_delay4_cycle                       := ghm_status_delay3_cycle

    val if_filters_empty                           = io.ghm_status_in(31)
    val if_ghm_empty                               = Mux(((io.ghm_packet_dest === 0.U) && (io.ghm_packet_in === 0.U)), 1.U, 0.U)
    val if_cdc_empty                               = cdc_empty.reduce(_&_)
    val if_no_inflight_packets                     = if_filters_empty & if_ghm_empty & io.if_agg_free & if_cdc_empty

    val zeros_59bit                                = WireInit(0.U(59.W))
    for(i <- 0 to params.number_of_little_cores - 1) {
      io.ghm_packet_outs(i)                       := packet_out_wires(i)
      io.ghm_status_outs(i)                       := Mux((if_no_inflight_packets === 1.U), Cat(zeros_59bit, ghm_status_delay4_cycle), 1.U)
    }


    for(i <- 0 to params.number_of_little_cores - 1) {
      warning                                     = warning | cdc_busy(i)
    }

    when (io.ghm_packet_dest =/= 0.U) {
      debug_gcounter                             := debug_gcounter + 1.U
    }

    io.bigcore_hang                              := warning
    io.bigcore_comp                              := ghe_event

    io.debug_gcounter                            := debug_gcounter

    val debug_collecting_checker_status           = io.ghe_event_in.reduce(_|_)
    val debug_backpressure_checkers               = debug_collecting_checker_status(0)
    io.debug_bp                                  := Cat(warning, debug_backpressure_checkers) // [1]: CDC; [0]: Checker

    for (i <- 0 to params.number_of_little_cores - 1) {
      io.icsl_counter(i)                         := io.ic_counter((i+1)*16+15, (i+1)*16)
    }
    io.icsl_na                                   := Cat(io.ghe_revent_in.reverse.reduce(Cat(_,_)), zero)
  }
}

case class GHMCoreLocated(loc: HierarchicalLocation) extends Field[Option[GHMParams]](None)

object GHMCore {

  def attach(params: GHMParams, subsystem: BaseSubsystem with HasTiles, where: TLBusWrapperLocation)
            (implicit p: Parameters) {
    val number_of_ghes                             = subsystem.tile_ghe_packet_in_EPNodes.size
    println("#### Jessica #### Tieing off GHM **Nodes**, core number:", number_of_ghes,"...!!")

    // Creating nodes for connections.
    val bigcore_hang_SRNode                        = BundleBridgeSource[UInt](Some(() => UInt(1.W)))
    val bigcore_comp_SRNode                        = BundleBridgeSource[UInt](Some(() => UInt(3.W)))
    val debug_bp_SRNode                            = BundleBridgeSource[UInt](Some(() => UInt(2.W)))
    val ghm_ght_packet_in_SKNode                   = BundleBridgeSink[UInt](Some(() => UInt((params.width_GH_packet).W)))
    val ic_counter_SKNode                          = BundleBridgeSink[UInt](Some(() => UInt((16*GH_GlobalParams.GH_NUM_CORES).W)))
    val ghm_ght_packet_dest_SKNode                 = BundleBridgeSink[UInt](Some(() => UInt(32.W)))

    val ghm_ght_status_in_SKNode                   = BundleBridgeSink[UInt](Some(() => UInt(32.W)))

    var ghm_ghe_packet_out_SRNodes                 = Seq[BundleBridgeSource[UInt]]()
    var icsl_out_SRNodes                           = Seq[BundleBridgeSource[UInt]]()
    var ghm_ghe_status_out_SRNodes                 = Seq[BundleBridgeSource[UInt]]()
    var ghm_ghe_event_in_SKNodes                   = Seq[BundleBridgeSink[UInt]]()
    var ghm_ghe_revent_in_SKNodes                  = Seq[BundleBridgeSink[UInt]]()
    var clear_ic_status_SkNodes                    = Seq[BundleBridgeSink[UInt]]()
    var clear_ic_status_tomainSRNodes              = Seq[BundleBridgeSource[UInt]]()
    var icsl_naSRNodes                             = Seq[BundleBridgeSource[UInt]]()

    val if_agg_free_SKNode                         = BundleBridgeSink[UInt](Some(() => UInt(1.W)))

    ghm_ght_packet_in_SKNode                      := subsystem.tile_ght_packet_out_EPNode
    ic_counter_SKNode                             := subsystem.tile_ic_counter_out_EPNode
    ghm_ght_packet_dest_SKNode                    := subsystem.tile_ght_packet_dest_EPNode
    ghm_ght_status_in_SKNode                      := subsystem.tile_ght_status_out_EPNode

    if_agg_free_SKNode                            := subsystem.tile_agg_free_EPNode
    for (i <- 0 to number_of_ghes-1) {
      val ghm_ghe_packet_out_SRNode                = BundleBridgeSource[UInt]()
      ghm_ghe_packet_out_SRNodes                   = ghm_ghe_packet_out_SRNodes :+ ghm_ghe_packet_out_SRNode
      subsystem.tile_ghe_packet_in_EPNodes(i)     := ghm_ghe_packet_out_SRNodes(i)

      val icsl_out_SRNode                          = BundleBridgeSource[UInt]()
      icsl_out_SRNodes                             = icsl_out_SRNodes :+ icsl_out_SRNode
      subsystem.tile_icsl_counter_in_EPNodes(i)   := icsl_out_SRNodes(i)

      val ghm_ghe_status_out_SRNode                = BundleBridgeSource[UInt]()
      ghm_ghe_status_out_SRNodes                   = ghm_ghe_status_out_SRNodes :+ ghm_ghe_status_out_SRNode
      subsystem.tile_ghe_status_in_EPNodes(i)     := ghm_ghe_status_out_SRNodes(i)

      val ghm_ghe_event_in_SkNode                  = BundleBridgeSink[UInt]()
      ghm_ghe_event_in_SKNodes                     = ghm_ghe_event_in_SKNodes :+ ghm_ghe_event_in_SkNode
      ghm_ghe_event_in_SKNodes(i)                 := subsystem.tile_ghe_event_out_EPNodes(i)

      val ghm_ghe_revent_in_SkNode                 = BundleBridgeSink[UInt]()
      ghm_ghe_revent_in_SKNodes                    = ghm_ghe_revent_in_SKNodes :+ ghm_ghe_revent_in_SkNode
      ghm_ghe_revent_in_SKNodes(i)                := subsystem.tile_ghe_revent_out_EPNodes(i)

      val clear_ic_status_SkNode                   = BundleBridgeSink[UInt]()
      clear_ic_status_SkNodes                      = clear_ic_status_SkNodes :+ clear_ic_status_SkNode
      clear_ic_status_SkNodes(i)                  := subsystem.tile_clear_ic_status_out_EPNodes(i)

      val clear_ic_status_tomainSRNode             = BundleBridgeSource[UInt]()
      clear_ic_status_tomainSRNodes                = clear_ic_status_tomainSRNodes :+ clear_ic_status_tomainSRNode
      subsystem.clear_ic_status_tomainEPNodes(i)  := clear_ic_status_tomainSRNodes(i)

      val icsl_naSRNode                            = BundleBridgeSource[UInt]()
      icsl_naSRNodes                               = icsl_naSRNodes :+ icsl_naSRNode
      subsystem.icsl_naEPNodes(i)                 := icsl_naSRNodes(i)
    }
    subsystem.tile_bigcore_comp_EPNode            := bigcore_comp_SRNode
    subsystem.tile_bigcore_hang_EPNode            := bigcore_hang_SRNode
    subsystem.tile_debug_bp_EPNode                := debug_bp_SRNode

    val debug_gcounter_SRNode                      = BundleBridgeSource[UInt](Some(() => UInt(64.W)))
    subsystem.tile_debug_gcounter_EPNode          := debug_gcounter_SRNode
    
    val bus = subsystem.locateTLBusWrapper(where)
    val ghm = LazyModule (new GHM (GHMParams (params.number_of_little_cores, params.width_GH_packet)))

    
    InModuleBody {
      ghm.module.clock                            := bus.module.clock
      ghm.module.io.ghm_packet_in                 := ghm_ght_packet_in_SKNode.bundle
      ghm.module.io.ghm_packet_dest               := ghm_ght_packet_dest_SKNode.bundle
      ghm.module.io.ghm_status_in                 := ghm_ght_status_in_SKNode.bundle
      ghm.module.io.if_agg_free                   := if_agg_free_SKNode.bundle 
      ghm.module.io.ic_counter                    := ic_counter_SKNode.bundle

      for (i <- 0 to number_of_ghes-1) {
        if (i == 0) { // The big core
          // GHE is not connected to the big core
          ghm_ghe_packet_out_SRNodes(i).bundle    := 0.U 
          ghm_ghe_status_out_SRNodes(i).bundle    := 0.U
          clear_ic_status_tomainSRNodes(i).bundle := ghm.module.io.clear_ic_status_tomain
          icsl_naSRNodes(i).bundle                := ghm.module.io.icsl_na
          icsl_out_SRNodes(i).bundle              := 0.U
        } else {// -1 big core
          ghm_ghe_packet_out_SRNodes(i).bundle    := ghm.module.io.ghm_packet_outs(i-1)
          ghm_ghe_status_out_SRNodes(i).bundle    := ghm.module.io.ghm_status_outs(i-1)
          clear_ic_status_tomainSRNodes(i).bundle := 0.U
          icsl_naSRNodes(i).bundle                := 0.U
          icsl_out_SRNodes(i).bundle              := ghm.module.io.icsl_counter(i-1)
          ghm.module.io.ghe_event_in(i-1)         := ghm_ghe_event_in_SKNodes(i).bundle
          ghm.module.io.ghe_revent_in(i-1)        := ghm_ghe_event_in_SKNodes(i).bundle
          ghm.module.io.clear_ic_status(i-1)      := clear_ic_status_SkNodes(i).bundle
        }
      }

      bigcore_hang_SRNode.bundle                  := ghm.module.io.bigcore_hang
      bigcore_comp_SRNode.bundle                  := ghm.module.io.bigcore_comp
      debug_bp_SRNode.bundle                      := ghm.module.io.debug_bp
      debug_gcounter_SRNode.bundle                := ghm.module.io.debug_gcounter
    }
    ghm
  }
}