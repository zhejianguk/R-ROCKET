package freechips.rocketchip.r

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}
import freechips.rocketchip.guardiancouncil._

case class R_RSUSLParams(
  xLen: Int,
  numARFS: Int
)

class R_RSUSLIO(params: R_RSUSLParams) extends Bundle {
  val arfs_out = Output(UInt(params.xLen.W))
  val farfs_out = Output(UInt(params.xLen.W))
  val arfs_idx_out = Output(UInt(8.W))
  val arfs_valid_out = Output(UInt(1.W))

  val pcarf_out = Output(UInt(40.W))
  val fcsr_out = Output(UInt(8.W))
  val pfarf_valid_out = Output(UInt(1.W))
  val core_hang_up = Output(UInt(1.W))

  val arfs_merge = Input(UInt((params.xLen*2).W))
  val arfs_index = Input(UInt(8.W))
  val arfs_if_ARFS = Input(UInt(1.W))
  val arfs_if_CPS = Input(UInt(1.W))
  val paste_arfs = Input(UInt(1.W))
  val rsu_status = Output(UInt(2.W))
  val clear_ic_status = Input(UInt(1.W))

  val cdc_ready = Output(UInt(1.W))

  val do_cp_check = Input(UInt(1.W))
  val if_cp_check_completed = Output(UInt(1.W))
  val core_arfs_in = Input(Vec(params.numARFS, UInt(params.xLen.W)))
  val core_farfs_in = Input(Vec(params.numARFS, UInt(params.xLen.W)))
  val elu_cp_deq = Input(UInt(1.W))
  val elu_cp_data = Output(UInt((4*params.xLen+8).W))
  val elu_status = Output(UInt(1.W))

  val core_trace = Input(UInt(1.W))
  val record_context = Input(UInt(1.W))
  val store_from_checker = Input(UInt(1.W)) // 0: from main; 1: from checker.
}

trait HasR_RSUSLIO extends BaseModule {
  val params: R_RSUSLParams
  val io = IO(new R_RSUSLIO(params))
}

class R_RSUSL(val params: R_RSUSLParams) extends Module with HasR_RSUSLIO {
  // Revisit: move it to the instruction counter
  val rsu_status                                  = RegInit(0.U(2.W))

  /* Loading snapshot from RSU Master */
  val arfs_ss                                     = SyncReadMem(params.numARFS+1, UInt(params.xLen.W))
  val farfs_ss                                    = SyncReadMem(params.numARFS+1, UInt(params.xLen.W))
  val arfs_ss_ECP                                 = SyncReadMem(params.numARFS+1, UInt(params.xLen.W))
  val farfs_ss_ECP                                = SyncReadMem(params.numARFS+1, UInt(params.xLen.W))
  val arfs_ss_GMode                               = SyncReadMem(params.numARFS+1, UInt(params.xLen.W))
  val farfs_ss_GMode                              = SyncReadMem(params.numARFS+1, UInt(params.xLen.W))

  val pcarfs_ss                                   = RegInit(0.U(40.W))

  val if_RSU_packet                               = WireInit(0.U(1.W))
  val packet_valid                                = RegInit(0.U(1.W)) 
  val packet_index                                = RegInit(0.U(8.W))
  val packet_arfs                                 = RegInit(0.U(params.xLen.W))
  val packet_farfs                                = RegInit(0.U(params.xLen.W))

  if_RSU_packet                                  := Mux(io.arfs_if_ARFS.asBool && io.arfs_if_CPS.asBool, 1.U, 0.U) 
  packet_valid                                   := Mux(if_RSU_packet === 1.U, 1.U, 0.U)
  packet_arfs                                    := Mux(if_RSU_packet === 1.U, io.arfs_merge(63,0), 0.U)
  packet_farfs                                   := Mux(if_RSU_packet === 1.U, io.arfs_merge(127,64), 0.U)
  packet_index                                   := Mux(if_RSU_packet === 1.U, io.arfs_index, 0.U)

  /* Storing the checker's current states */
  val recording_context                           = Reg(Bool())
  val recording_counter                           = RegInit(20.U(7.W))



  when (io.record_context.asBool && !recording_context) {
    recording_context                            := true.B
    recording_counter                            := 0.U
  } .elsewhen (recording_context === 1.U){
    recording_context                            := Mux(recording_counter === 0x20.U, false.B, true.B)
    recording_counter                            := Mux(recording_counter === 0x20.U, 0.U, recording_counter + 1.U)
  } .otherwise {
    recording_context                            := recording_context
    recording_counter                            := recording_counter
  }

  when (recording_context) {
    arfs_ss_GMode.write(recording_counter, io.core_arfs_in(recording_counter))
    farfs_ss_GMode.write(recording_counter, io.core_farfs_in(recording_counter))
  }

  /* Loading snapshot at End of Check Point from RSU Master */
  val if_RSU_packet_ECP                           = WireInit(0.U(1.W))
  val packet_valid_ECP                            = RegInit(0.U(1.W)) 
  val packet_index_ECP                            = RegInit(0.U(8.W))
  val packet_arfs_ECP                             = RegInit(0.U(params.xLen.W))
  val packet_farfs_ECP                            = RegInit(0.U(params.xLen.W))
  
  if_RSU_packet_ECP                              := Mux(io.arfs_if_ARFS.asBool && !io.arfs_if_CPS.asBool, 1.U, 0.U) 
  packet_valid_ECP                               := Mux(if_RSU_packet_ECP === 1.U, 1.U, 0.U)
  packet_arfs_ECP                                := Mux(if_RSU_packet_ECP === 1.U, io.arfs_merge(63,0), 0.U)
  packet_farfs_ECP                               := Mux(if_RSU_packet_ECP === 1.U, io.arfs_merge(127,64), 0.U)
  packet_index_ECP                               := Mux(if_RSU_packet_ECP === 1.U, io.arfs_index, 0.U)

  when (packet_valid === 1.U) {
    arfs_ss.write(packet_index, packet_arfs)
    farfs_ss.write(packet_index, packet_farfs)
    /*
    if (GH_GlobalParams.GH_DEBUG == 1) { 
      when (io.core_trace.asBool){
        printf(midas.targetutils.SynthesizePrintf("PACKET_CPS: [Index = %d] [Packet_arfs = %x], [Packet_farfs = %x]. \n", 
        packet_index, packet_arfs, packet_farfs))
      }
    }
    */
  } 
  
  when (packet_valid_ECP === 1.U) {
    arfs_ss_ECP.write(packet_index_ECP, packet_arfs_ECP)
    farfs_ss_ECP.write(packet_index_ECP, packet_farfs_ECP)
    /*
    if (GH_GlobalParams.GH_DEBUG == 1) { 
      when (io.core_trace.asBool){
        printf(midas.targetutils.SynthesizePrintf("PACKET_CPE: [Index = %d] [Packet_arfs = %x], [Packet_farfs = %x]. \n", 
        packet_index_ECP, packet_arfs_ECP, packet_farfs_ECP))
      }
    }
    */
  }

  pcarfs_ss                                      := Mux(packet_valid.asBool && (packet_index === 0x20.U), packet_arfs(39,0), pcarfs_ss)
  rsu_status                                     := Mux(io.clear_ic_status.asBool, 0.U, Mux(packet_index === 0x20.U, 1.U, Mux(packet_index_ECP === 0x20.U, 3.U, rsu_status)))

  /* Applying snapshot to the core */
  val arf_data                                    = WireInit(0.U((params.xLen.W)))
  val farf_data                                   = WireInit(0.U((params.xLen.W)))
  val arf_addr                                    = WireInit(0.U(8.W))
  val farf_addr                                   = WireInit(0.U(8.W))
  
  val arf_data_ECP                                = WireInit(0.U((params.xLen.W)))
  val farf_data_ECP                               = WireInit(0.U((params.xLen.W)))
  val arf_addr_ECP                                = WireInit(0.U(8.W))
  val farf_addr_ECP                               = WireInit(0.U(8.W))

  val apply_snapshot                              = RegInit(0.U(1.W))
  val apply_snapshot_memdelay                     = RegInit(0.U(1.W))
  val apply_counter                               = RegInit(20.U(8.W))
  val apply_counter_memdelay                      = RegInit(0.U(8.W))
  val do_check                                    = RegInit(0.U(1.W))
  val checking_counter                            = RegInit(0.U(8.W))


  apply_snapshot_memdelay                        := apply_snapshot
  apply_counter_memdelay                         := apply_counter
  arf_addr                                       := Mux(apply_snapshot.asBool, apply_counter, 0.U)
  farf_addr                                      := Mux(apply_snapshot.asBool, apply_counter, 0.U)
  arf_data                                       := Mux(!io.store_from_checker, arfs_ss.read(arf_addr, apply_snapshot.asBool), arfs_ss_GMode.read(arf_addr, apply_snapshot.asBool))
  farf_data                                      := Mux(!io.store_from_checker, farfs_ss.read(farf_addr, apply_snapshot.asBool), farfs_ss_GMode.read(arf_addr, apply_snapshot.asBool))

  arf_addr_ECP                                   := Mux(do_check.asBool, checking_counter, 0.U)
  farf_addr_ECP                                  := Mux(do_check.asBool, checking_counter, 0.U)
  arf_data_ECP                                   := arfs_ss_ECP.read(arf_addr_ECP,  do_check.asBool)
  farf_data_ECP                                  := farfs_ss_ECP.read(farf_addr_ECP, do_check.asBool)


  when ((io.paste_arfs === 0x01.U) && (apply_snapshot === 0.U)) {
    apply_snapshot                               := 1.U
    apply_counter                                := 0.U
  } .elsewhen (apply_snapshot === 1.U){
    apply_snapshot                               := Mux(apply_counter === 0x20.U, 0.U, 1.U)
    apply_counter                                := Mux(apply_counter === 0x20.U, 0.U, apply_counter + 1.U)
  } .otherwise {
    apply_snapshot                               := apply_snapshot
    apply_counter                                := apply_counter
  }

  val arfs_out_printf                             = Mux(((apply_snapshot_memdelay === 1.U) && (apply_counter_memdelay =/= 0x20.U)), arf_data, 0.U)
  val arfs_out_valid_printf                       = Mux(((apply_snapshot_memdelay === 1.U) && (apply_counter_memdelay =/= 0x20.U)), 1.U, 0.U)
  val arfs_out_idx                                = Mux(((apply_snapshot_memdelay === 1.U) && (apply_counter_memdelay =/= 0x20.U)), apply_counter_memdelay, 0.U)

  /*
  if (GH_GlobalParams.GH_DEBUG == 1) {
    when ((arfs_out_valid_printf.asBool) && (io.core_trace.asBool)) {
      printf(midas.targetutils.SynthesizePrintf("[CHECK POINTS --- Checker]: ARFS[%d] = [%x]\n", 
      arfs_out_idx, arfs_out_printf))
    }
  }
  */

  io.arfs_out                                    := Mux(((apply_snapshot_memdelay === 1.U) && (apply_counter_memdelay =/= 0x20.U)), arf_data, 0.U)
  io.farfs_out                                   := Mux(((apply_snapshot_memdelay === 1.U) && (apply_counter_memdelay =/= 0x20.U)), farf_data, 0.U)
  io.arfs_idx_out                                := Mux(((apply_snapshot_memdelay === 1.U) && (apply_counter_memdelay =/= 0x20.U)), apply_counter_memdelay, 0.U)
  io.arfs_valid_out                              := Mux(((apply_snapshot_memdelay === 1.U) && (apply_counter_memdelay =/= 0x20.U)), 1.U, 0.U)

  val pcarfs_ss_delay                             = RegInit(0.U(40.W))
  pcarfs_ss_delay                                := pcarfs_ss

  if (GH_GlobalParams.GH_DEBUG == 1) {
    when ((io.core_trace.asBool) && (pcarfs_ss_delay =/= pcarfs_ss)) {
      printf(midas.targetutils.SynthesizePrintf("[CP-T-PC] = [%x]\n", pcarfs_ss))
    }
  }
 
  io.pcarf_out                                   := pcarfs_ss
  io.fcsr_out                                    := Mux(((apply_snapshot_memdelay === 1.U) && (apply_counter_memdelay === 0x20.U)), farf_data, 0.U)
  io.pfarf_valid_out                             := Mux(((apply_snapshot_memdelay === 1.U) && (apply_counter_memdelay === 0x20.U)), 1.U, 0.U)
  io.cdc_ready                                   := packet_valid | packet_valid_ECP

  io.rsu_status                                  := rsu_status

  val u_channel                                   = Module (new GH_MemFIFO(FIFOParams((4*params.xLen+8), 4)))
  val channel_enq_valid                           = WireInit(false.B)
  val channel_enq_data                            = WireInit(0.U((4*params.xLen+8).W))
  val channel_deq_ready                           = WireInit(false.B)
  val channel_deq_data                            = WireInit(0.U((4*params.xLen+8).W))
  val channel_empty                               = WireInit(true.B)
  val channel_full                                = WireInit(false.B)

  u_channel.io.enq_valid                         := channel_enq_valid
  u_channel.io.enq_bits                          := channel_enq_data
  u_channel.io.deq_ready                         := channel_deq_ready
  channel_deq_data                               := u_channel.io.deq_bits
  channel_empty                                  := u_channel.io.empty
  channel_full                                   := u_channel.io.full

  val checking_counter_memdelay                   = RegInit(0.U(8.W))
  checking_counter_memdelay                      := checking_counter
  val if_check_completed                          = WireInit(0.U(1.W))

  if_check_completed                             := (checking_counter_memdelay === 0x19.U).asUInt
  io.if_cp_check_completed                       := if_check_completed

  when (!do_check.asBool) {
    do_check                                     := Mux(io.do_cp_check.asBool && !if_check_completed.asBool, 1.U, 0.U)
    checking_counter                             := Mux(io.clear_ic_status.asBool, 0.U, checking_counter)
  } .otherwise {
    do_check                                     := Mux(if_check_completed.asBool, 0.U, 1.U)
    checking_counter                             := Mux(checking_counter === 0x19.U, checking_counter, checking_counter + 1.U)
  }

  channel_enq_valid                              := do_check.asBool && (checking_counter =/= 0.U) && !if_check_completed.asBool && ((io.core_arfs_in(checking_counter_memdelay) =/= arf_data_ECP) ||  (io.core_farfs_in(checking_counter_memdelay) =/= farf_data_ECP))
  channel_enq_data                               := Mux(channel_enq_valid.asBool, Cat(checking_counter_memdelay, farf_data_ECP, io.core_farfs_in(checking_counter_memdelay), arf_data_ECP, io.core_arfs_in(checking_counter_memdelay)), 0.U)
  channel_deq_ready                              := io.elu_cp_deq.asBool
  io.elu_cp_data                                 := channel_deq_data
  io.elu_status                                  := ~channel_empty
  io.core_hang_up                                := apply_snapshot | apply_snapshot_memdelay | io.record_context | recording_context | (do_check.asBool && !if_check_completed.asBool)
  if (GH_GlobalParams.GH_DEBUG == 1) {
    when (channel_enq_valid && (io.core_trace.asBool)) {
        val print_farf_data                       = WireInit(0.U((params.xLen).W))
        val print_arf_data                        = WireInit(0.U((params.xLen).W))
        print_farf_data                          := io.core_farfs_in(checking_counter_memdelay)
        print_arf_data                           := io.core_arfs_in(checking_counter_memdelay)

        printf(midas.targetutils.SynthesizePrintf("ELU_ARF: an error is detected! [ARF_ID = %d] [farf_data_ECP = %x], [farf_data = %x], [arf_data_ECP = %x], [arf_data = %x]. \n", 
        checking_counter_memdelay, farf_data_ECP, print_farf_data, arf_data_ECP, print_arf_data))
    }
  }
}