package freechips.rocketchip.r

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

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

  val if_correct_process = Input(UInt(1.W))
  val core_hang_up = Output(UInt(1.W))
  val checker_mode = Output(UInt(1.W))

  val arfs_merge = Input(UInt((params.xLen*2).W))
  val arfs_index = Input(UInt(8.W))
  val arfs_pidx = Input(UInt(5.W))
  val copy_arfs = Input(UInt(1.W))
  val rsu_status = Output(UInt(2.W))

  val cdc_ready = Output(UInt(1.W))
}

trait HasR_RSUSLIO extends BaseModule {
  val params: R_RSUSLParams
  val io = IO(new R_RSUSLIO(params))
}

class R_RSUSL(val params: R_RSUSLParams) extends Module with HasR_RSUSLIO {
  // Revisit: move it to the instruction counter
  val checker_mode                                = RegInit(0.U(1.W))

  val rsu_status                                  = RegInit(0.U(2.W))
  val arfs_ss                                     = Reg(Vec(params.numARFS, UInt(params.xLen.W)))
  val farfs_ss                                    = Reg(Vec(params.numARFS, UInt(params.xLen.W)))
  val pcarf_ss                                    = RegInit(0.U(40.W))
  val fcsr_ss                                     = RegInit(0.U(8.W))
  

  /* Loading snapshot from RSU Master */
  val if_RSU_packet                               = WireInit(0.U(1.W))
  val packet_valid                                = RegInit(0.U(1.W)) 
  val packet_index                                = RegInit(0.U(8.W))
  val packet_arfs                                 = RegInit(0.U(params.xLen.W))
  val packet_farfs                                = RegInit(0.U(params.xLen.W))

  if_RSU_packet                                  := Mux(io.arfs_pidx(4,2) =/= 0.U, 1.U, 0.U)
  packet_valid                                   := Mux(if_RSU_packet === 1.U, 1.U, 0.U)
  packet_arfs                                    := Mux(if_RSU_packet === 1.U, io.arfs_merge(63,0), 0.U)
  packet_farfs                                   := Mux(if_RSU_packet === 1.U, io.arfs_merge(127,64), 0.U)
  packet_index                                   := Mux(if_RSU_packet === 1.U, io.arfs_index, 0.U)

  when (packet_valid === 1.U){
    for (i <- 0 until params.numARFS){
      arfs_ss(i)                                 := Mux(packet_index === i.U, packet_arfs, arfs_ss(i))
      farfs_ss(i)                                := Mux(packet_index === i.U, packet_farfs, farfs_ss(i))
     }
      pcarf_ss                                   := Mux(packet_index === 0x20.U, packet_arfs(39,0), pcarf_ss)
      fcsr_ss                                    := Mux(packet_index === 0x20.U, packet_farfs(7,0), fcsr_ss)
      rsu_status                                 := Mux(packet_index === 0x20.U, 0x01.U, rsu_status)
  }

  /* Applying snapshot to the core */
  val apply_snapshot                              = RegInit(0.U(1.W))
  val apply_counter                               = RegInit(20.U(8.W))

  when ((io.copy_arfs === 0x01.U) && (apply_snapshot === 0.U)) {
    apply_snapshot                               := 1.U
    apply_counter                                := 0.U
  } .elsewhen (apply_snapshot === 1.U){
    apply_snapshot                               := Mux(apply_counter === 0x20.U, 0.U, 1.U)
    apply_counter                                := Mux(apply_counter === 0x20.U, 0.U, apply_counter + 1.U)
  } .otherwise {
    apply_snapshot                               := apply_snapshot
    apply_counter                                := apply_counter
  }

  // Revisit
  checker_mode                                   := Mux(apply_counter === 0x20.U, 1.U, checker_mode)
  io.checker_mode                                := checker_mode & io.if_correct_process

  io.arfs_out                                    := Mux(((apply_snapshot === 1.U) && (apply_counter =/= 0x20.U)), arfs_ss(apply_counter), 0.U)
  io.farfs_out                                   := Mux(((apply_snapshot === 1.U) && (apply_counter =/= 0x20.U)), farfs_ss(apply_counter), 0.U)
  io.arfs_idx_out                                := Mux(((apply_snapshot === 1.U) && (apply_counter =/= 0x20.U)), apply_counter, 0.U)
  io.arfs_valid_out                              := Mux(((apply_snapshot === 1.U) && (apply_counter =/= 0x20.U)), 1.U, 0.U)

  io.pcarf_out                                   := pcarf_ss
  io.fcsr_out                                    := Mux(((apply_snapshot === 1.U) && (apply_counter === 0x20.U)), fcsr_ss, 0.U)
  io.pfarf_valid_out                             := Mux(((apply_snapshot === 1.U) && (apply_counter === 0x20.U)), 1.U, 0.U)

  io.cdc_ready                                   := packet_valid
  io.core_hang_up                                := apply_snapshot

  io.rsu_status                                  := rsu_status
}