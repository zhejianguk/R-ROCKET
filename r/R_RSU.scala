package freechips.rocketchip.r

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//===== GuardianCouncil Function: Start ====//
import freechips.rocketchip.guardiancouncil._
//===== GuardianCouncil Function: End   ====//


case class R_RSUParams(
  xLen: Int,
  numARFS: Int,
  scalarWidth: Int
)

class R_RSUIO(params: R_RSUParams) extends Bundle {
  val arfs_in = Input(Vec(params.numARFS, UInt(params.xLen.W)))
  val farfs_in = Input(Vec(params.numARFS, UInt(params.xLen.W)))
  val pcarf_in = Input(UInt(40.W))
  val fcsr_in = Input(UInt(8.W))
  val snapshot = Input(UInt(1.W))
  val merge = Input(UInt(1.W))
  val ght_filters_ready = Input(UInt(1.W))
  val core_hang_up = Output(UInt(1.W))
  val rsu_merging = Output(UInt(1.W))
  val arfs_merge = Output(Vec(params.scalarWidth, UInt((params.xLen*2).W)))
  val arfs_index = Output(Vec(params.scalarWidth, UInt(8.W)))
  val ic_crnt_target = Input(UInt(5.W))
  val arfs_pidx = Output(Vec(params.scalarWidth, UInt(8.W)))
  val rsu_busy = Output(UInt(1.W))
  val core_trace =Input(UInt(1.W))
}

trait HasR_RSUIO extends BaseModule {
  val params: R_RSUParams
  val io = IO(new R_RSUIO(params))
}

class R_RSU(val params: R_RSUParams) extends Module with HasR_RSUIO {
  val pcarf_ss                                    = RegInit(0.U(40.W))
  val arfs_ss                                     = Reg(Vec(params.numARFS, UInt(params.xLen.W)))
  val farfs_ss                                    = Reg(Vec(params.numARFS, UInt(params.xLen.W)))
  val fcsr_ss                                     = RegInit(0.U(8.W))

  val merging                                     = RegInit(0.U(1.W))
  val merge_counter                               = RegInit(0.U(8.W)) // 32/4 + 1 (PC) = 9 Packets in total
  val doSnapshot                                  = RegInit(0.U(1.W))
  val doMerge                                     = RegInit(0.U(1.W))
  val io_merge_delay1                             = RegInit(0.U(1.W))
  val io_merge_delay2                             = RegInit(0.U(1.W))

  doSnapshot                                     := io.snapshot
  io_merge_delay1                                := io.merge
  io_merge_delay2                                := io_merge_delay1
  doMerge                                        := io_merge_delay2

  
  when (doSnapshot === 1.U) {
      for (i <- 0 until params.numARFS) {
        arfs_ss(i)                               := io.arfs_in(i)
        farfs_ss(i)                              := io.farfs_in(i)
      }
      pcarf_ss                                   := io.pcarf_in
      fcsr_ss                                    := io.fcsr_in
  }

  when ((doMerge === 1.U) && (merging === 0.U)){
    merging                                      := 1.U
    merge_counter                                := 0.U
  } .elsewhen ((merging === 1.U) && (io.ght_filters_ready === 1.U)) {
    merging                                      := Mux (merge_counter === 8.U, 0.U, 1.U)
    merge_counter                                := Mux (merge_counter === 8.U, 0.U, merge_counter + 1.U)
  } .otherwise {
    merging                                      := merging
    merge_counter                                := merge_counter
  }

  io.core_hang_up                                := io.snapshot|doSnapshot

  val zeros_24bits                                = WireInit(0.U(24.W))
  val zeros_56bits                                = WireInit(0.U(56.W))
  val seven_3bits                                 = WireInit(7.U(3.W))

  for (w <- 0 until params.scalarWidth) {
    if (w == 0) {
      io.arfs_merge(w)                             := MuxCase(0.U,
                                                      Array(((merging === 1.U) && (merge_counter =/= 8.U)) -> Cat(farfs_ss(merge_counter<<2), arfs_ss(merge_counter<<2)),
                                                            ((merging === 1.U) && (merge_counter === 8.U)) -> Cat(zeros_56bits, fcsr_ss, zeros_24bits, pcarf_ss)
                                                          )
                                                          )

      io.arfs_index(w)                             := Mux((merging === 1.U), (merge_counter<<2), 0.U)
      io.arfs_pidx(w)                              := Mux((merging === 1.U), Cat(io.ic_crnt_target(4,0), seven_3bits), 0.U)
    } else {
      io.arfs_merge(w)                             := MuxCase(0.U,
                                                      Array(((merging === 1.U) && (merge_counter =/= 8.U)) -> Cat(farfs_ss((merge_counter<<2)+w.U), arfs_ss((merge_counter<<2)+w.U)),
                                                            ((merging === 1.U) && (merge_counter === 8.U)) -> 0.U
                                                          )
                                                          )

      io.arfs_index(w)                             := Mux(((merging === 1.U) && (merge_counter =/= 8.U)), ((merge_counter<<2) + w.U), 0.U)
      io.arfs_pidx(w)                              := Mux(((merging === 1.U) && (merge_counter =/= 8.U)), Cat(io.ic_crnt_target(4,0), seven_3bits), 0.U)
    }
  }
  
  io.rsu_merging                                   := merging
  io.rsu_busy                                      := Mux(io.snapshot.asBool || io.merge.asBool || io_merge_delay1.asBool || io_merge_delay2.asBool || doSnapshot.asBool || doMerge.asBool || merging.asBool, 1.U, 0.U)

  /*
  if (GH_GlobalParams.GH_DEBUG == 1) {
    when ((doSnapshot === 1.U) && (io.core_trace.asBool)) {
      printf(midas.targetutils.SynthesizePrintf("[CHECK POINTS --- Boom]: ARFS = [%x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x    %x]\n", 
      io.arfs_in(0), io.arfs_in(1), io.arfs_in(2), io.arfs_in(3),io.arfs_in(4), io.arfs_in(5), io.arfs_in(6), io.arfs_in(7),
      io.arfs_in(8), io.arfs_in(9), io.arfs_in(10), io.arfs_in(11),io.arfs_in(12), io.arfs_in(13), io.arfs_in(14), io.arfs_in(15),
      io.arfs_in(16), io.arfs_in(17), io.arfs_in(18), io.arfs_in(19),io.arfs_in(20), io.arfs_in(21), io.arfs_in(22), io.arfs_in(23),
      io.arfs_in(24), io.arfs_in(25), io.arfs_in(26), io.arfs_in(27),io.arfs_in(28), io.arfs_in(29), io.arfs_in(30), io.arfs_in(31)))
    }
  }
  */ 
}