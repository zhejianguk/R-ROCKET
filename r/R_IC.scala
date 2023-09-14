package freechips.rocketchip.r

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}
import freechips.rocketchip.guardiancouncil._

case class R_ICParams(
  totalnumber_of_cores: Int,
  width_of_ic: Int
)

// Revisit: add check of the correct_process!
class R_ICIO(params: R_ICParams) extends Bundle {
  val ic_run_isax                                = Input(UInt(1.W))
  val ic_exit_isax                               = Input(UInt(1.W))
  val ic_syscall                                 = Input(UInt(1.W))
  val ic_syscall_back                            = Input(UInt(1.W))
  val rsu_busy                                   = Input(UInt(1.W))

  val ic_threshold                               = Input(UInt((params.width_of_ic-1).W))
  val icsl_na                                    = Input(Vec(params.totalnumber_of_cores, UInt(1.W)))
  val ic_incr                                    = Input(UInt((3.W)))
  val if_ready_snap_shot                         = Input(UInt((1.W)))

  val crnt_target                                = Output(UInt(5.W))
  val if_filtering                               = Output(UInt(1.W)) // 1: filtering; 0: non-filtering
  val if_pipeline_stall                          = Output(UInt(1.W))
  val if_dosnap                                  = Output(UInt(1.W))

  val ic_counter                                 = Output(Vec(params.totalnumber_of_cores, UInt(params.width_of_ic.W)))
  val ic_status                                  = Output(Vec(params.totalnumber_of_cores, UInt(1.W)))
  val clear_ic_status                            = Input(Vec(params.totalnumber_of_cores, UInt(1.W)))
  val if_correct_process                         = Input(UInt((1.W)))
  val num_of_checker                             = Input(UInt((8.W)))
  val changing_num_of_checker                    = Input(UInt((1.W)))
}

trait HasR_ICIO extends BaseModule {
  val params: R_ICParams
  val io = IO(new R_ICIO(params))
}

class R_IC (val params: R_ICParams) extends Module with HasR_ICIO {
  val crnt_target                                = RegInit(0.U(3.W))
  val crnt_mask                                  = RegInit(0.U(5.W))
  val nxt_target                                 = RegInit(0.U(3.W))
  val ctrl                                       = RegInit(0.U(2.W))

  val if_filtering                               = WireInit(0.U(1.W))
  val if_pipeline_stall                          = WireInit(0.U(1.W))
  val if_dosnap                                  = WireInit(0.U(1.W))
  val sch_reset                                  = WireInit(0.U(1.W))

  val ic_counter                                 = RegInit(VecInit(Seq.fill(params.totalnumber_of_cores)(0.U(params.width_of_ic.W))))
  val ic_status                                  = RegInit(VecInit(Seq.fill(params.totalnumber_of_cores)(0.U(1.W)))) // 0: idle; 1: running
  val clear_ic_status                            = WireInit(VecInit(Seq.fill(params.totalnumber_of_cores)(0.U(1.W)))) // 0: idle; 1: running

  // FPS scheduler
  val sch_result                                = WireInit(0.U(5.W))
  val u_sch_fp                                  = Module (new GHT_SCH_FP(GHT_SCH_Params (params.totalnumber_of_cores-1)))
  u_sch_fp.io.core_s                           := 1.U
  u_sch_fp.io.core_e                           := io.num_of_checker
  u_sch_fp.io.inst_c                           := 1.U // Holding the scheduling results

  for (i <- 1 to params.totalnumber_of_cores - 1) {
    u_sch_fp.io.core_na(i-1)                   := ic_status(i)
  }
  sch_result                                   := u_sch_fp.io.sch_dest
  u_sch_fp.io.rst_sch                          := sch_reset


  // FSM to control the R_IC
  val fsm_reset :: fsm_presch :: fsm_sch :: fsm_cooling :: fsm_snap :: fsm_trans :: fsm_check :: fsm_postcheck :: Nil = Enum(8)
  val fsm_state                                 = RegInit(fsm_reset)
  val fsm_ini                                   = RegInit(0.U(1.W))
  val cooling_counter                           = RegInit(0.U(4.W))
  val exception                                 = RegInit(0.U(1.W))
  val cooling_threshold                         = 5.U
  cooling_counter                              := Mux((fsm_state =/= fsm_cooling), 0.U, Mux(cooling_counter < cooling_threshold, cooling_counter + 1.U, cooling_counter))
  val if_cooled                                 = Mux((cooling_counter >= cooling_threshold) && !io.rsu_busy.asBool, true.B, false.B)
  sch_reset                                    := Mux((fsm_state === fsm_reset) || (io.changing_num_of_checker.asBool), 1.U, 0.U)
  // if_dosnap                                    := Mux(((fsm_state === fsm_snap) && (io.if_ready_snap_shot.asBool) && (io.if_correct_process.asBool)), 1.U, 0.U)
  if_dosnap                                    := Mux((fsm_state === fsm_snap) && (io.if_ready_snap_shot.asBool), Mux(ctrl === 2.U, io.if_correct_process, 1.U), 0.U)
  val if_t_and_na                               = Mux(((io.ic_exit_isax.asBool || io.ic_syscall.asBool || (ic_counter(crnt_target) >= io.ic_threshold) || io.icsl_na(crnt_target).asBool) && (ic_status(sch_result).asBool)), 1.U, 0.U)
  val if_t_and_a                                = Mux(((io.ic_exit_isax.asBool || io.ic_syscall.asBool || (ic_counter(crnt_target) >= io.ic_threshold) || io.icsl_na(crnt_target).asBool) && (!ic_status(sch_result).asBool)), 1.U, 0.U)
  fsm_ini                                      := Mux(fsm_state === fsm_reset, 1.U, Mux(fsm_state === fsm_presch, fsm_ini, 0.U))


  switch (fsm_state) {
    is (fsm_reset){ // 000
      ctrl                                      := 2.U
      crnt_target                               := 0.U
      crnt_mask                                 := 0.U
      nxt_target                                := 0.U
      if_filtering                              := 0.U
      if_pipeline_stall                         := 0.U
      exception                                 := 0.U
      for (i <- 0 to params.totalnumber_of_cores - 1) {
        ic_status(i)                            := Mux(clear_ic_status(i).asBool, 0.U, ic_status(i))
        ic_counter(i)                           := Mux(clear_ic_status(i).asBool, 0.U, ic_counter(i))
      }
      fsm_state                                 := fsm_presch
    }

    is (fsm_presch){ // 001
      ctrl                                      := 2.U
      crnt_target                               := crnt_target
      crnt_mask                                 := crnt_mask
      nxt_target                                := nxt_target
      if_filtering                              := 0.U
      if_pipeline_stall                         := Mux(fsm_ini.asBool, 0.U, Mux(exception.asBool, 0.U, 1.U))
      exception                                 := Mux(exception.asBool && io.ic_syscall_back.asBool, 0.U, exception)
      for (i <- 0 to params.totalnumber_of_cores - 1) {
        ic_status(i)                            := Mux(clear_ic_status(i).asBool, 0.U, ic_status(i))
        ic_counter(i)                           := Mux(clear_ic_status(i).asBool, 0.U, ic_counter(i))
      }
      fsm_state                                 := Mux(fsm_ini.asBool, Mux(io.ic_run_isax.asBool, fsm_sch, fsm_presch), Mux(exception.asBool && !io.ic_syscall_back.asBool, fsm_presch, fsm_sch))
    }

    is (fsm_sch){ // 010
      ctrl                                      := ctrl
      crnt_target                               := crnt_target
      crnt_mask                                 := crnt_mask
      nxt_target                                := Mux(!ic_status(sch_result).asBool, sch_result, nxt_target)
      if_filtering                              := 0.U
      if_pipeline_stall                         := 1.U
      exception                                 := exception
      for (i <- 0 to params.totalnumber_of_cores - 1) {
        ic_status(i)                            := Mux(clear_ic_status(i).asBool, 0.U, ic_status(i))
        ic_counter(i)                           := Mux(clear_ic_status(i).asBool, 0.U, ic_counter(i))
      }
      fsm_state                                 := Mux(!ic_status(sch_result).asBool, fsm_cooling, fsm_sch)
    }

    is (fsm_cooling){ // 011
      ctrl                                      := ctrl
      crnt_target                               := Mux(if_cooled, nxt_target, crnt_target)
      crnt_mask                                 := crnt_mask
      nxt_target                                := nxt_target
      if_filtering                              := 0.U
      if_pipeline_stall                         := 1.U
      exception                                 := exception
      for (i <- 0 to params.totalnumber_of_cores - 1) {
        ic_status(i)                            := Mux(clear_ic_status(i).asBool, 0.U, ic_status(i))
        ic_counter(i)                           := Mux(clear_ic_status(i).asBool, 0.U, ic_counter(i))
      }
      fsm_state                                 := Mux(if_cooled, fsm_snap, fsm_cooling)
    }

    is (fsm_snap){ // 100
      ctrl                                      := ctrl
      crnt_target                               := crnt_target
      crnt_mask                                 := Cat(ctrl, crnt_target)
      nxt_target                                := nxt_target
      if_filtering                              := 0.U
      // if_pipeline_stall                         := Mux(io.if_correct_process.asBool, 1.U, 0.U)
      if_pipeline_stall                         := Mux(ctrl === 2.U, io.if_correct_process.asBool, 1.U)
      exception                                 := exception
      for (i <- 0 to params.totalnumber_of_cores - 1) {
        ic_status(i)                            := Mux(clear_ic_status(i).asBool, 0.U, Mux((crnt_target === i.U) && (ctrl(0) === 0.U), 1.U, ic_status(i)))
        ic_counter(i)                           := Mux(clear_ic_status(i).asBool, 0.U, ic_counter(i))
      }
      fsm_state                                 := Mux(ctrl === 2.U, Mux(io.if_ready_snap_shot.asBool && io.if_correct_process.asBool, fsm_trans, fsm_snap), Mux(io.if_ready_snap_shot.asBool, fsm_trans, fsm_snap))      
    }

    is (fsm_trans){ // 101 Do we really need a signal to transmit the snapshot? 
      ctrl                                      := ctrl
      crnt_target                               := crnt_target
      crnt_mask                                 := crnt_mask
      nxt_target                                := nxt_target
      if_filtering                              := 0.U
      if_pipeline_stall                         := 1.U
      exception                                 := exception
      for (i <- 0 to params.totalnumber_of_cores - 1) {
        ic_status(i)                            := Mux(clear_ic_status(i).asBool, 0.U, ic_status(i))
        ic_counter(i)                           := Mux(clear_ic_status(i).asBool, 0.U, ic_counter(i))
      }
      fsm_state                                 := Mux(ctrl === 3.U, Mux(!io.rsu_busy.asBool, fsm_reset, fsm_trans), Mux(ctrl === 1.U, fsm_presch, fsm_check))
    }

    is (fsm_check){ // 110
      ctrl                                      := Mux(io.ic_exit_isax.asBool, 3.U, Mux(io.ic_syscall.asBool || if_t_and_na.asBool, 1.U, Mux(if_t_and_a.asBool, 0.U, ctrl)))
      crnt_target                               := crnt_target
      crnt_mask                                 := crnt_mask
      nxt_target                                := nxt_target 
      if_filtering                              := Mux(io.ic_exit_isax.asBool || io.ic_syscall.asBool || (ic_counter(crnt_target) >= io.ic_threshold) || io.icsl_na(crnt_target).asBool, 0.U, 1.U)
      if_pipeline_stall                         := Mux(io.ic_exit_isax.asBool || io.ic_syscall.asBool || (ic_counter(crnt_target) >= io.ic_threshold) || io.icsl_na(crnt_target).asBool, 1.U, 0.U)
      exception                                 := Mux(io.ic_syscall.asBool, 1.U, exception)
      for (i <- 0 to params.totalnumber_of_cores - 1) {
        ic_status(i)                            := Mux(clear_ic_status(i).asBool, 0.U,  ic_status(i))
        ic_counter(i)                           := Mux(clear_ic_status(i).asBool, 0.U,  Mux((crnt_target === i.U) && (io.if_correct_process.asBool) && (!io.ic_syscall.asBool), (ic_counter(i) + io.ic_incr), ic_counter(i)))
      }
      fsm_state                                 := Mux(io.ic_exit_isax.asBool || io.ic_syscall.asBool || (ic_counter(crnt_target) >= io.ic_threshold) || io.icsl_na(crnt_target).asBool, fsm_postcheck, fsm_check)
    }

    is (fsm_postcheck){ // 111
      ctrl                                      := ctrl
      crnt_target                               := crnt_target
      crnt_mask                                 := crnt_mask
      nxt_target                                := nxt_target 
      if_filtering                              := 0.U
      if_pipeline_stall                         := 1.U
      exception                                 := exception
      for (i <- 0 to params.totalnumber_of_cores - 1) {
        ic_status(i)                            := Mux(clear_ic_status(i).asBool, 0.U,  ic_status(i))
        ic_counter(i)                           := Mux(clear_ic_status(i).asBool, 0.U,  Mux((crnt_target === i.U), (ic_counter(i) | 0x8000.U), ic_counter(i)))
      }
      fsm_state                                 := Mux(ctrl === 0.U, fsm_sch, fsm_cooling)
    }
  }

  

  if (GH_GlobalParams.GH_DEBUG == 1) {
    when ((io.ic_incr =/= 0.U) && (fsm_state === fsm_check)) {
      printf(midas.targetutils.SynthesizePrintf("sl_counter=[%x]\n", (ic_counter(crnt_target)+io.ic_incr)))
    }

    val fsm_state_delay                          = RegInit(fsm_reset)
    fsm_state_delay                              := fsm_state
    when (fsm_state_delay =/= fsm_state) {
      printf(midas.targetutils.SynthesizePrintf("fsm_state=[%x]\n", fsm_state))
    }
  }


  // Manage ic_status and ic_counter
  /* 
  for(i <- 0 to params.totalnumber_of_cores - 1) {
    ic_status(i)                                <= MuxCase(ic_status(i),
                                                     Array((clear_ic_status(i).asBool) -> 0.U,
                                                           ((crnt_target === i.U) && (fsm_state === fsm_snapshot)) -> 1.U
                                                          )
                                                          )

    ic_counter(i)                              <= MuxCase(ic_counter(i),
                                                     Array((clear_ic_status(i).asBool) -> 0.U,
                                                           (io.if_correct_process.asBool && (crnt_target === i.U) && (fsm_state === fsm_checking)) -> (ic_counter(i) + io.ic_incr),
                                                           ((crnt_target === i.U) && (fsm_state === fsm_postchecking)) -> (ic_counter(i) | 0x8000.U)
                                                          )
                                                          )                                   
  }
  */


  io.crnt_target                                := crnt_mask
  io.if_filtering                               := if_filtering & io.if_correct_process // Not used yet
  io.if_pipeline_stall                          := if_pipeline_stall
  io.if_dosnap                                  := if_dosnap

  io.ic_counter                                 := ic_counter
  io.ic_status                                  := ic_status
  for (i <- 0 to params.totalnumber_of_cores - 1){
    clear_ic_status(i)                          := io.clear_ic_status(i)
  }

}