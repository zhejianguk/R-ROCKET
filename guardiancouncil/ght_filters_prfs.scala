package freechips.rocketchip.guardiancouncil

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}

//==========================================================
// Parameters
//==========================================================
case class GHT_FILTERS_PRFS_Params(
  xlen: Int,
  packet_size: Int,
  core_width: Int,
  use_prfs: Boolean
)

//==========================================================
// I/Os
//==========================================================
class GHT_FILTERS_PRFS_IO (params: GHT_FILTERS_PRFS_Params) extends Bundle {
  val ght_ft_cfg_in                             = Input(UInt(32.W))
  val ght_ft_cfg_valid                          = Input(UInt(1.W))

  val ght_ft_inst_in                            = Input(Vec(params.core_width, UInt(32.W)))
  val ght_ft_pc_in                              = Input(Vec(params.core_width, UInt(32.W)))
  val ght_ft_newcommit_in                       = Input(Vec(params.core_width, Bool()))
  val ght_ft_alu_in                             = Input(Vec(params.core_width, UInt((2*params.xlen).W)))
  val ght_ft_is_rvc_in                          = Input(Vec(params.core_width, UInt(1.W)))

  val ght_ft_inst_index                         = Output(UInt(8.W))
  val packet_out                                = Output(UInt((2*params.packet_size).W))

  val ght_stall                                 = Input(Bool())
  val core_hang_up                              = Output(UInt(1.W))
  val ght_buffer_status                         = Output(UInt(2.W))
  val ght_prfs_rd_ft                            = Input(Vec(params.core_width, UInt(params.xlen.W)))

  val ght_prfs_forward_ldq                      = Output(Vec(params.core_width, Bool()))
  val ght_prfs_forward_stq                      = Output(Vec(params.core_width, Bool()))
  val ght_prfs_forward_ftq                      = Output(Vec(params.core_width, Bool()))
  val ght_prfs_forward_prf                      = Output(Vec(params.core_width, Bool()))

  val ght_filters_empty                         = Output(UInt(1.W))
  val debug_filter_width                        = Input(UInt(4.W))

  /* R Features */
  val ght_filters_ready                         = Output(UInt(1.W))
  val core_r_arfs                               = Input(Vec(params.core_width, UInt(params.packet_size.W)))
  val core_r_arfs_index                         = Input(Vec(params.core_width, UInt(8.W)))
  val rsu_merging                               = Input(UInt(1.W))
  val ic_crnt_target                            = Input(UInt(5.W)) 
  val gtimer                                    = Input(UInt(62.W))
  val gtimer_reset                              = Input(UInt(1.W))
  val use_fi_mode                               = Input(UInt(1.W))
}



trait HasGHT_FILTERS_PRFS_IO extends BaseModule {
  val params: GHT_FILTERS_PRFS_Params
  val io = IO(new GHT_FILTERS_PRFS_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class GHT_FILTERS_PRFS (val params: GHT_FILTERS_PRFS_Params) extends Module with HasGHT_FILTERS_PRFS_IO
{
  val packet                                    = WireInit(0.U((params.packet_size*2).W))
  val inst_type                                 = WireInit(0.U((8*2).W))
  val buffer_width                              = 8 + params.packet_size


  // val u_ght_filters                             = Seq.fill(params.core_width) {Module(new GHT_FILTER_PRFS(GHT_FILTER_PRFS_Params(params.xlen, params.packet_size, params.use_prfs)))}
  val u_ght_filters = Seq.tabulate(params.core_width) { id =>
    Module(new GHT_FILTER_PRFS(GHT_FILTER_PRFS_Params(params.xlen, params.packet_size, params.use_prfs, id)))
  }
  
  val u_buffer                                  = Seq.fill(params.core_width) {Module(new GH_FIFO(FIFOParams (buffer_width, 8)))}
  val core_hang_up                              = u_buffer(params.core_width-1).io.status_threeslots

  // Connecting filters
  val filter_inst_index                         = WireInit(VecInit(Seq.fill(params.core_width)(0.U(8.W))))
  val filter_packet                             = WireInit(VecInit(Seq.fill(params.core_width)(0.U(params.packet_size.W))))
  for (i <- 0 to params.core_width - 1) {
    u_ght_filters(i).io.ic_crnt_target         := this.io.ic_crnt_target
    u_ght_filters(i).io.ght_ft_cfg_in          := this.io.ght_ft_cfg_in
    u_ght_filters(i).io.ght_ft_cfg_valid       := this.io.ght_ft_cfg_valid
    u_ght_filters(i).io.ght_ft_inst_in         := this.io.ght_ft_inst_in(i)
    u_ght_filters(i).io.ght_ft_pc_in           := this.io.ght_ft_pc_in(i)
    u_ght_filters(i).io.ght_ft_newcommit_in    := this.io.ght_ft_newcommit_in(i)
    u_ght_filters(i).io.ght_ft_alu_in          := this.io.ght_ft_alu_in(i)
    u_ght_filters(i).io.ght_ft_is_rvc_in       := this.io.ght_ft_is_rvc_in(i)
    u_ght_filters(i).io.gtimer                 := this.io.gtimer
    u_ght_filters(i).io.gtimer_reset           := this.io.gtimer_reset
    u_ght_filters(i).io.use_fi_mode            := this.io.use_fi_mode

    filter_inst_index(i)                       := u_ght_filters(i).io.ght_ft_inst_index
    filter_packet(i)                           := u_ght_filters(i).io.packet_out
    u_ght_filters(i).io.ght_prfs_rd            := this.io.ght_prfs_rd_ft(i)
    if (params.use_prfs) {
      this.io.ght_prfs_forward_ldq(i)          := u_ght_filters(i).io.ght_prfs_forward_ldq
      this.io.ght_prfs_forward_stq(i)          := u_ght_filters(i).io.ght_prfs_forward_stq
      this.io.ght_prfs_forward_ftq(i)          := u_ght_filters(i).io.ght_prfs_forward_ftq
      this.io.ght_prfs_forward_prf(i)          := u_ght_filters(i).io.ght_prfs_forward_prf
    } else {
      this.io.ght_prfs_forward_ldq(i)          := false.B
      this.io.ght_prfs_forward_stq(i)          := false.B
      this.io.ght_prfs_forward_ftq(i)          := false.B
      this.io.ght_prfs_forward_prf(i)          := false.B
    }
  }

  // Connecting buffers: Enqueue Phase
  val buffer_enq_valid                          = WireInit(false.B)
  val buffer_enq_data                           = WireInit(VecInit(Seq.fill(params.core_width)(0.U(buffer_width.W))))
  val buffer_empty                              = WireInit(VecInit(Seq.fill(params.core_width)(false.B)))
  val buffer_full                               = WireInit(VecInit(Seq.fill(params.core_width)(false.B)))
  val buffer_deq_data                           = WireInit(VecInit(Seq.fill(params.core_width)(0.U(buffer_width.W))))
  val buffer_deq_valid                          = WireInit(false.B)
  val is_valid_packet                           = WireInit(VecInit(Seq.fill(params.core_width)(0.U(13.W))))

  val new_packet                                = WireInit(VecInit(Seq.fill(params.core_width)(0.U(1.W))))
  val doPush                                    = WireInit(0.U(1.W))
  val buffer_inst_type                          = WireInit(VecInit(Seq.fill(params.core_width)(0.U(8.W))))
  val bp                                        = WireInit(VecInit(Seq.fill(params.core_width)(0.U(params.packet_size.W))))
  val doPull                                    = WireInit(0.U(1.W))
  
  
  for (i <- 0 to params.core_width - 1) {
    new_packet(i)                              := Mux(filter_packet(i) =/= 0.U, 1.U, 0.U)                                             
    buffer_enq_data(i)                         := Mux(((filter_inst_index(i) =/= 0.U) && (filter_packet(i) =/= 0.U)),  
                                                    Cat(filter_inst_index(i), filter_packet(i)), 0.U)
  }
  doPush                                       := new_packet.reduce(_|_)
  buffer_enq_valid                             := Mux(doPush === 1.U, true.B, false.B)

  
  for (i <- 0 to params.core_width - 1) {
    u_buffer(i).io.enq_valid                   := buffer_enq_valid
    u_buffer(i).io.enq_bits                    := buffer_enq_data(i)
  }

  // Connecting buffers: Dequeue Phase
  /* Buffer Finite State Machine */
  for (i <- 0 to params.core_width - 1) {
    buffer_empty(i)                            := u_buffer(i).io.empty
    buffer_full(i)                             := u_buffer(i).io.full
    buffer_deq_data(i)                         := u_buffer(i).io.deq_bits
    buffer_inst_type(i)                        := u_buffer(i).io.deq_bits(buffer_width - 1, params.packet_size)
    bp(i)                                      := u_buffer(i).io.deq_bits(params.packet_size - 1, 0)
    u_buffer(i).io.deq_ready                   := buffer_deq_valid
    is_valid_packet(i)                         := Mux(buffer_inst_type(i) =/= 0.U, 1.U, 0.U)
  }
  
  val t_buffer                                  = RegInit(VecInit(Seq.fill(params.core_width)(0.U((params.packet_size*2).W))))
  val t_inst_type                               = RegInit(VecInit(Seq.fill(params.core_width)(0.U((8*2).W))))
  val is_valid_t_buffer                         = WireInit(VecInit(Seq.fill(params.core_width)(0.U(1.W))))
  val t_buffer_inst_type                        = WireInit(VecInit(Seq.fill(params.core_width)(0.U((8*2).W))))
  val t_buffer_packet                           = WireInit(VecInit(Seq.fill(params.core_width)(0.U((params.packet_size*2).W))))

  val load_t_buffer                             = WireInit(0.U(1.W))
  /* FIreGuard only
  for (i <- 0 to params.core_width - 1) {
    t_buffer(i)                                := Mux(load_t_buffer === 1.U, bp(i), t_buffer(i))
    t_inst_type(i)                             := Mux(load_t_buffer === 1.U, buffer_inst_type(i), t_inst_type(i))
    is_valid_t_buffer(i)                       := (t_buffer(i) =/= 0.U)
    t_buffer_inst_type(i)                      := t_inst_type(i)
    t_buffer_packet(i)                         := t_buffer(i)
  }
  buffer_deq_valid                             := load_t_buffer
  */ 

  /*
  for (i <- 0 to params.core_width - 1) {
    t_buffer(i)                                := MuxCase(t_buffer(i),
                                                      Array(((load_t_buffer === 1.U) && (io.rsu_merging === 0.U)) -> bp(i),
                                                            ((load_t_buffer === 1.U) && (io.rsu_merging === 1.U)) -> io.core_r_arfs(i)
                                                          )
                                                          )

    t_inst_type(i)                             := MuxCase(t_inst_type(i),
                                                      Array(((load_t_buffer === 1.U) && (io.rsu_merging === 0.U)) -> buffer_inst_type(i),
                                                            ((load_t_buffer === 1.U) && (io.rsu_merging === 1.U)) -> io.core_r_arfs_index(i)
                                                          )
                                                          )
                                                          
    is_valid_t_buffer(i)                       := (t_buffer(i) =/= 0.U)
    t_buffer_inst_type(i)                      := t_inst_type(i)
    t_buffer_packet(i)                         := t_buffer(i)
  }*/

  val zeros_144bits                             = WireInit(0.U(144.W))
  t_buffer(0)                                  := MuxCase(t_buffer(0),
                                                      Array(((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) === 0.U) && (buffer_inst_type(2) === 0.U) && (buffer_inst_type(3) === 0.U)) -> Cat(zeros_144bits, bp(0)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) =/= 0.U)) -> Cat(bp(1), bp(0)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) === 0.U) && (buffer_inst_type(2) =/= 0.U)) -> Cat(bp(2), bp(0)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) === 0.U) && (buffer_inst_type(2) === 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(bp(3), bp(0)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) === 0.U) && (buffer_inst_type(1) =/= 0.U) && (buffer_inst_type(2) === 0.U) && (buffer_inst_type(3) === 0.U)) -> Cat(zeros_144bits, bp(1)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) === 0.U) && (buffer_inst_type(1) =/= 0.U) && (buffer_inst_type(2) =/= 0.U)) -> Cat(bp(2), bp(1)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) === 0.U) && (buffer_inst_type(1) =/= 0.U) && (buffer_inst_type(2) === 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(bp(3), bp(1)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) === 0.U) && (buffer_inst_type(1) === 0.U) && (buffer_inst_type(2) =/= 0.U) && (buffer_inst_type(3) === 0.U)) -> Cat(zeros_144bits, bp(2)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) === 0.U) && (buffer_inst_type(1) === 0.U) && (buffer_inst_type(2) =/= 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(bp(3), bp(2)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) === 0.U) && (buffer_inst_type(1) === 0.U) && (buffer_inst_type(2) === 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(zeros_144bits, bp(3)),
                                                          )
                                                          )

  t_buffer(1)                                  := MuxCase(t_buffer(1),
                                                     Array(((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) =/= 0.U) && (buffer_inst_type(2) =/= 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(bp(3), bp(2)),
                                                            ((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) =/= 0.U) && (buffer_inst_type(2) =/= 0.U) && (buffer_inst_type(3) === 0.U)) -> Cat(zeros_144bits, bp(2)),
                                                            ((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) =/= 0.U) && (buffer_inst_type(2) === 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(zeros_144bits, bp(3)),
                                                            ((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) === 0.U) && (buffer_inst_type(2) =/= 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(zeros_144bits, bp(3)),
                                                            ((load_t_buffer.asBool) && (buffer_inst_type(0) === 0.U) && (buffer_inst_type(1) =/= 0.U) && (buffer_inst_type(2) =/= 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(zeros_144bits, bp(3)),
                                                            (load_t_buffer.asBool) ->  0.U
                                                          )
                                                          )
  
  t_buffer(2)                                  := 0.U
  t_buffer(3)                                  := 0.U

  val zeros_8bits                               = WireInit(0.U(8.W))
  t_inst_type(0)                               := MuxCase(t_inst_type(0),
                                                      Array(((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) === 0.U) && (buffer_inst_type(2) === 0.U) && (buffer_inst_type(3) === 0.U)) -> Cat(zeros_8bits, buffer_inst_type(0)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) =/= 0.U)) -> Cat(buffer_inst_type(1), buffer_inst_type(0)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) === 0.U) && (buffer_inst_type(2) =/= 0.U)) -> Cat(buffer_inst_type(2), buffer_inst_type(0)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) === 0.U) && (buffer_inst_type(2) === 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(buffer_inst_type(3), buffer_inst_type(0)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) === 0.U) && (buffer_inst_type(1) =/= 0.U) && (buffer_inst_type(2) === 0.U) && (buffer_inst_type(3) === 0.U)) -> Cat(zeros_8bits, buffer_inst_type(1)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) === 0.U) && (buffer_inst_type(1) =/= 0.U) && (buffer_inst_type(2) =/= 0.U)) -> Cat(buffer_inst_type(2), buffer_inst_type(1)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) === 0.U) && (buffer_inst_type(1) =/= 0.U) && (buffer_inst_type(2) === 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(buffer_inst_type(3), buffer_inst_type(1)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) === 0.U) && (buffer_inst_type(1) === 0.U) && (buffer_inst_type(2) =/= 0.U) && (buffer_inst_type(3) === 0.U)) -> Cat(zeros_8bits, buffer_inst_type(2)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) === 0.U) && (buffer_inst_type(1) === 0.U) && (buffer_inst_type(2) =/= 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(buffer_inst_type(3), buffer_inst_type(2)),
                                                             ((load_t_buffer.asBool) && (buffer_inst_type(0) === 0.U) && (buffer_inst_type(1) === 0.U) && (buffer_inst_type(2) === 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(zeros_8bits, buffer_inst_type(3))
                                                            )
                                                            )

  t_inst_type(1)                               := MuxCase(t_inst_type(1),
                                                     Array(((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) =/= 0.U) && (buffer_inst_type(2) =/= 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(buffer_inst_type(3), buffer_inst_type(2)),
                                                            ((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) =/= 0.U) && (buffer_inst_type(2) =/= 0.U) && (buffer_inst_type(3) === 0.U)) -> Cat(zeros_8bits, buffer_inst_type(2)),
                                                            ((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) =/= 0.U) && (buffer_inst_type(2) === 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(zeros_8bits, buffer_inst_type(3)),
                                                            ((load_t_buffer.asBool) && (buffer_inst_type(0) =/= 0.U) && (buffer_inst_type(1) === 0.U) && (buffer_inst_type(2) =/= 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(zeros_8bits, buffer_inst_type(3)),
                                                            ((load_t_buffer.asBool) && (buffer_inst_type(0) === 0.U) && (buffer_inst_type(1) =/= 0.U) && (buffer_inst_type(2) =/= 0.U) && (buffer_inst_type(3) =/= 0.U)) -> Cat(zeros_8bits, buffer_inst_type(3)),
                                                            ((load_t_buffer.asBool)) -> 0.U
                                                          )
                                                          )
  t_inst_type(2)                               := 0.U
  t_inst_type(3)                               := 0.U

  for (i <- 0 to params.core_width - 1) {
    is_valid_t_buffer(i)                       := (t_inst_type(i) =/= 0.U)
    t_buffer_inst_type(i)                      := t_inst_type(i)
    t_buffer_packet(i)                         := t_buffer(i)
  }


  buffer_deq_valid                             := MuxCase(0.U,
                                                      Array(((load_t_buffer === 1.U) && (io.rsu_merging === 0.U)) -> 1.U,
                                                            ((load_t_buffer === 1.U) && (io.rsu_merging === 1.U)) -> 0.U
                                                          )
                                                          )



  // Simulating 1/2-width event filter
  /* Additional logic for filtering delay */
  val filter_width                              = io.debug_filter_width
  val s_num_packets                             = WireInit(0.U(3.W))
  val s_not_enough_filter_width                 = WireInit(0.U(1.W))
  val s_delay_counter                           = WireInit(0.U(3.W))
  val s_delay_counter_reg                       =  RegInit(0.U(3.W))
  val zeros_2bits                               = WireInit(0.U(2.W))

  s_num_packets                                := Cat(zeros_2bits, io.ght_ft_newcommit_in(0))  + Cat(zeros_2bits, io.ght_ft_newcommit_in(1)) + Cat(zeros_2bits, io.ght_ft_newcommit_in(2)) + Cat(zeros_2bits, io.ght_ft_newcommit_in(3))
  s_not_enough_filter_width                    := Mux((s_num_packets > filter_width), 1.U, 0.U)
  
  s_delay_counter                              := MuxCase(0.U,
                                                    Array(((s_not_enough_filter_width === 0.U) || (filter_width === 0.U)) -> 0.U,
                                                          ((s_not_enough_filter_width === 1.U) && (filter_width === 1.U)) -> (s_num_packets - 1.U),
                                                          ((s_not_enough_filter_width === 1.U) && (filter_width === 2.U)) -> 1.U
                                                          )
                                                          )

  when (s_not_enough_filter_width === 1.U) {
    s_delay_counter_reg                        := s_delay_counter_reg + s_delay_counter
  } .otherwise {
    when (core_hang_up =/= 1.U) {
      s_delay_counter_reg                      := Mux(s_delay_counter_reg =/= 0.U, (s_delay_counter_reg - 1.U), 0.U)
    } .otherwise {
      s_delay_counter_reg                      := s_delay_counter_reg
    }
  }

  val filter_stall                              = Mux((s_delay_counter_reg =/= 0.U), 1.U, 0.U)

  val fsm_reset :: fsm_send_first :: fsm_send_second :: fsm_send_third :: fsm_send_fourth :: Nil = Enum(5)
  val fsm_state                                 = RegInit(fsm_reset)
  val fsm_reset_nxt_state                       = WireInit(fsm_reset)
  val fsm_first_nxt_state                       = WireInit(fsm_reset)
  val fsm_second_nxt_state                      = WireInit(fsm_reset)
  val fsm_third_nxt_state                       = WireInit(fsm_reset)
  val fsm_fourth_nxt_state                      = WireInit(fsm_reset)
  val load_t_buffer_first_state                 = WireInit(0.U(1.W))
  val load_t_buffer_second_state                = WireInit(0.U(1.W))
  val load_t_buffer_third_state                 = WireInit(0.U(1.W))
  val load_t_buffer_fourth_state                = WireInit(0.U(1.W))

  switch (fsm_state) {
    is (fsm_reset){ // 0x0b000
      packet                                   := 0.U
      inst_type                                := 0.U
      load_t_buffer                            := Mux((!buffer_empty(0) || (io.rsu_merging === 1.U)), 1.U, 0.U)
      fsm_state                                := Mux((!buffer_empty(0) || (io.rsu_merging === 1.U)), fsm_reset_nxt_state, fsm_reset)
    }

    is (fsm_send_first){
      when (io.ght_stall) { // 0x0b001
        packet                                 := 0.U
        inst_type                              := 0.U
        fsm_state                              := fsm_send_first
        load_t_buffer                          := 0.U
      } .otherwise {
        packet                                 := t_buffer_packet(0)
        inst_type                              := t_buffer_inst_type(0)
        fsm_state                              := fsm_first_nxt_state
        load_t_buffer                          := load_t_buffer_first_state
      }
    }

    is (fsm_send_second){
      when (io.ght_stall) { // 0x0b010
        packet                                 := 0.U
        inst_type                              := 0.U
        fsm_state                              := fsm_send_second
        load_t_buffer                          := 0.U
      } .otherwise {
        packet                                 := t_buffer_packet(1)
        inst_type                              := t_buffer_inst_type(1)
        fsm_state                              := fsm_second_nxt_state
        load_t_buffer                          := load_t_buffer_second_state
      }
    }

    is (fsm_send_third){
      when (io.ght_stall) { // 0x0b011
        packet                                 := 0.U
        inst_type                              := 0.U
        fsm_state                              := fsm_send_third
        load_t_buffer                          := 0.U
      } .otherwise {
        packet                                 := t_buffer_packet(2)
        inst_type                              := t_buffer_inst_type(2)
        fsm_state                              := fsm_third_nxt_state
        load_t_buffer                          := load_t_buffer_third_state
      }
    }

    is (fsm_send_fourth){
      when (io.ght_stall) { // 0x0b100
        packet                                 := 0.U
        inst_type                              := 0.U
        fsm_state                              := fsm_send_fourth
        load_t_buffer                          := 0.U
      } .otherwise {
        packet                                 := t_buffer_packet(3)
        inst_type                              := t_buffer_inst_type(3)
        fsm_state                              := fsm_fourth_nxt_state
        load_t_buffer                          := load_t_buffer_fourth_state
      }
    }
  }
    

  // These are used for 4-width Boom
  // There are some work is required to make them generic
  fsm_reset_nxt_state                          := MuxCase(fsm_reset, 
                                                    Array((io.rsu_merging === 1.U) -> fsm_send_first,
                                                          (is_valid_packet.reduce(_|_) === 1.U)  -> fsm_send_first
                                                          )
                                                          )

  fsm_first_nxt_state                          := MuxCase(fsm_send_first, 
                                                    Array((is_valid_t_buffer(1) =/= 0.U)  -> fsm_send_second,
                                                          ((is_valid_t_buffer(1) === 0.U) && (is_valid_t_buffer(2) =/= 0.U))  -> fsm_send_third,
                                                          ((is_valid_t_buffer(1) === 0.U) && (is_valid_t_buffer(2) === 0.U) && (is_valid_t_buffer(3) =/= 0.U))  -> fsm_send_fourth,
                                                          ((is_valid_t_buffer(1) === 0.U) && (is_valid_t_buffer(2) === 0.U) && (is_valid_t_buffer(3) === 0.U)) -> Mux((!buffer_empty(0) || (io.rsu_merging === 1.U)), fsm_reset_nxt_state, fsm_reset)
                                                          )
                                                          )

  fsm_second_nxt_state                         := MuxCase(fsm_send_second, 
                                                    Array((is_valid_t_buffer(2) =/= 0.U)  -> fsm_send_third,
                                                          ((is_valid_t_buffer(2) === 0.U) && (is_valid_t_buffer(3) =/= 0.U))  -> fsm_send_fourth,
                                                          ((is_valid_t_buffer(2) === 0.U) && (is_valid_t_buffer(3) === 0.U)) -> Mux((!buffer_empty(0) || (io.rsu_merging === 1.U)), fsm_reset_nxt_state, fsm_reset)
                                                          )
                                                          )

  fsm_third_nxt_state                         := MuxCase(fsm_send_third, 
                                                    Array((is_valid_t_buffer(3) =/= 0.U)  -> fsm_send_fourth,
                                                          (is_valid_t_buffer(3) === 0.U) -> Mux((!buffer_empty(0) || (io.rsu_merging === 1.U)), fsm_reset_nxt_state, fsm_reset)
                                                          )
                                                          )

  fsm_fourth_nxt_state                        := Mux(((io.rsu_merging === 1.U) || (!buffer_empty(0))), fsm_reset_nxt_state, fsm_reset)

  load_t_buffer_first_state                   := Mux((is_valid_t_buffer(1) === 0.U) && (is_valid_t_buffer(2) === 0.U) && (is_valid_t_buffer(3) === 0.U) && (!buffer_empty(0) || (io.rsu_merging === 1.U)), 1.U, 0.U)
  load_t_buffer_second_state                  := Mux((is_valid_t_buffer(2) === 0.U) && (is_valid_t_buffer(3) === 0.U) && (!buffer_empty(0) || (io.rsu_merging === 1.U)), 1.U, 0.U)
  load_t_buffer_third_state                   := Mux((is_valid_t_buffer(3) === 0.U) && (!buffer_empty(0) || (io.rsu_merging === 1.U)), 1.U, 0.U) 
  load_t_buffer_fourth_state                  := Mux((!buffer_empty(0) || (io.rsu_merging === 1.U)), 1.U, 0.U)

  // Outputs
  io.ght_ft_inst_index                        := inst_type(7,0)
  io.packet_out                               := Cat(inst_type(15,8), packet(280,145), inst_type(7,0), packet(135,0)) // Added inst_type for checker cores
  io.core_hang_up                             := core_hang_up | filter_stall
  io.ght_buffer_status                        := Cat(buffer_full(params.core_width-1), buffer_empty.reduce(_&_))
  io.ght_filters_empty                        := buffer_empty.reduce(_&_)

  /* R Features */
  io.ght_filters_ready                        := MuxCase(0.U,
                                                    Array(((load_t_buffer === 1.U) && (io.rsu_merging === 0.U)) -> 0.U,
                                                          ((load_t_buffer === 1.U) && (io.rsu_merging === 1.U)) -> 1.U
                                                         )
                                                         )
}