package freechips.rocketchip.r

import chisel3._
import chisel3.util._
import chisel3.experimental.{BaseModule}
import freechips.rocketchip.guardiancouncil._

case class R_LSLParams(
  nEntries: Int,
  xLen: Int
)

class R_LSLIO(params: R_LSLParams) extends Bundle {
  val m_ld_valid = Input(UInt(1.W))
  val m_st_valid = Input(UInt(1.W))
  val m_ldst_data = Input(UInt((params.xLen).W))
  val m_ldst_addr = Input(UInt((params.xLen).W))
  val cdc_ready = Output(UInt(1.W))

  val req_ready = Output(UInt(1.W))
  val req_valid = Input(UInt(1.W))
  val req_addr = Input(UInt(40.W)) // Later used for comparision
  val req_tag = Input(UInt(8.W))
  val req_cmd = Input(UInt(2.W)) // 01: load; 10: store
  val req_data = Input(UInt(params.xLen.W)) // Later used for comparision
  val req_size = Input(UInt(2.W))

  val resp_valid = Output(UInt(1.W))
  val resp_tag = Output(UInt(8.W))
  val resp_size = Output(UInt(2.W))
  val resp_data = Output(UInt(params.xLen.W))
  val resp_has_data = Output(UInt(1.W))
  val resp_replay = Output(UInt(1.W))
  val near_full = Output(UInt(1.W))
}

trait HasR_RLSLIO extends BaseModule {
  val params: R_LSLParams
  val io = IO(new R_LSLIO(params))
}

class R_LSL(val params: R_LSLParams) extends Module with HasR_RLSLIO {
  val fifowidth               = 2*params.xLen + 2 // extra two bits added to indicate inst type
  val u_channel               = Module (new GH_MemFIFO(FIFOParams(fifowidth, params.nEntries)))

  val channel_enq_valid       = WireInit(false.B)
  val channel_enq_data        = WireInit(0.U(fifowidth.W))
  val channel_deq_ready       = WireInit(false.B)
  val channel_deq_data        = WireInit(0.U(fifowidth.W))
  val channel_empty           = WireInit(true.B)
  val channel_full            = WireInit(false.B)
  val channel_nearfull        = WireInit(0.U(1.W))

  u_channel.io.enq_valid     := channel_enq_valid
  u_channel.io.enq_bits      := channel_enq_data
  u_channel.io.deq_ready     := channel_deq_ready
  channel_deq_data           := u_channel.io.deq_bits
  channel_empty              := u_channel.io.empty
  channel_full               := u_channel.io.full
  channel_nearfull           := u_channel.io.status_threeslots

  val enq_valid               = RegInit(false.B)
  val enq_data                = RegInit(0.U(fifowidth.W))

  enq_valid                  := (io.m_st_valid === 1.U) || (io.m_ld_valid === 1.U)
  enq_data                   := Cat(io.m_st_valid, io.m_ld_valid, io.m_ldst_data, io.m_ldst_addr)
  channel_enq_valid          := Mux(enq_valid, 1.U, 0.U)
  channel_enq_data           := Mux(enq_valid, enq_data, 0.U)
  io.cdc_ready               := enq_valid

  val resp_valid_reg          = RegInit(0.U(1.W))
  val resp_tag                = RegInit(0.U(8.W))
  val cmd                     = RegInit(0.U(2.W))
  val req_size_reg            = RegInit(0.U(2.W))

  channel_deq_ready          := io.req_valid
  resp_valid_reg             := io.req_valid
  resp_tag                   := io.req_tag
  cmd                        := io.req_cmd
  req_size_reg               := io.req_size

  io.req_ready               := !channel_empty
  io.resp_valid              := resp_valid_reg
  io.resp_tag                := resp_tag
  // Revisit
  io.resp_size               := Mux((resp_valid_reg === 1.U), req_size_reg, 0.U)
  io.resp_data               := Mux((resp_valid_reg === 1.U) && (cmd === 1.U), channel_deq_data(127, 64), 0.U)
  io.resp_has_data           := Mux((resp_valid_reg === 1.U) && (cmd === 1.U), 1.U, 0.U)
  io.resp_replay             := 0.U // Cashed respone
  io.near_full               := u_channel.io.status_fiveslots
}