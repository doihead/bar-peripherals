
package barp

import chisel3._
import chisel3.util._
import chisel3.experimental.{IntParam, BaseModule}
import freechips.rocketchip.subsystem.BaseSubsystem
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntIsOneOf
import chisel3.experimental.hierarchy.public
import freechips.rocketchip.util.{SynchronizerShiftReg}



case class QDECParams(
  address: BigInt = 0x1000
)

case object QDECKey extends Field[Option[QDECParams]](None)

trait QDECDeviceIO extends Bundle {
  val a = Input(Bool())
  val b = Input(Bool())
  val a2 = Input(Bool())
  val b2 = Input(Bool())
}

class ConcreteQDECIO extends QDECDeviceIO {

}

trait QDECDevice extends HasRegMap {
  val io: QDECDeviceIO

  implicit val p: Parameters
  //def params: QDECParams
  val enc_cnt = RegInit(0.U(32.W))
  val enc_cnt2 = RegInit(0.U(32.W))

  val a_sync = RegNext(io.a, false.B)
  val b_sync = RegNext(io.b, false.B)

  val a2_sync = RegNext(io.a2, false.B)
  val b2_sync = RegNext(io.b2, false.B)

  val a_prev_val = RegNext(a_sync, false.B)
  val a_trig = !a_prev_val && a_sync

  val a2_prev_val = RegNext(a2_sync, false.B)
  val a2_trig = !a2_prev_val && a2_sync

  a_sync := SynchronizerShiftReg(io.a, 2, name = Some("a_synch"))
  b_sync := SynchronizerShiftReg(io.b, 2, name = Some("b_synch"))
  a2_sync := SynchronizerShiftReg(io.a2, 2, name = Some("a2_synch"))
  b2_sync := SynchronizerShiftReg(io.b2, 2, name = Some("b2_synch"))

  a_prev_val := a_sync
  a2_prev_val := a2_sync


  when(a_trig) {
    when (b_sync === true.B) {
      enc_cnt := enc_cnt + 1.U
    } .otherwise {
      enc_cnt := enc_cnt - 1.U
    }
  }

  when(a2_trig) {
    when (b2_sync === true.B) {
      enc_cnt2 := enc_cnt2 + 1.U
    } .otherwise {
      enc_cnt2 := enc_cnt2 - 1.U
    }
  }

  regmap(
  0x00 -> Seq(
    RegField(32, enc_cnt)),
  0x04 -> Seq(
    RegField(32, enc_cnt2))) // A one word read write register
}

class QDECTL(params: QDECParams, beatBytes: Int)(implicit p: Parameters)
  extends TLRegisterRouter(
    params.address, "qdec1", Seq("ucbbar,qdec"),
    beatBytes = beatBytes)(
      new TLRegBundle(params, _) with QDECDeviceIO)(
      new TLRegModule(params, _, _) with QDECDevice)

trait CanHaveQDECDevice { this: BaseSubsystem =>
  private val portName = "qdec2"

  val qdec = p(QDECKey) match {
    case Some(params) => {
      val qdec = LazyModule(new QDECTL(params, pbus.beatBytes)(p))
      pbus.coupleTo(portName) { qdec.node := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
      Some(qdec)
    }
    case None => None
  }
}

trait CanHaveQDECDeviceImp extends LazyModuleImp {
  val outer: CanHaveQDECDevice
  val qdec_io = outer.qdec match {
    case Some(qdec) => {
      val qdecio = IO(new ConcreteQDECIO).suggestName("qdec3")
      qdecio <> qdec.module.io
      Some(qdecio)
    }
    case None => None
  }
}

class WithQDECDevice(address: BigInt) extends Config((site, here, up) => {
  case QDECKey => Some(QDECParams(address=address))
})

