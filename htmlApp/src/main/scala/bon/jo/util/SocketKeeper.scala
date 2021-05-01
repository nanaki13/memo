package bon.jo.util

import bon.jo.util.SocketKeeper.SocketContext
import org.scalajs.dom.raw.{Event, MessageEvent, WebSocket}

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.util.{Failure, Success, Try}



object SocketKeeper {
  case class SocketContext(name : String,url: String,var onmessage: js.Function1[MessageEvent, _] ) {


  }

  def updateOnMessage(function:js.Function1[MessageEvent, _] )(implicit ctx : SocketContext): Unit = {
    ctx.onmessage = function
    SocketKeeper().ws.onmessage = function
  }

  val sockets: mutable.Map[String, SocketKeeper] = mutable.Map[String, SocketKeeper]()

  def logEvent(event: Event): String = jsonLog(event)

  def jsonLog: js.Any => String = JSON.stringify(_)

  private def defaultSocket(implicit ctx : SocketContext): WebSocket = {

    println(s"creation : $ctx")
    val ret = new WebSocket(ctx.url)
    ret.onclose = {
      e =>
        println(s"${ctx.name} : close, ${logEvent(e)}")
        sockets -= ctx.name

    }
    ret.onerror =  {
      e =>
        println(s"${ctx.name} : error, ${logEvent(e)}")
        sockets -= ctx.name

    }
    ret.onmessage = ctx.onmessage
    ret
  }

  private def defaultSocket(webSocket : WebSocket)(implicit ctx : SocketContext): WebSocket = {
    println(s"creation : $ctx sur $webSocket")
    val ret = new WebSocket(ctx.url)
    ret.onclose = webSocket.onclose
    ret.onerror = webSocket.onerror
    ret.onmessage = ctx.onmessage
    ret.onopen = webSocket.onopen
    ret
  }

  def apply()(implicit ctx : SocketContext): SocketKeeper = {
    sockets.getOrElseUpdate(ctx.name,
      {
        new SocketKeeper(ctx.name, ctx.url, defaultSocket, true)
      }
    )
  }



  def send(request: String,beforeSend :Option[() =>Unit] = None)(implicit ctx : SocketContext): SocketKeeper = {
    val s = SocketKeeper()
    (s.send(request,beforeSend) match {
      case None => None
      case e : Some[SocketKeeper] => e
    }) foreach { e => sockets += ctx.name -> e }
    s
  }

}

case class SocketKeeper(name: String, url: String,private val  ws: WebSocket, fresh: Boolean) {
  private def send(request: String,beforeSend  :Option[() =>Unit] = None)(implicit ctx : SocketContext): Option[SocketKeeper] = {
    if (fresh) {
      onOpen(request,beforeSend)
      Some(this.copy(fresh = false))
    } else {
      Try({beforeSend.foreach(_());ws.send(request)}) match {
        case Success(_) => None
        case Failure(_) => println("on retente"); Some({
          val s = SocketKeeper.defaultSocket( this.ws)
          this.copy(ws = s, fresh = true)
        })
      }
    }

  }

  def onOpen(send: String): Unit = {
    ws.onopen = _ => {
      ws.send(send)
    }
  }
  def onOpen(send: String,beforeSend   :Option[() =>Unit] = None): Unit = {
    ws.onopen = _ => {
      beforeSend.foreach(_())
      ws.send(send)
    }
  }

}
