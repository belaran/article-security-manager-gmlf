#!/bin/sh
exec scala "$0" "$@"
!#

import java.lang.Character.{ LETTER_NUMBER => CR,LINE_SEPARATOR => LF }
import java.net.InetSocketAddress
import java.nio.channels.AsynchronousChannelGroup._
import java.nio.channels.AsynchronousServerSocketChannel._
import java.nio.channels.{ AsynchronousSocketChannel => ASC }
import java.nio.channels.CompletionHandler
import java.nio.ByteBuffer._
import java.util.concurrent.Executors._
import scala.annotation.implicitNotFound
import scala.collection.mutable.ListBuffer

import org.jsoup.Jsoup
import org.jsoup.safety.Whitelist

/**
 * This code was borrowed from https://gist.github.com/igm/2936086
 */
object HttpServerSecurityManager extends SecurityManager {

  override def checkAccept(host:String, port:Int) = {
   if ( ! (host.equals("localhost") || host.equals("127.0.0.1")) || (port != 8000 && port < 50000)  )
     throw new SecurityException("This server is not allowed to be bound to " + host + ":" + port)
  }

  override def checkPropertyAccess(propertyName: String) = {}

  override def checkPermission(perm:java.security.Permission) = {}
}

object CustomClassLoader extends ClassLoader {
  override def findClass(classname:String): Class[_] = checkClass(super.findClass(classname))

  def containsIllegalMethods(clazz:Class[_]) = {
    clazz.getDeclaredMethods()
      .filter( ! _.getName().startsWith("get") )
      .filter( ! _.getName().startsWith("set") )
      .filter( ! _.getName().startsWith("is") ).length > 0
  }

  def checkClass(clazz:Class[_]): Class[_] = {
    if ( clazz.getPackage().toString().startsWith("org.custom.beans") && ! containsIllegalMethods(clazz) )
      throw new IllegalStateException("Illegal object detected!")
   clazz
  }
}

object HttpServer extends App {
  val port = 8000

  def checkPermissionsAreSet(): Boolean = {
    var sm = System.getSecurityManager()
  /*  if ( sm == null ) {
      sm = HttpServerSecurityManager
      System.setSecurityManager(sm)
    }*/

    try {
      if ( sm != null )
        sm.checkAccept("localhost",port)
    } catch {
      case se: java.lang.SecurityException => return false
    }
    return true
  }


  Console.println("Starting HttpServer")
  if ( checkPermissionsAreSet() ) {
    val channelGroup = withFixedThreadPool(1, defaultThreadFactory());
    val listenChannel = open(channelGroup).bind(new InetSocketAddress(port));
    while (true) MyHttpProcess(listenChannel.accept().get())();
  } else
    Console.println("Ce programme ne peut fonctionner sans l'autorisation d'accéder au port " + port)
  Console.println("Shutting down HttpServer")
}

case class MyHttpProcess(ch: ASC) {
  val buf = allocate(1024); buf.flip()

  implicit def fn2CH(fn: Int => Unit) = new CompletionHandler[Integer, Void]() {
    def completed(res: Integer, _2: Void): Unit = fn(res)
    def failed(_1: Throwable, _2: Void) = throw new RuntimeException(_1)
  }

  def apply() = readLine { request_line => readHeader { header => ch.write(wrap(html), null, (res: Int) => ch.close()) } }

  def readHeader(fn: List[String] => Unit, lines: ListBuffer[String] = new ListBuffer): Unit = readLine {
    case "" => fn(lines.toList)
    case any => readHeader(fn, lines += any);
  }

  def readLine(fn: String => Unit, sb: StringBuilder = new StringBuilder): Unit = readChar {
    case LF => readChar { case CR => fn(sb.toString) }
    case CR => fn(sb.toString);
    case any => readLine(fn, sb += any)
  }

  def readChar(fn: Char => Unit) = buf.hasRemaining() match {
    case true => fn(buf.get().toChar);
    case _ =>
      buf.clear();
      ch.read(buf, null, (res: Int) => if (res != -1) { buf.flip(); fn(buf.get().toChar) } else ch.close())
  }


  val content = Jsoup.clean("<h1>Hello World</h1>", Whitelist.basic())
  val html = (s"""
HTTP/1.1 200
Client
Date: Tue, 24 Apr 2012 10:10:51 GMT
Content-Type: text/html; charset=UTF-8

$content""").getBytes
}

HttpServer.main(args)
