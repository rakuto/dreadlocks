/**
 * RitchReflection - scalalites.org
 *
 *  Copyright (c) 2009 Rakuto Furutani (rakuto@scalalites.org)
 *  All rights reserved.
 *
 *  Permission to use, copy, modify, and distribute this software in source
 *  or binary form for any purpose with or without fee is hereby granted,
 *  provided that the following conditions are met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
package org.scalalites.dreadlocks

sealed trait TypeConversion
{
  def val2type[T](value: T) = value match {
    case (_: Byte)    => classOf[Byte]
    case (_: Short)   => classOf[Short]
    case (_: Int)     => classOf[Int]
    case (_: Long)    => classOf[Long]
    case (_: Float)   => classOf[Float]
    case (_: Double)  => classOf[Double]
    case (_: Boolean) => classOf[Boolean]
    case (_: Char)    => classOf[Char]
    case (_: String)  => classOf[String]
    case (_: Unit)    => classOf[Unit]
  } 
  def val2javatype[T](value: T) = value match {
    case (_: Byte)    => java.lang.Byte.TYPE
    case (_: Short)   => java.lang.Short.TYPE
    case (_: Int)     => classOf[Integer]
    case (_: Long)    => java.lang.Long.TYPE
    case (_: Float)   => java.lang.Float.TYPE
    case (_: Double)  => java.lang.Double.TYPE
    case (_: Boolean) => java.lang.Boolean.TYPE
    case (_: Char)    => java.lang.Character.TYPE
    case (_: String)  => classOf[String]
    case (_: Unit)    => java.lang.Void.TYPE
  }
}

sealed class RichClass[T](klass: Class[T]) extends Proxy with TypeConversion
{
  def self = klass
  def invoke[T](name: String, that: Any, args: Array[_], retType: Class[T]): T = {
    val parameterTypes = args.map(val2type(_))
    val meth = klass.getMethod(name, parameterTypes: _*) 
    val jargs = args.map{ x => val2javatype(x).cast(x)}
    val ret = meth.invoke(that, jargs: _*)
    retType.cast(ret)
  }
}

object RichReflection {
  implicit def classWrapper(klass: Class[_]) = new RichClass(klass)
}

/*object Reflection {*/

  //def main(args: Array[String]) = {
    //import Reflection._

    //var args = Array(0, 3)
    //var ret = classOf[String].invoke("substring", "abcde", args, classOf[String])
    //println(ret)
  //}
/*}*/
