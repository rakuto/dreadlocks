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
