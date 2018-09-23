case class ValoresArray(dominio: Dominio, valores:Array[Float]) extends Valores(dominio, valores) {

   override def toString: String = {

      (0 until dominio.maximoIndice).map(i => {
         val asignacionFinal = Asignacion(dominio, i)
         asignacionFinal.toString + " = " + obtenerValor(asignacionFinal)
      }).mkString("\n")

   }

   def obtenerValor(asignacion: Asignacion): Float = valores(asignacion.calcularIndice)


   def combinar(v: Valores): ValoresArray = v match{
      case v: ValoresArray => combinarArrayArray(v)
      case v: ValoresArbol => combinarArrayArray(v.convertir)
   }

   def convertir: ValoresArbol = ValoresArbol(dominio, valores)

   private def combinarArrayArray(va: ValoresArray):ValoresArray = {

      val dominioFinal = dominio + va.dominio

      val valoresFinal = (0 until dominioFinal.maximoIndice).map(i => {
         val asignacionFinal = Asignacion(dominioFinal, i)
         val asignacionThis = asignacionFinal.proyectar(dominio)
         val asignacionOtro = asignacionFinal.proyectar(va.dominio)
         obtenerValor(asignacionThis) * va.obtenerValor(asignacionOtro)
      })

      ValoresArray(dominioFinal, valoresFinal.toArray[Float])

   }

   //Es necesario que la suma de asignación añada los elementos por la izquierda
   //para que restringir funcione correctamente, cosa que es incosistente con
   //el resto de la practica
   def restringir(variable: Variable, estado: Int): ValoresArray = {

      val dominioFinal = dominio - variable

      val valoresFinal = (0 until dominioFinal.maximoIndice).map(i => {
         val asignacionFinal = Asignacion(dominioFinal, i)
         val asignacionCompleta = (estado, variable) +: asignacionFinal
         val asignacionOrdenada = asignacionCompleta.proyectar(dominio)
         obtenerValor(asignacionOrdenada)
      })

      ValoresArray(dominioFinal, valoresFinal.toArray[Float])
   }

}

object PruebaValoresArray extends App {

   val X1 = Variable("X1",2)
   val X2 = Variable("X2",2)
   val X3 = Variable("X3",2)


   val valores1 = ValoresArray(Dominio(List(X1, X2)), Array(0.3f, 0.7f, 0.6f, 0.4f))
   val valores2 = ValoresArray(Dominio(List(X2, X3)), Array(0.9f, 0.1f, 1f, 0f))

   val comb = valores1.combinar(valores2)
   println("Valores de la combinacion: " + comb.obtenerValores.toList)

   val restringido = valores1.restringir(X1, 0)
   println("Valores restringido: " + restringido.obtenerValores.toList)
   println("Valores de valores1: " + valores1.obtenerValores.toList)

   println(valores1.toString)


}
