case class ValoresArbol(dominio: Dominio, valores:Array[Float]) extends Valores(dominio, valores) {

   val raiz: Nodo = crearArbol(0, Asignacion(Dominio(List())))

   private def crearArbol(indice: Int, asignacion: Asignacion): Nodo = {

      val variable = dominio.listaVar(indice)

      if(indice < dominio.longitud - 1){

         val listaHijos = (0 until variable.estados).map(i =>
            crearArbol(indice+1, asignacion :+ (i, variable))).toList

         NodoVariable(variable, listaHijos)

      } else {

         val listaHijos = (0 until variable.estados).map(i => {
            val asignacionHoja = asignacion :+ (i, variable)
            NodoHoja(valores(asignacionHoja.calcularIndice))
         }).toList

         NodoVariable(variable, listaHijos)

      }

   }

   override def toString: String = {

      def go(nivel: Int, n: Nodo): String = n match {
         case n: NodoVariable => n.hijos.indices.map(i => "\t" * nivel + n.v.nombre + ": " + i +
            "\n" + go(nivel+1, n.hijos(i))).mkString("")

         case n: NodoHoja => "\t"*nivel + " = " + n.valor + "\n"
      }

      go(0, raiz)
   }

   def obtenerValor(asignacion: Asignacion): Float = {
      if (asignacion.dominio != dominio)
         throw new IllegalArgumentException("Dominios no coincidentes")
      else raiz.obtenerValor(asignacion)
   }

   def combinar(v: Valores): ValoresArbol = v match {
      case v: ValoresArbol => this.combinarArbolArbol(v)
      case v: ValoresArray => this.combinarArbolArbol(v.convertir)
   }

   private def combinarArbolArbol(v: ValoresArbol): ValoresArbol =
      ValoresArbol(dominio + v.dominio, raiz.combinar(v.raiz).obtenerValores.toArray)


   def restringir(variable: Variable, estado: Int): ValoresArbol =
      ValoresArbol(dominio - variable,
         raiz.restringir(variable, estado).obtenerValores.toArray)


   def convertir: ValoresArray = ValoresArray(dominio, raiz.obtenerValores.toArray)

}

object PruebaValoresArbol extends App {

   val X1 = Variable("X1",2)
   val X2 = Variable("X2",2)
   val X3 = Variable("X3",2)

   val valores1 = ValoresArbol(Dominio(List(X1, X2)), Array(0.3f, 0.7f, 0.6f, 0.4f))
   val valores2 = ValoresArbol(Dominio(List(X2, X3)), Array(0.9f, 0.1f, 1f, 0f))

   val comb = valores1.combinar(valores2)
   println("Valores de la combinacion: " + comb.obtenerValores.toList)

   val restringido = valores1.restringir(X1, 0)
   println("Valores restringido: " + restringido.obtenerValores.toList)
   println("Valores de valores1: " + valores1.obtenerValores.toList)

   println(comb.toString)


}
