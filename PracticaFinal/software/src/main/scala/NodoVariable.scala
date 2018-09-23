case class NodoVariable(v: Variable, hijos: List[Nodo]) extends Nodo {

   def obtenerValores: List[Float] = hijos.flatMap(h => h.obtenerValores)

   def obtenerHijo(ind: Int): Nodo = hijos(ind)

   def obtenerValor(asig: Asignacion): Float = hijos(asig.datos(v)).obtenerValor(asig)

   def combinar(n: Nodo): Nodo = n match {
      case n: NodoHoja => NodoVariable(v, hijos.map(h => h.combinar(n)))
      case n: NodoVariable => {

         val otroFinal = (0 until v.estados).map(valor => n.restringir(v, valor))
         val listaHijos = otroFinal.indices.map(i => hijos(i).combinar(otroFinal(i)))

         NodoVariable(v, listaHijos.toList)
      }
   }

   def restringir(variable: Variable, estado: Int): Nodo = {
      if(variable == v) hijos(estado)
      else NodoVariable(v, hijos.map(h => h.restringir(variable, estado)))
   }
}
