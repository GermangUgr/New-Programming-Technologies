case class NodoHoja(valor: Float) extends Nodo {

   def obtenerValores: List[Float] = List(valor)

   def obtenerHijo(ind: Int): Nodo = this

   def obtenerValor(asig: Asignacion): Float = valor

   def combinar(n: Nodo): Nodo = n match {
      case n: NodoHoja => NodoHoja(valor * n.valor)
      case n: NodoVariable => n.combinar(this)
   }

   def restringir(variable: Variable, estado: Int): Nodo = NodoHoja(valor)
}
