abstract class Nodo {

   def obtenerValores: List[Float]

   def obtenerHijo(ind: Int): Nodo

   def obtenerValor(asig: Asignacion): Float

   def combinar(n: Nodo): Nodo

   def restringir(variable: Variable, estado: Int): Nodo

}
