abstract class Valores(dominio: Dominio, valores:Array[Float]) {

   override def toString: String

   def obtenerValor(asignacion: Asignacion): Float

   def obtenerValores: Array[Float] = valores

   def obtenerVariables: List[Variable] = dominio.listaVar

   def combinar(v: Valores): Valores

   def restringir(variable: Variable, estado: Int): Valores

   def convertir: Valores

}
