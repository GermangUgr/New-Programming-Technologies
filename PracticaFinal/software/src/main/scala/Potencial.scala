//Si la clase no fuera abstracta se podría implementar todos los metodos
//haciendo la disntinción sobre el dato miembro valores
abstract class Potencial(dom: Dominio, vals: Valores) {

   def dominio: Dominio = dom

   def valores: Valores = vals

   override def toString: String = vals.toString

   def combinar(p: Potencial): Potencial

   def restringir(variable: Variable, estado: Int): Potencial

   def obtenerValores: List[Float] = vals.obtenerValores.toList

   def convertir: Potencial

}
