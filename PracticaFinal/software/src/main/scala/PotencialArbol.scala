case class PotencialArbol(dom: Dominio, vals: ValoresArbol) extends Potencial(dom, vals) {

   def combinar(p: Potencial): PotencialArbol = PotencialArbol(dom + p.dominio, vals.combinar(p.valores))

   def restringir(variable: Variable, estado: Int): PotencialArbol =
      PotencialArbol(dom - variable, vals.restringir(variable, estado))

   def convertir: PotencialArray = PotencialArray(dom, vals.convertir)
}
