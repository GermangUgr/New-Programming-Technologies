case class PotencialArray(dom: Dominio, vals: ValoresArray) extends Potencial(dom, vals){

   def combinar(p: Potencial): PotencialArray = PotencialArray(dom + p.dominio, vals.combinar(p.valores))

   def restringir(variable: Variable, estado: Int): PotencialArray =
      PotencialArray(dom - variable, vals.restringir(variable, estado))

   def convertir: PotencialArbol = PotencialArbol(dom, vals.convertir)

}
