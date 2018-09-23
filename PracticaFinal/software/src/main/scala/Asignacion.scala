case class Asignacion(dominio: Dominio, valores: List[Int]) {

   val datos = calcularDatos

   private def calcularDatos(): Map[Variable, Int] = {

      if (!dominio.listaVar.map(v => v.estados).zip(valores).forall( par => par._2 >= 0 && par._2 < par._1))
         throw new IllegalArgumentException("Valor ilegal en asignaciones")

      else Map(dominio.listaVar.zip(valores):_*)

   }

   def vacia: Boolean = dominio.vacio

   def obtenerNumeroVariables: Int = dominio.longitud

   def obtenerValorVariable(v: Variable): Int = datos(v)

   def :+ (par:(Int, Variable)): Asignacion = {
      new Asignacion(dominio :+ par._2, valores :+ par._1)
   }

   def +: (par:(Int, Variable)): Asignacion = {
      new Asignacion(par._2 +: dominio, par._1 +: valores)
   }

   def calcularIndice: Int = dominio.pesos.zip(valores).map(par => par._1 * par._2).sum

   override def toString: String = {
      datos.map(par => "[" + par._1.nombre + " - " + par._2 + "]").mkString(" ")
   }

   def proyectar(d: Dominio): Asignacion = {

      val intersec = dominio.listaVar.zip(valores).
         filter(par => d.listaVar.exists(v => v.nombre == par._1.nombre))

      val dom = Dominio(intersec.map(par => par._1))
      val variables = intersec.map(par => par._2)

      new Asignacion(dom,  variables)
   }

}

object Asignacion  {

   def apply(dominio: Dominio): Asignacion = new Asignacion(dominio, List.fill(dominio.longitud)(0))

   def apply(dominio: Dominio, indice: Int): Asignacion = new Asignacion(dominio, dominio.pesos.map(p => indice/p).zip(dominio.listaVar.map(v => v.estados)).
      map({case (x, y) => x % y}))

}

object PruebaAsignacion extends App {

   val asignacionVacia=Asignacion(Dominio(List()), List())
   println("Comprobacion vacio asignacion vacia: "+asignacionVacia.vacia)

   val X1 = Variable("X1",3)
   val X2 = Variable("X2",4)
   val X3 = Variable("X3",2)
   val X4 = Variable("X4",2)
   val X5 = Variable("X5",3)

   val dominio1 = Dominio(List(X1, X2, X3))

   val asignacion1=Asignacion(Dominio(List(X1, X2, X3, X4)), List(2,3,1,0))
   println("Comprobacion vacio sobre asignacion no vacia: " + asignacion1.vacia)
   println("Se muestra la asignacion: ")
   println(asignacion1)

   // Calculo del indice asociado a la asignacion (debe ser 46)
   val indice1=asignacion1.calcularIndice
   println("indice1 (debe ser 46): " + indice1)

   //A partir del indice obtenemos la asignacion
   val asignacionDeIndice = Asignacion(asignacion1.dominio, indice1)

   // Se muestra la asignacion obtenida: debe ser X1=2, X2=3, X3=1, X4=0
   println("Asignacion resultante: " + asignacionDeIndice)

   val asignacion2 = asignacion1.proyectar(dominio1)
   println("Asignacion2 Proyectada: " + asignacion2)

   println("Asignacion1: " + asignacion1)
   val asignacion3 = asignacion1 :+ (2, X5)
   println("Asignacion3 + X5: " + asignacion3)
   println("Asignacion3 dominio: " + asignacion3.dominio.toString)



}
