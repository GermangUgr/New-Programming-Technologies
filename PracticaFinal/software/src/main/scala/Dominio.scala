case class Dominio(listaVar: List[Variable]) {

   private var pesosVariables = calcularPesos
   //var indiceVariables: List[Int]

   private def calcularPesos: List[Int] = {

      var listaEstados = listaVar.map(a => a.estados)

      def go(l: List[Int]): List[Int] = {
         if(l.size >= listaEstados.size) l
         else go(List(l.head * listaEstados(listaEstados.size - l.size)) ::: l)
      }

      go(List(1))

   }

   def apply(ind: Int): Variable = listaVar(ind)

   def longitud: Int = listaVar.length

   def vacio: Boolean = listaVar.isEmpty

   def pesos: List[Int] = pesosVariables

   def maximoIndice: Int = listaVar.map(a => a.estados).product

   override def toString: String = {
      listaVar.indices.map( a => listaVar(a).nombre + "(s: " + listaVar(a).estados
         + " w: " + pesosVariables(a) + ")").toString()
   }

   def +: (v: Variable): Dominio ={
      if (!listaVar.exists((a) => a.nombre == v.nombre)) Dominio(List(v):::listaVar)
      else Dominio(listaVar)
   }

   def :+ (v: Variable): Dominio ={
      if (!listaVar.exists((a) => a.nombre == v.nombre)) Dominio(listaVar:::List(v))
      else Dominio(listaVar)
   }

   def + (d: Dominio): Dominio = Dominio(listaVar.filter( v => !d.listaVar.exists( a => a.nombre == v.nombre)):::d.listaVar)

   def - (v: Variable): Dominio = Dominio(listaVar.filter( a => a != v ))

}


object PruebaVariableDominio extends App {

   val X1 = Variable("X1", 3)
   val X2 = Variable("X2", 4)
   val X3 = Variable("X3", 2)
   val X4 = Variable("X4", 2)
   var X5 = Variable("X5", 5)

   var dominio = Dominio(List(X1,X2,X3,X4))
   var dominio2 = Dominio(List(X1,X2,X3,X4,X5))
   dominio = dominio + dominio2
   //dominio = dominio - Variable("X5", 5)
   println(dominio)
   var vacio = Dominio(List())

}
