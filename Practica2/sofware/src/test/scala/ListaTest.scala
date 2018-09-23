import Funciones.busquedaBinaria
import Prueba.lista1
import org.scalacheck.Properties
import org.scalacheck.Prop.{AnyOperators, forAll, throws}
import org.scalacheck.Gen._

object ListaTest extends Properties("ListaTest") {

   val coordenadasExtremos = for {
      fila <- choose(0, 20)
      columna <- oneOf(0, fila)

   } yield (fila, columna)

   property("Elementos de los lados del trianguo valen 1") = {
      forAll(coordenadasExtremos) { (i) => {
         val resultado = Funciones.calcularValorTrianguloPascal(i._1, i._2)
         println("Fila = "+i._1 +" Columna = "+ i._2+ " Resultado = "+resultado)
         resultado == 1
         }
      }
   }

   val strGen = (n: Int) => listOfN(n, oneOf('(', ')', alphaChar.sample.get)).map(_.mkString)

   property("Chequear cadenas balanceadas") = {

      val gen = strGen(10)

      forAll(gen) { (i) => {
         println(i.toList)
         val balanceada = Funciones.chequearBalance(i.toList)
         var correctas = 0
         for(j <- 1 to i.length - 1){
            val subcadena = i.take(j)
            val balance = subcadena.filter(x => x == '(').length - subcadena.filter(x => x == ')').length
            if(balance >= 0) correctas += 1
         }

         (correctas == i.length - 1 && balanceada) || !balanceada
      }
      }
   }

   property("Test contar cambios posibles") = {

      val monedas = List(1,2,5,10)
      val resultado = Funciones.contarCambiosPosibles(23, monedas)
      resultado == 52

   }

   val secuenciaEnteros = listOfN(50, choose(0,100))

   property("Busqueda binaria") = {
      val randGen = new scala.util.Random
      forAll(secuenciaEnteros) { (i) => {
         val sorted = i.sorted.toArray
         val ind1 = randGen.nextInt(sorted.length)
         val ind2 = Funciones.busquedaBinaria(sorted, sorted(ind1), (a: Int , b: Int) => a < b)
         sorted(ind1) == sorted(ind2)
      }

      }

      val ar = Array(1,2,3,4,5,6,7,8,9,10,20)
      val ind1 = Funciones.busquedaBinaria(ar, -3, (a:Int, b:Int) => a < b)
      val ind2 = Funciones.busquedaBinaria(ar, 15, (a:Int, b:Int) => a < b)
      val ind3 = Funciones.busquedaBinaria(ar, 25, (a:Int, b:Int) => a < b)

      ind1 == -1 && ind2 == -1 && ind3 == -1
   }

   val secuenciaEnteros2 = listOfN(10, choose(0,10))

   property("Test de longitud de listas") = {
      forAll(secuenciaEnteros2) {
         xs => {
            val lista1 = Lista(xs:_*)
            Lista.longitud(lista1) == xs.length
         }
      }
   }

   property("Test de suma enteros de listas") = {
      forAll(secuenciaEnteros2){
         xs => {
            val lista1 = Lista(xs:_*)
            Lista.sumaEnteros(lista1) == xs.sum
         }
      }
   }

   property("Test producto enteros listas") = {
      forAll(secuenciaEnteros2){
         xs => {
            val lista1 = Lista(xs:_*)
            Lista.productoEnteros(lista1)?= xs.product
         }
      }
   }

   property("Test concatenar listas") = {
      forAll(secuenciaEnteros2){
         xs => {
            val lista1 = Lista(xs:_*)
            Lista.toList(Lista.concatenar(lista1, lista1)) == xs:::xs
         }
      }
   }

   property("Test fold right listas") = {
      forAll(secuenciaEnteros2){
         xs => {
            val lista1 = Lista(xs:_*)
            Lista.foldRight(lista1, 0)((a,b) => a + b) == xs.foldRight(0)((a,b) => a + b)
         }
      }
   }

   property("Test suma fold right listas") = {
      forAll(secuenciaEnteros2){
         xs => {
            val lista1 = Lista(xs:_*)
            Lista.sumaFoldRight(lista1) == xs.sum
         }
      }
   }

   property("Test producto fold right listas") = {
      forAll(secuenciaEnteros2){
         xs => {
            val lista1 = Lista(xs:_*)
            Lista.productoFoldRight(lista1) == xs.product
         }
      }
   }

   property("Test asignar cabeza listas") = {
      forAll(secuenciaEnteros2){
         xs => {
            val lista1 = Lista(xs:_*)
            Lista.toList(Lista.asignarCabeza(lista1, 0)) == 0::xs
         }
      }
   }

   property("Test tail listas") = {
      forAll(secuenciaEnteros2){
         xs => {
            val lista1 = Lista(xs:_*)
            Lista.toList(Lista.tail(lista1)) == xs.tail
         }
      }
   }

   property("Test eliminar listas") = {
      forAll(secuenciaEnteros2){
         xs => {
            val lista1 = Lista(xs:_*)
            val rand = 5
            Lista.toList(Lista.eliminar(lista1, rand)) == xs.drop(rand)
         }
      }
   }

   property("Test eliminar mientras listas") = {
      forAll(secuenciaEnteros2){
         xs => {
            val lista1 = Lista(xs:_*)
            val criterio = (n: Int) => n < 7
            Lista.toList(Lista.eliminarMientras(lista1, criterio)) == xs.dropWhile(criterio)
         }
      }
   }

   property("Test eliminar ultimo listas") = {
      forAll(secuenciaEnteros2){
         xs => {
            val lista1 = Lista(xs:_*)
            Lista.toList(Lista.eliminarUltimo(lista1)) == xs.dropRight(1)
         }
      }
   }

   property("Test fold left listas") = {
      forAll(secuenciaEnteros2){
         xs => {
            val lista1 = Lista(xs:_*)
            Lista.foldLeft(lista1, 0)((a,b) => a - b) == xs.foldLeft(0)((a,b) => a - b)
         }
      }
   }

}
