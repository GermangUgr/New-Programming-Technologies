
/**
  * Objeto singleton para probar la funcionalidad del triangulo
  * de Pascal
  */
object Funciones {
  /**
    * Metodo main: en realidad no es necesario porque el desarrollo
    * deberia guiarse por los tests de prueba
    *
    * @param args
    */
  def main(args: Array[String]) {
    println("................... Triangulo de Pascal ...................")

    // Se muestran 10 filas del trinagulo de Pascal
    for (row <- 0 to 10) {
      // Se muestran 10 10 columnas
      for (col <- 0 to row)
        print(calcularValorTrianguloPascal(row, col) + " ")

      // Salto de linea final para mejorar la presentacion
      println()
    }

    // Se muestra el valor que debe ocupar la columna 5 en la fila 10
    println(calcularValorTrianguloPascal(15, 10))
    println(calcularValorTrianguloPascal(0, 0))

     val ar = Array(1,2,3,4,5,6,7,8,9,10,20)
     val ind = busquedaBinaria(ar, -3, (a:Int, b:Int) => a < b)
     println("Resultado: " + ind)

     val cambios = contarCambiosPosibles(23, List(1,2,5,10)) // 52
     println("Cambios Posibles: " + cambios)

  }

  /**
    * Ejercicio 1: funcion para generar el triangulo de Pascal
    *
    * @param columna
    * @param fila
    * @return
    */

  def calcularValorTrianguloPascal(fila: Int, columna: Int): Int = {

     if(columna == 0 || columna == fila) 1
     else calcularValorTrianguloPascal(fila - 1, columna) + calcularValorTrianguloPascal(fila - 1, columna -1)

  }

  /**
    * Ejercicio 2: funcion para chequear el balance de parentesis
    *
    * @param cadena cadena a analizar
    * @return valor booleano con el resultado de la operacion
    */
  def chequearBalance(cadena: List[Char]): Boolean = {

     def go(cad: List[Char], cont: Int): Boolean = {

        if(cont < 0) false
        else if(cad.isEmpty) cont == 0
        else if(cad.head == ')') go(cad.tail, cont - 1)
        else if(cad.head == '(') go(cad.tail, cont + 1)
        else go(cad.tail, cont)

     }

     go(cadena, 0)
  }

  /**
    * Ejercicio 3: funcion para determinar las posibles formas de devolver el
    * cambio de una determinada cantidad con un conjunto de monedas
    *
    * @param cantidad
    * @param monedas
    * @return contador de numero de vueltas posibles
    */
   def contarCambiosPosibles(cantidad: Int, monedas: List[Int]): Int = {
      if(cantidad ==0)1
      else{
         if(monedas.isEmpty)0
         else if(cantidad >= monedas.head){
            (0 to cantidad/monedas.head).map(num => contarCambiosPosibles(cantidad-num*monedas.head,monedas.tail)).foldLeft(0)(_+_)
         }
         else contarCambiosPosibles(cantidad,monedas.tail)
      }
   }

  /**
   * Metodo generico para busqueda binaria
   * @param coleccion conjunto de datos sobre los que buscar
   * @param aBuscar elemento a buscar
   * @param criterio para comparar dos elementos de tipo A
   * @tparam A parametro de tipo
   * @return posicion del valor buscado o -1 en caso de no hallarlo
   */
  def busquedaBinaria[A](coleccion : Array[A], aBuscar: A, criterio : (A,A) => Boolean) : Int = {

     @annotation.tailrec
     def go(izd: Int, dcha: Int): Int ={
        if(izd > dcha) -1
        else if(criterio(coleccion((izd + dcha)/2), aBuscar)) go((izd + dcha)/2 + 1, dcha)
        else if(criterio(aBuscar, coleccion((izd + dcha)/2))) go(izd, (izd + dcha)/2 - 1)
        else (izd + dcha)/2
     }

     go(0, coleccion.length-1)
  }
}
