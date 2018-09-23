/**
  * Interfaz generica para la lista
  * @tparam A
  */
sealed trait Lista[+A]

/**
  * Objeto para definir lista vacia
  */
case object Nil extends Lista[Nothing]

/**
  * Clase para definir la lista como compuesta por elemento inicial
  * (cabeza) y resto (cola)
  * @param cabeza
  * @param cola
  * @tparam A
  */
case class Cons[+A](cabeza : A, cola : Lista[A]) extends Lista[A]

/**
  * Objeto para desarrollar las funciones pedidas
  */
object Lista {

   /**
     * Metodo para permitir crear listas sin usar new
     * @param elementos secuencia de elementos a incluir en la lista
     * @tparam A
     * @return
     */
   def apply[A](elementos: A*): Lista[A] = {
      if(elementos.isEmpty) Nil
      else Cons(elementos.head, apply(elementos.tail: _*))
   }

   /**
     * Obtiene la longitud de una lista
     * @param lista
     * @tparam A
     * @return
     */

   def longitud[A](lista: Lista[A]): Int = {
      lista match  {
         case Nil => 0
         case Cons(cadena, cola) => 1 + longitud(cola)
      }
   }

   /**
     * Metodo para sumar los valores de una lista de enteros
     * @param enteros
     * @return
     */
   def sumaEnteros(enteros: Lista[Int]) : Double = enteros match {
      case Nil => 0.0
      case Cons(cabeza, cola) => cabeza.toDouble + sumaEnteros(cola)
   }

   /**
     * Metodo para multiplicar los valores de una lista de enteros
     * @param enteros
     * @return
     */
   def productoEnteros(enteros : Lista[Int]) : Double = enteros match {
      case Nil => 1.0
      case Cons(cabeza, cola) => if(cabeza == 0) 0.0 else cabeza.toDouble * productoEnteros(cola)
   }

   /**
     * Metodo para agregar el contenido de dos listas
     * @param lista1
     * @param lista2
     * @tparam A
     * @return
     */
   def concatenar[A](lista1: Lista[A], lista2: Lista[A]): Lista[A] = lista1 match {
      case Nil => lista2
      case Cons(cabeza, cola) => Cons(cabeza, concatenar(cola, lista2))
   }

   /**
     * Funcion de utilidad para aplicar una funcion de forma sucesiva a los
     * elementos de la lista
     * @param lista
     * @param neutro
     * @param funcion
     * @tparam A
     * @tparam B
     * @return
     */
   def foldRight[A, B](lista : Lista[A], neutro : B)
                      (funcion : (A, B) => B): B = lista match {
      case Nil => neutro
      case Cons(cabeza, cola) => funcion(cabeza, foldRight(cola, neutro)(funcion: (A, B) => B))
   }

   /**
     * Suma mediante foldRight
     * @param listaEnteros
     * @return
     */
   def sumaFoldRight(listaEnteros : Lista[Int]) : Double = {
      foldRight(listaEnteros, 0.0)((a, b) => a + b)
   }

   /**
     * Producto mediante foldRight
     * @param listaEnteros
     * @return
     */
   def productoFoldRight(listaEnteros : Lista[Int]) : Double = {
      foldRight(listaEnteros, 1.0)((a, b) => a * b)
   }

   /**
     * Reemplaza la cabeza por nuevo valor. Se asume que si la lista esta vacia
     * se devuelve una lista con el nuevo elemento
     *
     * @param lista
     * @param cabezaNueva
     * @tparam A
     * @return
     */
   def asignarCabeza[A](lista : Lista[A], cabezaNueva : A) : Lista[A] = lista match {
      case Nil => Cons(cabezaNueva, Nil)
      case Cons(cabeza, cola) => Cons(cabezaNueva, lista)
   }

   /**
     * Elimina el elemento cabeza de la lista
     * @param lista
     * @tparam A
     * @return
     */
   def tail[A](lista : Lista[A]): Lista[A] = lista match {
      case Nil => Nil
      case Cons(cabeza, cola) => cola
   }

   /**
     * Elimina los n primeros elementos de una lista
     * @param lista lista con la que trabajar
     * @param n numero de elementos a eliminar
     * @tparam A tipo de datos
     * @return
     */
   def eliminar[A](lista : Lista[A], n: Int) : Lista[A] = (lista,n) match {
      case (Nil, _) => Nil
      case (_, 0) => lista
      case (Cons(cabeza, cola), _) => eliminar(cola, n - 1)
   }

   /**
     * Elimina elementos mientra se cumple la condicion pasada como
     * argumento
     * @param lista lista con la que trabajar
     * @param criterio predicado a considerar para continuar con el borrado
     * @tparam A tipo de datos a usar
     * @return
     */
   def eliminarMientras[A](lista : Lista[A], criterio: A => Boolean) : Lista[A] = lista match {
      case Nil => Nil
      case Cons(cabeza, cola) => {
         if(!criterio(cabeza)) lista
         else eliminarMientras(cola, criterio)
      }
   }

   /**
     * Elimina el ultimo elemento de la lista. Aqui no se pueden compartir
     * datos en los objetos y hay que generar una nueva lista copiando
     * datos
     * @param lista lista con la que trabajar
     * @tparam A tipo de datos de la lista
     * @return
     */
   def eliminarUltimo[A](lista : Lista[A]) : Lista[A] = lista match {
      case Cons(cabeza, Nil) => Nil
      case Cons(cabeza, cola) => Cons(cabeza, eliminarUltimo(cola))
   }

   /**
     * foldLeft con recursividad tipo tail
     * @param lista lista con la que trabajar
     * @param neutro elemento neutro
     * @param funcion funcion a aplicar
     * @tparam A parametros de tipo de elementos de la lista
     * @tparam B parametro de tipo del elemento neutro
     * @return
     */

   @annotation.tailrec
   def foldLeft[A, B](lista : Lista[A], neutro: B)(funcion : (B, A) => B): B = lista match {
      case Nil => neutro
      case Cons(cabeza, cola) => foldLeft(cola, funcion(neutro, cabeza))(funcion)
   }

   def toList[A](lista: Lista[A]): List[A] = lista match{
      case Nil => List()
      case Cons(cabeza, cola) => cabeza::toList(cola)
   }
}

object Prueba extends  App {
   val lista1 = Lista(1,2,3,4,5,6,7)
   val lista2 = Lista(10,11,12)
   val lista3 = Lista.concatenar(lista1, lista2)

   println("Concatenadas: " + lista3)
   println("Suma lista1: " + Lista.sumaFoldRight(lista1))
   println("Producto lista1: " + Lista.productoFoldRight(lista1))
   println("Nueva cabeza: " + Lista.asignarCabeza(lista1, 0))
   println("Tail lista1: " + Lista.tail(lista1))
   println("Eliminar 3 primeros de lista1: " + Lista.eliminar(lista1, 3))
   println("Eliminar mientras < 4: " + Lista.eliminarMientras(lista1, (x: Int) => x < 4))
   println("Eilimar ultimo de lista1: " + Lista.eliminarUltimo(lista1))
   println("FoldLeft de resta de lista1: " + Lista.foldLeft(lista1, 0)((a,b) => a - b))
   println("Lista 1 a List: " + Lista.toList(lista1))
}
