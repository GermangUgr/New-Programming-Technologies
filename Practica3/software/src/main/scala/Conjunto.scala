
/**
  * Clase para representar conjuntos definidos mediante una funcion
  * caracteristica (un predicado). De esta forma, se declara el tipo
  * conjunto como un predicado que recibe un entero (elemento) como
  * argumento y dvuelve un valor booleano que indica si pertenece o no
  * al conjunto
  *
  * @param funcionCaracteristica
  */
class Conjunto(val funcionCaracteristica: Int => Boolean) {
/**
    * Crea una cadena con el contenido completo del conjunto
    *
    * @return
    */
  override def toString(): String = {
    // El uso de this(i) implica la el uso del metodo apply
    val elementos = for (i <- -Conjunto.LIMITE to Conjunto.LIMITE 
                         if this(i)) yield i
    elementos.mkString("{", ",", "}")
  }
  
  /**
    * Metodo para determinar la pertenencia de un elemento al
    * conjunto
    * @param elemento
    * @return valor booleano indicando si elemento cumple
    *         la funcion caracteristica o no
    */
  def apply(elemento: Int): Boolean = {
     funcionCaracteristica(elemento)
  }
}



/**
  * Objeto companion que ofrece metodos para trabajar con
  * conjuntos
  */
object Conjunto {
   /**
    * Limite para la iteracion necesaria algunas operaciones,
    * entre -1000 y 1000
    */
  private final val LIMITE = 1000
  
  /**
    * Metodo que permite crear objetos de la clase Conjunto
    * de forma sencilla
    * @param f
    * @return
    */
  def apply(f: Int => Boolean): Conjunto = {
    new Conjunto(f)
  }

   def conjuntoUnElemento(elemento : Int) : Conjunto = {
      Conjunto((a) => a == elemento)
   }

   def union(c1 : Conjunto, c2 : Conjunto) : Conjunto = {
      Conjunto((a) => c1.funcionCaracteristica(a) || c2.funcionCaracteristica(a))
   }

   def interseccion(c1 : Conjunto, c2 : Conjunto) : Conjunto = {
      Conjunto((a) => c1.funcionCaracteristica(a) && c2.funcionCaracteristica(a))
   }

   def diferencia(c1 : Conjunto, c2 : Conjunto) : Conjunto = {
      Conjunto((a) => c1.funcionCaracteristica(a) && !c2.funcionCaracteristica(a))
   }

   def filtrar(c : Conjunto, predicado : Int => Boolean) : Conjunto = {
      Conjunto((a) => c.funcionCaracteristica(a) && predicado(a))
   }

   def paraTodo(conjunto : Conjunto, predicado : Int => Boolean) : Boolean = {
      def iterar(elemento : Int) : Boolean = {
         if(elemento > LIMITE) true
         else if (!conjunto.funcionCaracteristica(elemento)) iterar(elemento + 1)
         else predicado(elemento) && iterar(elemento + 1)
      }
      iterar(-LIMITE)
   }

   //Version de existe que no utiliza paraTodo
   def existe2(c : Conjunto, predicado : Int => Boolean) : Boolean = {
      def iterar(elemento : Int) : Boolean = {
         if(elemento > LIMITE) false
         else if (!c.funcionCaracteristica(elemento)) iterar(elemento + 1)
         else if (predicado(elemento)) true
         else iterar(elemento + 1)
      }
      iterar(-LIMITE)
   }

   def existe(c : Conjunto, predicado : Int => Boolean) : Boolean = !paraTodo(c, (a) => !predicado(a))

   def map(c : Conjunto, funcion : Int => Int) : Conjunto = {
      Conjunto((a) => existe(c, (b) => a == funcion(b)))
   }
}