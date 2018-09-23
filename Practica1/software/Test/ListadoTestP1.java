
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;
import java.io.IOException;
import java.util.List;
import java.util.Map;

/**
 * Práctica 1 NTP
 */
public class ListadoTestP1 {
   private static ListadoEmpleados listado;

   /**
    * Codigo a ejecutar antes de realizar las llamadas a los métodos
    * de la clase; incluso antes de la propia instanciación de la
    * clase. Por eso el método debe ser estatico
    */
   @Before
   public void inicializacion() {
      System.out.println("Metodo inicializacion conjunto pruebas");
      // Se genera el listado de empleados
      try {
         listado = new ListadoEmpleados("./data/datos.txt");
      } catch (IOException e) {
         System.out.println("Error en lectura de archivo de datos");
      };
   }

   /**
    * Test para comprobar que se ha leido de forma correcta la
    * informacion de los empleados
    *
    * @throws Exception
    */
   @Test
   public void testConstruccionListado() throws Exception {
      assert (listado.obtenerNumeroEmpleadosArchivo() == 5000);
   }

   /**
    * Test para comprobar la deteccion de dnis repetidos
    */
   @Test
   public void testComprobarHayDnisRepetidos() {

      assert (listado.hayDnisRepetidosArchivo() == true);
   }

   /**
    * Test para comprobar el numero de empleados con correos
    * repetidos
    */
   @Test
   public void testComprobarContadoresDnisRepetidosArchivo() {

      assert (listado.contarEmpleadosDnisRepetidos() == 4);
   }

   /**
    * Test para probar la funcionalidad que repara DNI
    */

   @Test
   public void testRepararDnisRepetidos(){

      Map<String, List<Empleado>> dniRepetidosArchivo = listado.obtenerDnisRepetidosArchivo();
      assert (listado.contarEmpleadosDnisRepetidos() == 4);
      listado.repararDnisRepetidos(dniRepetidosArchivo);
      assert (listado.hayDnisRepetidosArchivo() == false);

   }

   /**
    * Test para probar la funcionalidad que repara Correos
    */

   @Test
   public void testRepararCorreosRepetidos(){

      Map<String, List<Empleado>> correoRepetidosArchivo = listado.obtenerCorreosRepetidosArchivo();
      assert (listado.contarEmpleadosCorreosRepetidos() == 315);
      listado.repararCorreosRepetidos(correoRepetidosArchivo);
      assert (listado.hayCorreosRepetidos() == false);
   }

}