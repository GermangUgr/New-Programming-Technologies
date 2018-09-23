import java.io.IOException;
import java.lang.reflect.Array;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.function.Function;

public class ListadoEmpleados {

    private List<Empleado> listadoArchivo = new ArrayList<Empleado>();

    private Map<String, Empleado> listado;

    public ListadoEmpleados(String dataDir) throws IOException {

        Stream<String> lineas = Files.lines(Paths.get(dataDir), StandardCharsets.ISO_8859_1);

        lineas.forEach(linea -> {
            Empleado e = crearEmpleado(linea);
            listadoArchivo.add(e);
        });

    }

    public Empleado crearEmpleado(String datosEmpleado){

        Pattern patron = Pattern.compile(",\\s\\s");

        List<String> datos = patron.splitAsStream(datosEmpleado).collect(Collectors.toList());

        return new Empleado(datos.get(0), datos.get(1), datos.get(2), datos.get(3));


    }

    public int obtenerNumeroEmpleadosArchivo(){

        return listadoArchivo.size();

    }

    public boolean hayDnisRepetidosArchivo(){

        List<String> dniNoRepetidos = listadoArchivo.stream().map(Empleado::obtenerDni).distinct().collect(Collectors.toList());

        return dniNoRepetidos.size() != listadoArchivo.size();

    }

    public boolean hayCorreosRepetidos(){

        List<String> correoNoRepetidos = listadoArchivo.stream().map(Empleado::obtenerCorreo).distinct().collect(Collectors.toList());

        return correoNoRepetidos.size() != listadoArchivo.size();

    }

    public Map<String, List<Empleado>> obtenerDnisRepetidosArchivo(){

        Map<String, List<Empleado>> agrupadosDNI = listadoArchivo.stream().
                collect(Collectors.groupingBy(Empleado::obtenerDni, HashMap::new, Collectors.toList()));

        return agrupadosDNI.entrySet().stream().
                filter(entrada -> entrada.getValue().size() > 1).
                collect(Collectors.toMap(entry -> entry.getKey(), entry -> entry.getValue()));

    }

    public Map<String, List<Empleado>> obtenerCorreosRepetidosArchivo(){

        Map<String, List<Empleado>> agrupadosCorreo = listadoArchivo.stream().
                collect(Collectors.groupingBy(Empleado::obtenerCorreo, HashMap::new, Collectors.toList()));

        return agrupadosCorreo.entrySet().stream().
                filter(entrada -> entrada.getValue().size() > 1).
                collect(Collectors.toMap(entry -> entry.getKey(), entry -> entry.getValue()));

    }

    public int contarEmpleadosDnisRepetidos(){

        Map<String, List<Empleado>> agrupadosDNI = obtenerDnisRepetidosArchivo();

        Integer numEmpleadosDniRepetido = agrupadosDNI.entrySet().stream().
                map((entrada) -> entrada.getValue().size()).reduce(0, (x, y) -> x + y);

        return numEmpleadosDniRepetido;
    }

    public int contarEmpleadosCorreosRepetidos(){

        Map<String, List<Empleado>> agrupadosCorreo = obtenerCorreosRepetidosArchivo();

        Integer numEmpleadosCorreoRepetido = agrupadosCorreo.entrySet().stream().
                map((entrada) -> entrada.getValue().size()).reduce(0, (x, y) -> x + y);

        return numEmpleadosCorreoRepetido;
    }

    public void repararDnisRepetidos(Map<String, List<Empleado>> listaRepeticion){

        listaRepeticion.entrySet().stream().forEach((dni) -> {

            dni.getValue().stream().forEach((empleado) -> empleado.asignarDniAleatorio());

        });

    }

    public void repararCorreosRepetidos(Map<String, List<Empleado>> listaRepeticion){

        listaRepeticion.entrySet().stream().forEach((correo) -> {

            correo.getValue().stream().forEach((empleado) -> empleado.generarCorreoCompleto());

        });

    }

    public void validarListaArchivo(){

        repararDnisRepetidos(obtenerDnisRepetidosArchivo());
        repararCorreosRepetidos(obtenerCorreosRepetidosArchivo());

        listado = listadoArchivo.stream().collect(Collectors.toMap(emp -> emp.obtenerDni(), emp -> emp));

    }

    public long cargarArchivoAsignacionSector(String path) throws IOException {

        Pattern patron = Pattern.compile("\\s");
        Stream<String> lineas = Files.lines(Paths.get(path), StandardCharsets.ISO_8859_1).skip(2);
        Optional<String> sectorS = Files.lines(Paths.get(path), StandardCharsets.ISO_8859_1).findFirst();
        Sector sector = Sector.valueOf(sectorS.get());

        long errores = lineas.map((linea) -> procesarAsignacionSector(linea, sector, patron)).
                filter((linea) -> linea == false).count();

        return errores;

    }

    public boolean procesarAsignacionSector(String dni, Sector sector, Pattern patron){

        Empleado e = listado.get(dni);

        if (e == null)
            return false;
        else {
            e.asignarSector(sector);
            return true;
        }

    }

    public long cargarArchivoAsignacionRuta(String path) throws IOException {

        Pattern patron = Pattern.compile("\\s");
        Stream<String> lineas = Files.lines(Paths.get(path), StandardCharsets.ISO_8859_1).skip(2);
        Optional<String> rutaS = Files.lines(Paths.get(path), StandardCharsets.ISO_8859_1).findFirst();
        Ruta ruta = Ruta.valueOf(rutaS.get());

        long errores = lineas.map((linea) -> procesarAsignacionRuta(linea, ruta, patron)).
                filter((linea) -> linea == false).count();

        return errores;

    }

    public boolean procesarAsignacionRuta(String dni, Ruta ruta, Pattern patron){

        Empleado e = listado.get(dni);

        if (e == null)
            return false;
        else {
            e.asignarRuta(ruta);
            return true;
        }

    }

    public Map<Ruta, Long> obtenerContadoresRuta(Sector sector){

        return listado.values().stream().filter((emp) -> emp.obtenerSector() == sector).
                map((emp) -> emp.obtenerRuta()).
                sorted(Comparator.naturalOrder()).
                collect(Collectors.groupingBy(Function.identity(), TreeMap::new, Collectors.counting()));

    }

    /*
    public Map<Ruta, Long> obtenerContadoresRuta(Sector sector){

        return listado.values().stream().filter((emp) -> emp.obtenerSector() == sector).
                collect(Collectors.groupingBy(Empleado::obtenerRuta, HashMap::new, Collectors.counting()));

    }*/

    public Map<Sector, Map<Ruta, Long>> obtenerContadoresSectorRuta(){

        return Arrays.stream(Sector.values()).
                collect(Collectors.toMap(sector -> sector, sector -> obtenerContadoresRuta(sector)));

    }

    public List<Long> obtenerContadoresSectores(){

        Map<Sector, Map<Ruta, Long>> sectorRuta = obtenerContadoresSectorRuta();
        Long l = new Long(0);

        return sectorRuta.entrySet().stream().collect(Collectors.toMap(entry -> entry.getKey(),
                entry -> entry.getValue().values().stream().reduce(l, (x, y) -> x + y), (a, b) -> a , TreeMap::new)).
                values().stream().collect(Collectors.toList());

    }

    public List<Empleado> buscarEmpleadosSinSectorSinRuta(){

        return listado.values().stream().
                filter(emp -> emp.obtenerSector() == Sector.NOSECTOR && emp.obtenerRuta() == Ruta.NORUTA).
                collect(Collectors.toList());
    }

    public List<Empleado> buscarEmpleadosSinRuta(Sector s){

        return listado.values().stream().
                filter(emp -> emp.obtenerSector() == s && emp.obtenerRuta() == Ruta.NORUTA).
                collect(Collectors.toList());
    }

    public List <Empleado> buscarEmpleadosConSectorSinRuta(){

        return Arrays.stream(Sector.values()).filter((s) -> s != Sector.NOSECTOR).
                flatMap((sec) -> buscarEmpleadosSinRuta(sec).stream()).collect(Collectors.toList());

    }

    public List<Empleado> buscarEmpleadosSinSector(Ruta r){

        return listado.values().stream().
                filter(emp -> emp.obtenerSector() == Sector.NOSECTOR && emp.obtenerRuta() == r).
                collect(Collectors.toList());
    }

    public List <Empleado> buscarEmpleadosSinSectorConRuta(){

        return Arrays.stream(Ruta.values()).filter((r) -> r != Ruta.NORUTA).
                flatMap((rut) -> buscarEmpleadosSinSector(rut).stream()).collect(Collectors.toList());

    }
    
    public void equilibrarSectores(){

        List<Long> cont = obtenerContadoresSectores();
        System.out.println(cont);
        Long diff = cont.get(0) - cont.get(1);

        List<Empleado> nosectors = listado.values().stream().filter(emp -> emp.obtenerSector() == Sector.NOSECTOR).
                collect(Collectors.toList());

        for(Empleado e: nosectors){

            if(diff < 0){

                e.asignarSector(Sector.SECTOR1);
                diff++;

            } else if (diff > 0){

                e.asignarSector(Sector.SECTOR2);
                diff--;

            } else { // diff == 0

                e.asignarSector(Sector.SECTOR1);
                diff++;

            }

        }

    }



    public static void main(String args[]) throws IOException {

        ListadoEmpleados listado = new ListadoEmpleados("/home/german/Documentos/NTP/Practica1/data/datos.txt");
        listado.validarListaArchivo();

        long fallosS1 = listado.cargarArchivoAsignacionSector("/home/german/Documentos/NTP/Practica1/data/asignacionSECTOR1.txt");
        long fallosS2 = listado.cargarArchivoAsignacionSector("/home/german/Documentos/NTP/Practica1/data/asignacionSECTOR2.txt");
        long fallosR1 = listado.cargarArchivoAsignacionRuta("/home/german/Documentos/NTP/Practica1/data/asignacionRUTA1.txt");
        long fallosR2 = listado.cargarArchivoAsignacionRuta("/home/german/Documentos/NTP/Practica1/data/asignacionRUTA2.txt");
        long fallosR3 = listado.cargarArchivoAsignacionRuta("/home/german/Documentos/NTP/Practica1/data/asignacionRUTA3.txt");


        listado.listado.entrySet().stream().forEach((entrada) -> {
            System.out.println(entrada.getKey());
            System.out.println(entrada.getValue());
        });

        System.out.println(fallosS1);
        System.out.println(fallosS2);
        System.out.println(fallosR1);
        System.out.println(fallosR2);
        System.out.println(fallosR3);

        Map<Ruta, Long> prueba = listado.obtenerContadoresRuta(Sector.SECTOR1);

        prueba.entrySet().stream().forEach((entrada) -> {
            System.out.println(entrada.getKey());
            System.out.println(entrada.getValue());
        });


    }

}
