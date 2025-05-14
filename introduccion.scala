// Clase THREAD para manejar las hebras (procesos)
// Clase THREAD -> representa una hebra que ejecuta las instrucciones de su método run()
/* class MiHebra extends Thread {
    override def run = ...
   }
 Opciones para crear una (hebra) Thread:
  - Opción 1 -> crear clase que herede la clase Thread y sobreescribir el método run() con el comportamiento deseado
  - Opción 2 -> crear un obj que implemente la interfaz Runnable, y pasar su referencia a un objeto de la clase Thread a través del constructor
*/
// Proceso ligero -> Hebra; 'run' es lo que se ejecuta al invocarla (Hilo)
class Hebra extends Thread {
  override def run() = println("Hello World")
}
// Hebra implementando interfaz Runnable -> da método run (tontería)
class Escribir(c: Char) extends Runnable { // extends Thread
  override def run =
    for (i<-0 until 10) print(c)
}
// HEBRA QUE SE QJECUTA AL LLAMARLA DANDOLE UN CUERPO -> body:=> Unit
def log(msg: String): Unit ={
  println(s"${Thread.currentThread().getName}: $msg")
}
def thread(body: => Unit): Thread = {
  val t = new Thread {
    override def run() = body
  }
  t.start()
  t
}

// hilo que se ejecuta en bucle cada t tiempo (si t= 0, se puede eliminar sleep), se puede implementar sobre el anterior (crear val, val.star, dev val)
def periodico(t: Long)(b: => Unit): Thread = {
  new Thread {
    override def run() = {
      while(true){
        b
        Thread.sleep(t) // Suspender proceso con el tiempo pasado x parámetro
      }
    }

  }
}

// hilo que devuelve un valor dada una función, editable para + fun -> tupla; o ninguna y que devuelva
def devolved[A](a: => A): A ={
  var resA: A = null.asInstanceOf[A]
  val h1 = new Thread{
    override def run(): Unit ={ // Usable el anterior thread(resA = a); h1.join
      resA = a
    }
  }
  h1.start()
  h1.join()
  resA
}
object introduccion {
  @main def MainHebra = { // el comentario es sin usar la extensión Runnable (con Thread)
    val h1 = new Hebra
    // conversión a Hilo:
    val h2 = new Thread(new Escribir('A')) // val h1 = new Escribir('A')
    val h3 = new Thread(new Escribir('B')) // val h2 = new Escribir('B')
    val h4 = new Thread(new Escribir('C')) // val h3 = new Escribir('C’)
    // Métodos hebras:
    h2.start(); h3.start(); h4.start() // h1, h2, h3 comienzan su ejecución
    h4.join() // Método de sincronización, espera a que h4 acabe
    Thread.sleep(5000) // suspender durante 5 s
    println(Thread.currentThread().getName) // Devuelve el nombre de la hebra que se está ejecutando (hi.getName)

    val t = thread{
      Thread.sleep(1000)
      log("Una hebra en ejecución") // println("Una hebra en ejecución")
      Thread.sleep(1000)
      log("Aún en ejecución")
      Thread.sleep(1000)
      log("Terminado")
    }
    val hilo1 = periodico(1000)(println("Hello "))
    val hilo2 = periodico(3000)(println("World "))
    hilo1.start(); hilo2.start()
    println(devolved("Hello " + "World"))

  }
}
