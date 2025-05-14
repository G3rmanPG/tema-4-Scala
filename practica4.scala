
import scala.util.Random
@volatile var turno = 0
@volatile var iter = 0
@volatile var lista:List[Boolean] = Nil
@volatile var lista2:List[Int] = Nil

/*1. a) Escribe un programa Scala que tenga 3 hebras, además de la main. Cada hebra debe
escribir t veces el carácter c, siendo t y c parámetros del constructor de la hebra. ¿Se
mezclan los resultados? */
class Ej1a(c: Char, t: Int) extends Thread{
  override def run()= {
    for(i <- 0 until t) print(c)
  }
}
/*b) Modifica el programa anterior para que las letras A, B y C se escriban siguiendo el patrón:
ABBCCC. Así, si la primera hebra escribe t=3 As, la segunda 2*t=6 Bs y la tercera 3*t=9 Cs
la única salida válida es ABBCCCABBCCCABBCCC. Para sincronizar las hebras, utiliza una
variable compartida turno que asigne turnos a las hebras, de forma que cada hebra solo
escriba su letra cuando le toca. Añade al constructor de las hebras un nuevo parámetro
miId para establecer el turno de cada hebra. Cada hebra espera su turno mediante una
instrucción de espera activa del tipo while(turno! = miId) Thread.sleep(0).*/
class Ej1b(c:Char, id: Int, t:Int) extends Thread{
  override  def run() ={
    for(i <- 0 until t){
      while(turno != id) Thread.sleep(0) // Si no es su turno, espera
      print(c)
      iter += 1
      if((id+1) == iter){
        iter=0; turno=(turno+1)%3 // Envoltorio (módulo 3 xq es el nº de letras diferentes)
      }
    }
  }
}
/*2. Implementa el método def periodico(t: Long)(b: =>Unit): Thread que
crea una hebra que ejecuta de forma indefinida el código b cada t milisegundos. Añade
la función al paquete objeto desarrollado en clase para que puedas utilizarla en cualquier
otro ejercicio. */
class Ej2a(t: Long)(b: => Unit) extends Thread{
  val h = new Thread{
    override def run() ={
      while(true){
        b
        Thread.sleep(t)
      }
    }
  }
}
/*3. a) Define la función def parallel[A,B](a: =>A, b: =>B):(A,B) que crea dos
hebras con los comportamientos dados por los parámetros a y b. El método devuelve los
valores calculados por cada una de las hebras. Añade la función al paquete objeto
desarrollado en clase para que puedas utilizarla en cualquier otro ejercicio. Nota: para
inicializar una variable local capaz de registrar los valores calculados por una hebra, debes
utilizar la instrucción var a: A = null.asInstanceOf[A]. */

def parallel[A,B](a: => A, b: => B): (A,B) ={
  @volatile var ra: A = null.asInstanceOf[A]
  @volatile var rb: B = null.asInstanceOf[B]

  val t1 = new Thread(() => ra = a)
  val t2 = new Thread(() => rb = b)
  t1.start(); t2.start();
  t1.join(); t2.join(); // La hebra principal espera a que t1 y t2 terminen

  (ra, rb)// Devolver tupla
}

/*b) Escribe un programa Scala que compruebe, si todas las componentes de una lista de
booleanos son true. Para ello:
  a. Implementa el método def todosTrue(inic:Int, fin:Int): Boolean
     que devuelve true sii todas las componentes de lista(inic..fin-1) son
     true. Haz una implementación iterativa y otra recursiva de este método.
  b. Utiliza el método parallel para crear dos hebras que comprueben cada una de
     ellas si cada una de las mitades de la lista tiene todos sus componentes a true.
  c. Razona cómo se sincronizan la hebra principal y las dos hebras creadas en este ejercicio.
  Utiliza las funciones fill (de List) y nextBoolean, nextInt (de
  scala.util.Random) para crear listas aleatorias de booleanos mediante
  List.fill(Random.nextInt(10))(Random.nextBoolean())*/

// b. iterativa
def todosTrueIter(inic: Int, fin: Int): Boolean = {
  for (i <- inic until fin)
    if (!lista(i)) return false
  true
}
// recursiva
def todosTrueRec(inic: Int, fin: Int): Boolean = {
  if (inic >= fin) true
  else if (!lista(inic)) false
  else todosTrueRec(inic + 1, fin)
}
/*4. Escribe un programa Scala que imprima por pantalla los n primeros elementos de la serie
de Fibonacci fib0 = 0, fib1 = 1, fib2 = 1, fib3 = 2, … Para ello, crea un pipeline de hebras
como el que aparece en la figura para el caso n = 5.
Cada una de las hebras del pipeline llama a la función
def fibonacci(n: Int): (Int, Int)
con un valor n > 1. Esta función calcula el valor fibn del modo siguiente: si n = 1, imprime
1 y devuelve (1,0); en otro caso, crea una nueva hebra que debe ejecutar la función
fibonacci(n-1) y debe, por tanto, imprimir fibn-1 y devolver (fibn-1, fibn-2).

Nota: Este procedimiento para calcular los elementos de la serie de Fibonacci es muy
ineficiente. El objetivo del ejercicio es mostrar cómo implementar una función como
fibonacci que combina recursión y concurrencia, ya que cada llamada recursiva es
ejecutada por una nueva hebra creada dinámicamente.
Si n = 7 la salida por pantalla del programa que se debe implementar debe ser parecida a:
main: fib(0) = 0
Thread-5: fib(1) = 1
Thread-4: fib(2) = 1
Thread-3: fib(3) = 2
Thread-2: fib(4) = 3
Thread-1: fib(5) = 5
Thread-0: fib(6) = 8
main: fib(7) = 13
main: Fin del programa*/

def fibonacci(n: Int): (Int,Int) ={
  require(n>0)
  if(n == 1){
    print(s"fib($n) = 1")
    (1,0)
  }else
    var p = (0,0)
    val h =  thread{
      p = fibonacci(n-1)
    }
    h.join()
    print(s"fib($n) = ${p._2 + p._1}")
    (p._2 + p._1, p._1)
}
/*5. Escribe un programa Scala que implemente una versión concurrente del algoritmo de
mergesort (ordenación por mezcla). El programa debe crear un árbol binario de hebras cuya
profundidad depende del número de elementos a ordenar. Para ello debes implementar
las funciones:
a. def mezclar(l1: List[Int], l2: List[Int]): List[Int]
que dadas dos listas ordenadas l1 y l2 construye una lista ordenada mezclando
los elementos de l1 y l2. Por ejemplo, dadas l1 = List(1, 3, 5, 10) y
l2 = List(2, 4, 7, 9), la función debe devolver la lista List(1, 2, 3, 4, 5, 7, 9, 10).

b. def ordenar(l: List[Int]): List[Int]
que dadas una lista l la ordena utilizando una versión recursiva y concurrente del
algoritmo mergesort del modo siguiente: si la lista l tiene un único elemento o está
vacía, no hace nada (la lista ya está ordenada) ; ii) en otro caso, la función divide la
lista l en dos mitades y crea dos hebras cada una de las cuales ejecuta el método
ordenar sobre una de las dos mitades. Una vez que las hebras han ordenado las
sublistas, la función las mezcla de forma ordenada y devuelve el resultado (la lista
original ordenada).
 */
def merge(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
  case (Nil,_) => l2
  case (_,Nil) => l1
  case(a1 :: l11, a2 :: l22) =>
    if(a1 >= a2) a2::merge(l1,l22) // Extraer de la lista el elem más pequeño
    else a1::merge(l11, l2)
}
/*b. def ordenar(l: List[Int]): List[Int]
que dadas una lista l la ordena utilizando una versión recursiva y concurrente del
algoritmo mergesort del modo siguiente: si la lista l tiene un único elemento o está
vacía, no hace nada (la lista ya está ordenada) ; ii) en otro caso, la función divide la
lista l en dos mitades y crea dos hebras cada una de las cuales ejecuta el método
ordenar sobre una de las dos mitades. Una vez que las hebras han ordenado las
sublistas, la función las mezcla de forma ordenada y devuelve el resultado (la lista
original ordenada). */
def ordenar(l: List[Int]): List[Int] = l match{
  case Nil => Nil
  case _::Nil => l
  case _::_ =>
    val(leftPart, rightPart) = l.splitAt(l.size/2)
    val(l1Sorted, l2Sorted) = parallel(ordenar(leftPart), ordenar(rightPart))
    merge(l1Sorted, l2Sorted)

}

/* Para implementar esta función puedes utilizar la función splitAt(n)de la clase
List y la función parallel implementada en el ejercicio 3 de esta relación.
La lista aleatoria a ordenar puede crearse utilizando una instrucción del tipo
List.fill(Random.nextInt(50))(Random.nextInt(100)) */
object practica4 {
  @main def Main = {
    val h1 = new Ej1a('A', 4)
    val h2 = new Ej1a('B', 3)
    val h3 = new Ej1a('C', 2)
    h1.start(); h2.start(); h3.start()
    // ejercicio 2 b)
    val h5 = new Ej2a(1000)(println("Me gusta el Jamon \n"))
    val h6 = new Ej2a(1000)(println("Antony GOAT \n"))
    h5.start(); h6.start()

    val lista: List[Boolean] = List.fill(Random.nextInt(10))(Random.nextBoolean())
    val mid = lista.length / 2

    val (leftResult, rightResult) = parallel(
      todosTrueIter(0, mid),
      todosTrueIter(mid, lista.length)
    )

    val resultadoFinal = leftResult && rightResult

    println(s"Lista generada: $lista")
    println(s"Resultado (ambas mitades true): $resultadoFinal")
    // c. : La hebra principal espera que ambas hebras terminen (join) antes de continuar

    // Ejercicio 4
    val n = 7
    val p = fibonacci(0)
    println(s"Resultado fib(0) =  $p \n")
    val f = fibonacci(n)
    println(s"Resultado fib($n) = $f \n")
    println("Fin ejercicio 7 \n")

    // Ejercicio 5
    val lista5 = List.fill(Random.nextInt(50))(Random.nextInt(100))
    println(s"${lista5.mkString("[", ",", "]")}")
    val lista5Sorted = ordenar(lista5)
    println(s"${lista5Sorted.mkString("[", ",", "]")}")
  }
}
