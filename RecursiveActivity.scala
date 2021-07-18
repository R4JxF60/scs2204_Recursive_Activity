object RecursiveActivity extends App {

  // Question 01
  def prime(p: Int, n: Int=2): Boolean = n match {
    case x if(p==x) => true
    case x if(gcd(p,x)>1) => false
    case x => prime(p, x+1)
  }
  
  def gcd(x: Int, y: Int): Int = {
    if(y == 0)
      return x
    return gcd(y, x%y)
  }

  // Question 02
  def primeSeq(p: Int): Unit = {
    if(p>1) {
      primeSeq(p-1)
      if(prime(p))
        print(p+" ")
    }
  }
    
  // Question 03
  def sum(n: Int): Int = n match {
    case 0 => 0
    case _ => n+sum(n-1)
  }

  // Question 04
  def isEvenOrOdd(n: Int): String = n match {
    case 0 => "Even"
    case 1 => "Odd"
    case _ => isEvenOrOdd(n-2)
  }

  // Question 05
  def isEven(n: Int): Boolean = n match {
    case 0 => true
    case 1 => false
    case _ => isEven(n-2)
  }

  def sumOfEven(n: Int): Int = n-1 match {
    case x if(x<=0) => 0
    case x if(isEven(x))=> x+sumOfEven(x-1)
    case x => sumOfEven(x)
  }

  // Question 06
  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => fib(n-2)+fib(n-1)
  }

  def fibSeries(n: Int): Unit = {
    if(n>=0){
      fibSeries(n-1)
      print(fib(n)+" ")
    }
  }
  
  // Drive code
  // Question 01
    println("Question 01")
    println(prime(5))
    println(prime(8))

    // Question 02
    println("Question 02")
    primeSeq(10)
    println()

    // Question 03
    println("Question 03")
    println(sum(5))

    // Question 04
    println("Question 04")
    println(isEvenOrOdd(536))
    println(isEvenOrOdd(999))

    // Question 05
    println("Question 05")
    println(sumOfEven(5))
    println(sumOfEven(15))

    // Question 06
    println("Question 06")
    fibSeries(9)
    println()
}
