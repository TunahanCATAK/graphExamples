import scala.collection.mutable.Stack
import scala.collection.mutable.LinkedList

object Solution {
  
  var visited: Array[Boolean] = Array()
  var roadCount: Int = 0
  
  def main (args: Array[String]) {
    
    //val file = new java.io.File("C:\\Users\\tr1c4011\\Desktop\\testInput.txt");
    val sc = new java.util.Scanner (System.in);
    var q = sc.nextInt();
    for ( a_i <- 0 to (q - 1)){
      
      var n = sc.nextInt()
      var m = sc.nextInt()
      var clib = sc.nextLong()
      var croad = sc.nextLong()
      
      var myGraph: Array[List[Int]] = new Array(n)
      for (i_x <- 0 until n){
        myGraph(i_x) = List[Int]()
      }

      var a1 = 0
      while(a1 < m){
        var city_1 = sc.nextInt()
        var city_2 = sc.nextInt()
        
        myGraph(city_2 - 1) = (city_1 - 1) :: myGraph(city_2 - 1)
        myGraph(city_1 - 1) = (city_2 - 1) :: myGraph(city_1 - 1)
        a1+=1;
      }
      
      if (croad <= clib){
        var totalCost = DFS(myGraph, n, clib, croad)
        //var totalCost = DFSandNumberOfRoads(myGraph, n, clib, croad)
        println(totalCost.toString())  
      }
      else{
        println( (n*clib).toString())
      }
    }
  }
  
  def DFS(g: Array[List[Int]], n: Int, libCost: Long, roadCost: Long): Long ={
    var totalCost: Long = 0
    
    var libCount: Long = 0
    visited = new Array(n)
    roadCount = 0
    
    for (i <- 0 until visited.length){
      visited(i) = false;
    }
    
    for (i <- 0 until n){
      if (visited(i) == false){
        libCount += 1
        DFSVisit(g, i)
      }
    }
    
    totalCost = roadCount*roadCost + libCost*libCount;
    return totalCost;
  }
  
  def DFSVisit(g: Array[List[Int]], i: Int) {
    visited(i) = true
    
    for (v <- possibleNodes(g, i)){
      if (visited(v) == false){
        DFSVisit(g, v)
        roadCount = roadCount + 1
      }
    }
  }
  
  def possibleNodes(g: Array[List[Int]], index: Int): List[Int] = {
    var adj: List[Int] = List()
    
    for (i <- 0 until g(index).length){
      adj = g(index)(i) :: adj
    }
    
    return adj
  }

}