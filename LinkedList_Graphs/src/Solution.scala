import scala.collection.mutable.Stack
import scala.collection.mutable.LinkedList

object Solution {
  def main (args: Array[String]) {
    
    val file = new java.io.File("C:\\Users\\tr1c4011\\Desktop\\testInput.txt");
    val sc = new java.util.Scanner (file);
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
        var totalCost = DFTandNumberOfRoads(myGraph, n, clib, croad)
        println(totalCost.toString())  
      }
      else{
        println( (n*clib).toString())
      }
    }
    //myGraph = InitGraph()


  }
  
  def DFTandNumberOfRoads(g: Array[List[Int]], n: Int, unitCostOfLib: Long, unitCostOfRoad: Long): Long = {
    var roadNumber = 0;
    var libNumber = 0;
    
    var nodeStack: Stack[Int] = Stack()
    var visitted: Array[Boolean] = new Array(n)
    
    for (i <- 0 until visitted.length){
      visitted(i) = false;
    }
    
    //nodeStack.push(startIndex)
    //visitted(startIndex) = true
    var index = 0;
    
    index = isCompleted(visitted)
    while (index >=(0)){
      visitted(index) = true
      nodeStack.push(index)
      libNumber += 1
      
      while (index != -5) {
        index = nextPossibleNode(g, index, visitted)
        if (index != -1){
          nodeStack.push(index)  
          visitted(index) = true
          roadNumber += 1
        }
        else {
          if (nodeStack.isEmpty)
            index = -5;
          else index = nodeStack.pop()
        }
      }
        
      index = isCompleted(visitted)
    }
    
    return roadNumber * unitCostOfRoad + libNumber * unitCostOfLib;
  }
  
  def isCompleted(ar: Array[Boolean]) : Int = {
    var index = -1;
    
    for (i <- 0 until ar.length){
      if (ar(i) != true)
        return i;
    }
    
    return index;
  }
  
  def nextPossibleNode(g: Array[List[Int]], n: Int, visitted: Array[Boolean]): Int = {
    var nextIndex = -1;
    
    for (i <- 0 until g.apply(n).length){
      if (visitted(g.apply(n).apply(i)) == false){
        nextIndex = g.apply(n).apply(i)
        return nextIndex
      }
    }
    
    return nextIndex
  }
  
  def InitGraph(): List[List[Int]] = {
    var subSet: List[Int] = List(1, 2)
    var subSet1: List[Int] = List(0, 3, 4)
    var subSet2: List[Int] = List(0, 3)
    var subSet3: List[Int] = List(1, 2)
    var subSet4: List[Int] = List(1, 5)
    var subSet5: List[Int] = List(4)
    
    var graph: List[List[Int]] = List(
      subSet,
      subSet1,
      subSet2,
      subSet3,
      subSet4,
      subSet5
    )
    
    return graph      
  }
}