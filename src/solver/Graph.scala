package solver
import maze.GridLocation

class Graph{

  var nodes: Map[Int, GridLocation] = Map()
  var adjacencyList: Map[Int, List[Int]] = Map()

  def addNode(index: Int, a: GridLocation): Unit = {
    nodes += index -> a
    adjacencyList += index -> List()
  }

  def addEdge(index1: Int, index2: Int): Unit = {
    adjacencyList += index1 -> (index2 :: adjacencyList(index1))
    adjacencyList += index2 -> (index1 :: adjacencyList(index2))
  }

}