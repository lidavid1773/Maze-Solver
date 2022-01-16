package solver

import maze.{GridLocation, MapTile, PhysicsVector}

import scala.collection.mutable

object PathFinding {

  def findPath(start: GridLocation, end: GridLocation, map: List[List[MapTile]]): List[GridLocation] = {

    // Creates a graph with all the passable tiles from GameMap
    val graph: Graph = new Graph()

    var indexCount = 0
    var rowCount = -1
    var columnCount = -1

    for (row <- map){
      rowCount += 1
      for (tile <- row){
        columnCount += 1
        if (columnCount > map.head.length-1){
          columnCount = 0
        }
        if (tile.passable){
          graph.addNode(indexCount, new GridLocation(columnCount,rowCount))
          indexCount += 1
        }
      }
    }

    for ((key1, value1) <- graph.nodes){
      for ((key2,value2) <- graph.nodes){
        if (value1.x == value2.x+1 && value1.y == value2.y || value1.y == value2.y+1 && value1.x == value2.x){
          graph.addEdge(key1,key2)
        }
      }
    }

    var startingIndex = 0
    var endingIndex = 0
    for ((key, value) <- graph.nodes){
      if (value == start){
        startingIndex = key
      }
      if (value == end){
        endingIndex = key
      }
    }

    // Creates a path and backtracks it
    var explored: Set[Int] = Set(startingIndex)

    var distance: Map[Int,Int] = Map()
    distance += startingIndex -> -1

    val toExplore: mutable.Queue[Int] = new mutable.Queue()
    toExplore.enqueue(startingIndex)

    while (toExplore.nonEmpty) {
      val nodeToExplore = toExplore.dequeue()
      for (node <- graph.adjacencyList(nodeToExplore)) {
        if (!explored.contains(node) && !explored.contains(endingIndex)){
          distance += node -> nodeToExplore
          toExplore.enqueue(node)
          explored = explored + node
        }
      }
    }

    var previousID: Int = distance(endingIndex)
    var lst: List[Int] = List()
    lst = endingIndex :: lst
    lst = previousID :: lst
    var runs = 0
    while (runs < 999999){
      if (previousID != startingIndex){
        previousID = distance(previousID)
        lst = previousID :: lst
      }
      runs += 1
    }

    var retLst: List[GridLocation] = List()

    for (numb <- lst) {
      for ((key, value) <- graph.nodes) {
        if (numb == key){
          retLst = retLst :+ value
        }
      }
    }

    retLst
  }


  def getVelocity(path: List[GridLocation], currentLocation: PhysicsVector): PhysicsVector = {
    var currentTile: GridLocation = new GridLocation(currentLocation.x.floor.toInt, currentLocation.y.floor.toInt)

    var tileCount1 = 0
    var tileCount2 = -1
    for (tile <- path) {
      if (tile.x == currentTile.x) {
        if (tile.y == currentTile.y) {
          currentTile = tile
          tileCount2 = tileCount1
        }
      }
      tileCount1 += 1
    }

    def magnitudeTimesFive(magnitude: PhysicsVector): Unit = {
      magnitude.x = magnitude.x * 5
      magnitude.y = magnitude.y * 5
    }

    val lastTileIndex = path.length - 1
    var count3 = 0

    if (tileCount2 != lastTileIndex) {
      var nextTile: PhysicsVector = new PhysicsVector(0, 0, 0)
      for (tile <- path) {
        if (tileCount2 + 1 == count3) {
          nextTile = new PhysicsVector(tile.x + .5, tile.y + .5, 0)
        }
        count3 += 1
      }
      var magnitude: PhysicsVector = new PhysicsVector(nextTile.x - currentLocation.x, nextTile.y - currentLocation.y, 0)
      magnitude = magnitude.normal2d()
      magnitudeTimesFive(magnitude)
      magnitude

    }

    else {
      val center: PhysicsVector = new PhysicsVector(currentTile.x + .5, currentTile.y + .5, 0)
      if (center.distance2d(currentLocation) < .1) {
        new PhysicsVector(0, 0, 0)
      }
      else {
        var magnitude: PhysicsVector = new PhysicsVector(center.x - currentLocation.x, center.y - currentLocation.y, 0)
        magnitude = magnitude.normal2d()
        magnitudeTimesFive(magnitude)
        magnitude
      }
    }
  }
}
