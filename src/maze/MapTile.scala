package maze

object MapTile {

  def generateRow(row: String): List[MapTile] = {
    row.map((ch: Char) => MapTile(ch.toString)).toList
  }

  def apply(tileType: String): MapTile = {
    tileType match {
      case "-" => new MapTile("ground", true)
      case "G" => new MapTile("goal", true)
      case "O" => new MapTile("wall", false)
    }
  }

}

class MapTile(val tileType: String, val passable: Boolean) {

}
