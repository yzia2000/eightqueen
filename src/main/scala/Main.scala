import zio._
import zio.stream._

sealed trait CellValue {
  val value: Int
}

case object Queen extends CellValue {
  val value = 1
}
case object Empty extends CellValue {
  val value = 0
}

case class Cell(value: CellValue)
type Row = Vector[CellValue]
type Board = Vector[Row]

object Row {
  def default(n: Int): Row = {
    Vector.fill(n)(Empty)
  }
}

object Board {
  def default(n: Int): Board = {
    Vector.fill(n)(Row.default(n))
  }

  def playQueen(board: Board, row: Int, col: Int) = {
    board.updated(row, board(row).updated(col, Queen))
  }

  def toString(board: Board) = {
    board.map(_.mkString(", ")).mkString("\n")
  }

  def tap[T](x: T) = {
    println(x)
    x
  }

  def solve(n: Int, board: Board, numQueens: Int): Option[Board] = {
    if (numQueens == n) Some(board)
    else {
      val result = for {
        row <- 0 until n
        col <- 0 until n
        updatedBoard <- board(row)(col) match {
          case Empty if canPlace(n, board, row, col) => {
            solve(n, playQueen(board, row, col), numQueens + 1)
          }
          case _ => None
        }
      } yield (updatedBoard)
      result.headOption
    }
  }

  def canPlace(n: Int, board: Board, row: Int, col: Int): Boolean = {
    // Linear Checks

    val checkTopToBottom =
      (0 until n).foldLeft(0)((agg, x) => agg + board(x)(col).value) > 0

    val checkLeftToRight =
      (0 until n).foldLeft(0)((agg, x) => agg + board(row)(x).value) > 0

    val checkDiagTopLeftBottomRight =
      (
        ((row - Seq(row, col).min) until n)
          .zip(((col - Seq(row, col).min) until n))
        )
        .foldLeft(0)((agg, x) =>
          x match { case (x, y) => board(x)(y).value + agg }
        ) > 0

    val checkDiagBottomLeftTopRight =
      (
        ((0 until n)
          .map(x => (row - x, col + x))
          ++
            (0 until n)
              .map(x => (row + x, col - x)))
          .filter({ case (x, y) => x >= 0 && x < n && y >= 0 && y < n })
        )
        .foldLeft(0)((agg, x) =>
          x match { case (x, y) => board(x)(y).value + agg }
        ) > 0

    !(checkTopToBottom || checkLeftToRight || checkDiagTopLeftBottomRight || checkDiagBottomLeftTopRight)
  }
}

@main def main: Unit = {
  val n = 5
  val board = Board.default(n)
  print(
    Board
      .solve(n, board, 0)
      .fold("Doesn't work lol\n")(Board.toString)
  )
}
