package eightqueen

import zio._
import zio.stream._

sealed trait CellValue

case object Queen extends CellValue {
  override def toString(): String = "1"
}
case object Empty extends CellValue {
  override def toString(): String = "0"
}
case object Hit extends CellValue {
  override def toString(): String = "x"
}

case class Cell(value: CellValue)

// TODO: use refined types of fixed value iterables
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

  def playQueen(board: Board, coord: (Int, Int)) = coord match {
    case (row, col) =>
      board.updated(row, board(row).updated(col, Queen))
  }

  def toString(board: Board) = {
    board.map(_.mkString(", ")).mkString("\n")
  }

  def tap[T](x: T) = {
    println(x)
    x
  }

  def solve(n: Int, board: Board, numQueens: Int): UIO[Option[Board]] = {
    if (numQueens == n) ZIO.some(board)
    else {
      val cells =
        board.zipWithIndex
          .map({ case (cells, row) =>
            cells.zipWithIndex.map({ case (cellValue, col) =>
              (cellValue, (row, col))
            })
          })
          .flatten

      ZStream
        .fromIterable(cells)
        // TODO: Learn how to tune this
        .mapZIOPar(3)({
          case (Empty, coord) => {
            solve(n, markHits(n, playQueen(board, coord), coord), numQueens + 1)
          }
          case _ => ZIO.none
        })
        .collectSome
        .runHead
    }
  }

  def markHit(board: Board)(coord: (Int, Int)): Board = {
    val (row, col) = coord

    board(row)(col) match {
      case Empty => board.updated(row, board(row).updated(col, Hit))
      case _     => board
    }
  }

  def markHits(n: Int, board: Board, coord: (Int, Int)): Board = coord match {
    case (row, col) =>
      lazy val placesHit =
        (0 until n).map((_, col)) ++
          (0 until n).map((row, _)) ++
          ((row - Seq(row, col).min) until n)
            .zip(((col - Seq(row, col).min) until n)) ++
          ((0 until n)
            .map(x => (row - x, col + x))
            ++
              (0 until n)
                .map(x => (row + x, col - x)))
            .filter({ case (x, y) => x >= 0 && x < n && y >= 0 && y < n })
      placesHit.foldLeft(board)((agg, coord) => markHit(agg)(coord))
  }
}

object App extends ZIOAppDefault {
  val n = 12
  val board = Board.default(n)

  val myAppLogic =
    for {
      result <- Board.solve(n, board, 0)
      _ <- Console.printLine(Board.toString(result.head))
    } yield ()

  def run = myAppLogic
}
