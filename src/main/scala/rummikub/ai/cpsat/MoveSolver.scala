package rummikub.ai.cpsat

import com.google.ortools.Loader
import com.google.ortools.sat.{CpModel, CpSolver, CpSolverStatus, IntVar, LinearArgument, LinearExpr, LinearExprBuilder}
import rummikub.ai.{BoardAndPlayer, NumberGroups, SequenceGroups}
import rummikub.model.*
import rummikub.model.Piece.{Fixed, Joker}

object MoveSolver:
  Loader.loadNativeLibraries()

  private case class GroupComposition(group: Group, counts: Map[Piece, Int])

  private val allTiles: Vector[Piece] =
    Joker +: (for c <- Colour.values.toVector; n <- (1 to 13).toVector yield Fixed(c, n))

  private def value(piece: Piece): Long = piece match
    case Joker       => 30L
    case Fixed(_, n) => n.toLong

  private val universe: Vector[GroupComposition] =
    val universeBag = Bag(allTiles.toList ::: List(Joker))
    (SequenceGroups(universeBag) ++ NumberGroups(universeBag)).toVector
      .map(group => GroupComposition(group, counts(group.pieces.toList)))

  def apply(board: Board, rack: Bag, firstMove: Boolean): Option[BoardAndPlayer] =
    val model      = new CpModel
    val boardCount = board.pieces.pieces
    val rackCount  = rack.pieces

    val groupVars: Vector[IntVar]   = universe.indices.map(j => model.newIntVar(0, 2, s"x$j")).toVector
    val rackVars: Map[Piece, IntVar] =
      allTiles.map(tile => tile -> model.newIntVar(0, rackCount.getOrElse(tile, 0).toLong, tile.toString)).toMap

    conservationConstraints(model, groupVars, rackVars, boardCount)

    val valuePlaced = weightedTileSum(rackVars)
    model.maximize(valuePlaced)
    if firstMove then model.addGreaterOrEqual(valuePlaced, LinearExpr.constant(30L))

    solve(model).flatMap(solver => decode(solver, groupVars, rackVars, rack))

  private def conservationConstraints(model: CpModel, groupVars: Vector[IntVar], rackVars: Map[Piece, IntVar], boardCount: Map[Piece, Int]): Unit =
    allTiles.foreach(tile => addConservationConstraint(model, groupVars, rackVars, boardCount, tile))

  private def addConservationConstraint(model: CpModel, groupVars: Vector[IntVar], rackVars: Map[Piece, IntVar], boardCount: Map[Piece, Int], tile: Piece): Unit =
    val expr = groupContribution(groupVars, tile)
    expr.addTerm(rackVars(tile), -1L)
    model.addEquality(expr.build(), LinearExpr.constant(boardCount.getOrElse(tile, 0).toLong))

  private def groupContribution(groupVars: Vector[IntVar], tile: Piece): LinearExprBuilder =
    val expr = LinearExpr.newBuilder()
    universe.indices.foreach { j =>
      val a = universe(j).counts.getOrElse(tile, 0)
      if a != 0 then expr.addTerm(groupVars(j), a.toLong)
    }
    expr

  private def weightedTileSum(rackVars: Map[Piece, IntVar]): LinearArgument =
    val expr = LinearExpr.newBuilder()
    allTiles.foreach(tile => expr.addTerm(rackVars(tile), value(tile)))
    expr.build()

  private def solve(model: CpModel): Option[CpSolver] =
    val solver = new CpSolver
    solver.getParameters.setMaxTimeInSeconds(5.0)
    val status = solver.solve(model)
    Option.when(status == CpSolverStatus.OPTIMAL || status == CpSolverStatus.FEASIBLE)(solver)

  private def decode(solver: CpSolver, groupVars: Vector[IntVar], rackVars: Map[Piece, IntVar], rack: Bag): Option[BoardAndPlayer] =
    val placedFromRack = allTiles.flatMap(tile => List.fill(solver.value(rackVars(tile)).toInt)(tile)).toList
    Option.when(placedFromRack.map(value).sum > 0)(assembleResult(solver, groupVars, placedFromRack, rack))

  private def assembleResult(solver: CpSolver, groupVars: Vector[IntVar], placedFromRack: List[Piece], rack: Bag): BoardAndPlayer =
    BoardAndPlayer(Board(decodeGroups(solver, groupVars)), removePieces(rack, placedFromRack))

  private def decodeGroups(solver: CpSolver, groupVars: Vector[IntVar]): List[Group] =
    universe.indices.toList.flatMap(j => List.fill(solver.value(groupVars(j)).toInt)(universe(j).group))

  private def removePieces(rack: Bag, pieces: List[Piece]): Bag =
    pieces.foldLeft(rack)((remaining, piece) => remaining - piece)

  private def counts(pieces: List[Piece]): Map[Piece, Int] =
    pieces.groupBy(identity).view.mapValues(_.size).toMap
