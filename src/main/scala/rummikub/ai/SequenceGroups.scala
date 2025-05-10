package rummikub.ai

import cats.data.NonEmptyList
import cats.syntax.all.*
import rummikub.model.*

import scala.annotation.tailrec

object SequenceGroups {
  def apply(bag: Bag): Set[Group] = {
    val numberOfJokers = bag.numberOfJokers

    bag
      .distinctNonJokers
      .groupBy(_.colour)
      .toSet
      .flatMap { case (_, pieces) => createGroupsForColour(pieces.sortBy(_.number), numberOfJokers) }
  }

  private def createGroupsForColour(nonJokers: List[Piece.Fixed], jokers: Int): Set[Group] = {
    @tailrec
    def recurse(nonJokers: List[Piece.Fixed], groups: Set[Group]): Set[Group] =
      nonJokers match {
        case Nil => groups
        case head :: tail => recurse(tail, groups ++ createGroupsWithFirst(head, tail, jokers))
      }

     recurse(nonJokers, Set.empty[Group])
  }

  private def createGroupsWithFirst(first: Piece.Fixed, remainingNonJokers: List[Piece.Fixed], jokers: Int) = {
    val withPrecedingJokers = (jokers, first.number) match {
      case (0, _) => Set.empty[Group]
      case (_, 1) => Set.empty[Group]
      case (1, n) => createGroupsWithSeed(NonEmptyList.of(first, Piece.Joker), remainingNonJokers, 0, n)
      case (2, 2) => createGroupsWithSeed(NonEmptyList.of(first, Piece.Joker), remainingNonJokers, 1, 2)
      case (2, n) =>
        createGroupsWithSeed(NonEmptyList.of(first, Piece.Joker), remainingNonJokers, 1, n) ++
          createGroupsWithSeed(NonEmptyList.of(first, Piece.Joker, Piece.Joker), remainingNonJokers, 0, n)
      case _ => throw new IllegalStateException(s"Unexpected number of jokers: $jokers")
    }

    withPrecedingJokers ++
      createGroupsWithSeed(NonEmptyList.of(first), remainingNonJokers, jokers, first.number)
  }

  private def createGroupsWithSeed(seed: NonEmptyList[Piece], remainingNonJokers: List[Piece.Fixed], jokers: Int, n: Int, groups: Set[Group] = Set.empty): Set[Group] = {
    if (n > 13) groups
    else if (n === 13)
      if (seed.length >= 3) groups + Group.Run(seed.reverse)
      else groups
    else {
      val unextended = if (seed.size >= 3) groups + Group.Run(seed.reverse) else groups

      val extendedByJokers: Set[Group] =
        if (jokers === 0) unextended
        else createGroupsWithSeed(Piece.Joker :: seed, remainingNonJokers, jokers - 1, n + 1, unextended)

      remainingNonJokers match {
        case Nil => extendedByJokers
        case next :: rest =>
          if (next.number <= n) createGroupsWithSeed(seed, rest, jokers, n, extendedByJokers)
          else if (next.number === n + 1) createGroupsWithSeed((next:Piece) :: seed, rest, jokers, next.number, extendedByJokers)
          else extendedByJokers
      }
    }
  }
}
