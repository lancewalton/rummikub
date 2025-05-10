package rumikub.ai

import cats.data.NonEmptyList
import cats.syntax.all.*
import rumikub.model.*

import scala.annotation.tailrec

object NumberGroups {
  def apply(bag: Bag): Set[Group] = {
    val numberOfJokers = bag.numberOfJokers

    bag
      .distinctNonJokers
      .groupBy(_.number)
      .toSet
      .flatMap { case (_, pieces) => createGroupsForNumber(pieces, numberOfJokers) }
  }

  private def createGroupsForNumber(nonJokers: List[Piece.Fixed], jokers: Int): Set[Group] = {
    @tailrec
    def recurse(remainingNonJokers: List[Piece.Fixed], groups: Set[Group]): Set[Group] =
      remainingNonJokers match {
        case Nil => groups
        case head :: tail => recurse(tail, groups ++ createGroupsWithFirst(head, tail, jokers))
      }

    recurse(nonJokers, Set.empty)
  }

  private def createGroupsWithFirst(first: Piece.Fixed, remainingNonJokers: List[Piece.Fixed], jokers: Int) =
    if (jokers === 2)
      createGroupsWithSeed(NonEmptyList.of(first), remainingNonJokers) ++
        createGroupsWithSeed(NonEmptyList.of(first, Piece.Joker), remainingNonJokers) ++
        createGroupsWithSeed(NonEmptyList.of(first, Piece.Joker, Piece.Joker), remainingNonJokers)
    else if (jokers === 1)
      createGroupsWithSeed(NonEmptyList.of(first), remainingNonJokers) ++
        createGroupsWithSeed(NonEmptyList.of(first, Piece.Joker), remainingNonJokers)
    else
      createGroupsWithSeed(NonEmptyList.of(first), remainingNonJokers)

  private def createGroupsWithSeed(seed: NonEmptyList[Piece], remainingNonJokers: List[Piece.Fixed]): Set[Group] =
    if (seed.size === 4) Set(Group.Number(seed.reverse))
    else {
      val newGroups =
        if (seed.size === 3) Set[Group](Group.Number(seed.reverse))
        else Set.empty[Group]

      remainingNonJokers match {
        case Nil => newGroups
        case head :: tail =>
          val withoutHead = newGroups ++ createGroupsWithSeed(seed, tail)
          if (seed.exists(_ === head)) withoutHead
          else withoutHead ++ createGroupsWithSeed(head :: seed, tail)
      }
    }
}
