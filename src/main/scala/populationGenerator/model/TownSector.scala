package populationGenerator.model

sealed trait TownSector

object TownSector {
  case object Slums extends TownSector
  case object Poor extends TownSector
  case object MiddleClass extends TownSector
  case object UpperMiddleClass extends TownSector
  case object Exclusive extends TownSector
  case object PrivateState extends TownSector
}
