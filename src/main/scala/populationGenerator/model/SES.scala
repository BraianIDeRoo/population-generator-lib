package populationGenerator.model

import TownSector._

sealed trait SES {
  def lower: TownSector
  def current: TownSector
  def higher: TownSector
}

object SES {

  sealed trait BasePoorSes extends SES {
    override def lower: TownSector = Slums

    override def current: TownSector = TownSector.Poor

    override def higher: TownSector = MiddleClass
  }

  case object Poor extends BasePoorSes

  case object Struggling extends BasePoorSes

  case object Comfortable extends SES {
    override def lower: TownSector = TownSector.Poor

    override def current: TownSector = MiddleClass

    override def higher: TownSector = UpperMiddleClass
  }

  case object Affluent extends SES {
    override def lower: TownSector = MiddleClass

    override def current: TownSector = UpperMiddleClass

    override def higher: TownSector = Exclusive
  }

  case object Rich extends SES {
    override def lower: TownSector = UpperMiddleClass

    override def current: TownSector = Exclusive

    override def higher: TownSector = PrivateState
  }
}
