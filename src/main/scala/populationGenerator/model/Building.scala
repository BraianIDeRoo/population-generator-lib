package populationGenerator.model

import braianideroo.random.SeedRandom
import populationGenerator.model.Config.BuildingGeneratorSetting
import populationGenerator.model.error.GeneratorError
import zio.{ZIO, ZLayer}

case class Building(residents: Iterable[Resident],
                    buildingType: String,
                    subtype: Option[String]) {}

object Building {
  def fromConfig(
    residents: Iterable[Resident],
    config: BuildingGeneratorSetting
  ): ZIO[SeedRandom, GeneratorError, Building] =
    for {
      bt <- config.buildingType
      btLayer = ZLayer.succeed(bt)
      subtype <- config.buildingSubType.provideSomeLayer[SeedRandom](btLayer)
    } yield Building(residents, bt, subtype)
}
