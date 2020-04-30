/*
 * Copyright 2020 Braian I. De Roo
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import braianideroo.random.SeedRandom
import braianideroo.random.value.{Probabilities, RandomVIO, RandomValue}
import populationGenerator.model.Config.{
  BuildingGeneratorSetting,
  ChildSettings,
  GeneratorSettings,
  SpouseSettings
}
import populationGenerator.model._
import populationGenerator.model.error.GeneratorError
import zio.{Has, Ref, ZIO, ZLayer}

package object populationGenerator {

  type PopulationGenerator = Has[PopulationGenerator.Service]

  object PopulationGenerator {

    trait Service {
      def generatePopulation(n: Int): ZIO[Any, GeneratorError, Population]
    }

    case class PopulationConfigLive(
      generatorSettings: GeneratorSettings,
      spouseSettings: SpouseSettings[GeneratorSettings],
      childSettings: ChildSettings[GeneratorSettings],
      buildingSettings: BuildingGeneratorSetting
    )

    val live: ZLayer[Has[PopulationConfigLive] with SeedRandom,
                     Nothing,
                     PopulationGenerator] =
      ZLayer.fromEffect {
        for {
          configuration <- ZIO.access[Has[PopulationConfigLive]](_.get)
          random <- ZIO.access[SeedRandom](x => x)
          residents <- Ref.make[List[Resident]](List())
          buildings <- Ref.make[List[Building]](List())
        } yield
          new Service {

            private def getSectorForBuilding(building: Building) = {
              val sectorChance: Probabilities[Int] =
                Map(0 -> 90.0, 1 -> 5.0, -1 -> 5.0)

              val headResident = building.residents.head

              RandomValue.fromMap(sectorChance).map(_.get) >>= {
                case 0  => ZIO.succeed(headResident.ses.current)
                case 1  => ZIO.succeed(headResident.ses.higher)
                case -1 => ZIO.succeed(headResident.ses.lower)
              }
            }

            private def orderBySectors
              : RandomVIO[Nothing, Map[TownSector, Vector[Building]]] = {
              for {
                m <- Ref.make[Map[TownSector, Vector[Building]]](Map())
                buildList <- buildings.get
                _ <- ZIO.foreach(buildList)(
                  x =>
                    for {
                      sector <- getSectorForBuilding(x)
                      _ <- m.update(
                        old =>
                          old.get(sector) match {
                            case Some(value) => old.updated(sector, value :+ x)
                            case None        => old + (sector -> Vector(x))
                        }
                      )
                    } yield ()
                )
                res <- m.get
              } yield res
            }

            private def generateFamily(
              resident: Resident
            ): ZIO[SeedRandom,
                   GeneratorError,
                   (Resident, Option[Resident], List[Resident])] = {
              resident.age match {
                case Child => ZIO.succeed((resident, None, List()))
                case Adult | Elderly =>
                  for {
                    spouse <- Resident.fromSpouseConfig(
                      resident,
                      resident.children,
                      List(),
                      configuration.generatorSettings,
                      configuration.spouseSettings
                    )
                    childNumber <- configuration.generatorSettings.childrenPerFamily
                    children <- ZIO.foreach(0 until childNumber)(
                      _ =>
                        Resident.fromChildSettings(
                          List(resident) ++ spouse,
                          configuration.generatorSettings,
                          configuration.childSettings
                      )
                    )
                    newResident = resident.copy(
                      spouse = spouse,
                      children = children
                    )
                    newSpouse = spouse.map(x => x.copy(children = children))
                  } yield (newResident, newSpouse, children)
              }
            }

            override def generatePopulation(
              n: Int
            ): ZIO[Any, GeneratorError, Population] = {
              val loop = for {
                resident <- Resident.fromConfig(
                  configuration.generatorSettings,
                  None,
                  List(),
                  None
                )
                _ <- resident.age match {
                  case Adult | Elderly =>
                    for {
                      family <- generateFamily(resident)
                      all = List(family._1) ++ family._2 ++ family._3
                      building <- Building.fromConfig(
                        all,
                        configuration.buildingSettings
                      )
                      _ <- residents.update(_ ++ all)
                      _ <- buildings.update(_ :+ building)
                    } yield ()
                  case Child =>
                    val building = Building.apply(Seq(resident), "none", None)
                    for {
                      _ <- residents.update(_ :+ resident)
                      _ <- buildings.update(_ :+ building)
                    } yield ()
                }
              } yield ()

              for {
                _ <- loop.doUntilM(
                  _ => residents.get >>= (x => ZIO.succeed(x.size >= n))
                )
                map <- orderBySectors
              } yield Population(map)
            }.provide(random)
          }
      }

    // accessor methods
    def generatePopulation(
      n: Int
    ): ZIO[PopulationGenerator, GeneratorError, Population] =
      ZIO.accessM[PopulationGenerator](_.get.generatePopulation(n))
  }

}
