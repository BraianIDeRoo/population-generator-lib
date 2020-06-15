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
import braianideroo.random.value.{RandomVIO, RandomValue}
import populationGenerator.model.Config.{
  BuildingGeneratorSetting,
  GeneratorSettings,
  TempRelation
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

    case class PopulationConfigLive(generatorSettings: GeneratorSettings,
                                    buildingSettings: BuildingGeneratorSetting)

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
              val sectorChance: List[(Int, Double)] =
                List((0, 90.0), (1, 5.0), (-1, 5.0))
              Map(0 -> 90.0, 1 -> 5.0, -1 -> 5.0)

              val headResident = building.family.residents.head

              RandomValue.fromSimpleIterable(sectorChance).map(_.get) >>= {
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
              tempFamily: TempFamily,
              resident: Resident,
              generatorSettings: GeneratorSettings
            ): ZIO[SeedRandom, GeneratorError, Family] = {

              def inner(
                tempFamily: TempFamily,
                relationships: List[(Relationship, Int)]
              ): ZIO[SeedRandom, GeneratorError, Unit] = {
                relationships match {
                  case Nil => ZIO.unit
                  case ::(head, next) =>
                    if (head._2 > 0) {
                      val newHead = (head._1, head._2 - 1)
                      (for {
                        newResident <- Resident(
                          generatorSettings,
                          tempFamily,
                          Some(head._1, resident)
                        )
                        _ <- tempFamily.addResident(newResident)
                      } yield ()) *> inner(tempFamily, next.+:(newHead))
                    } else inner(tempFamily, next)
                }
              }

              val residentLayer = ZLayer.succeed(resident)
              val tempFamilyLayer = ZLayer.succeed(tempFamily)
              val noRelation = ZLayer.succeed[Option[TempRelation]](None)
              val fullLayer = residentLayer ++ tempFamilyLayer ++ noRelation
              for {
                relationshipNumbers <- generatorSettings.residentsPerRelation
                  .provideSomeLayer[SeedRandom](fullLayer)
                orderedRelationships = relationshipNumbers.toList
                  .sortBy(_._1._1)
                  .map(x => (x._1._2, x._2))
                _ <- tempFamily.addResident(resident)
                _ <- inner(tempFamily, orderedRelationships)
                family <- tempFamily.toFamily
              } yield family
            }

            override def generatePopulation(
              n: Int
            ): ZIO[Any, GeneratorError, Population] = {
              val loop = for {
                tempFamily <- TempFamily.apply
                resident <- Resident(
                  configuration.generatorSettings,
                  tempFamily,
                  None
                )
                _ <- resident.age match {
                  case Adult | Elderly =>
                    for {
                      family <- generateFamily(
                        tempFamily,
                        resident,
                        configuration.generatorSettings
                      )
                      all = family.residents
                      building <- Building.fromConfig(
                        family,
                        configuration.buildingSettings
                      )
                      _ <- residents.update(_ ++ all)
                      _ <- buildings.update(_ :+ building)
                    } yield ()
                  case Child =>
                    val family = Family.apply(Vector(resident), Map())
                    val building = Building.apply(family, "none", None)
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
