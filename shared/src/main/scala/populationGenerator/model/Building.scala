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
