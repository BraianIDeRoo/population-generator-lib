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

import braianideroo.random.value.{RandomVIO, RandomValue}
import populationGenerator.model.Resident.TempResident
import zio.{Has, ZIO}

object Config {

  type TempRelation = (Relationship, Resident)
  type GenerationOrder = Int
  type GeneratorSettingsEnv =
    Has[TempResident] with Has[TempFamily] with Has[Option[TempRelation]]
  type CompletedGeneratorSettingsEnv =
    Has[Resident] with Has[TempFamily] with Has[Option[TempRelation]]

  trait GeneratorSettings {

    def firstNames: RandomValue[GeneratorSettingsEnv, Nothing, String]
    def familyNames: RandomValue[GeneratorSettingsEnv, Nothing, String]
    def races: RandomValue[GeneratorSettingsEnv, Nothing, String]
    def socioeconomicStatuses: RandomValue[GeneratorSettingsEnv, Nothing, SES]
    def ages: RandomValue[GeneratorSettingsEnv, Nothing, Age]
    def traits: RandomValue[GeneratorSettingsEnv, Nothing, String]
    def jobs: RandomValue[GeneratorSettingsEnv, Nothing, Option[String]]
    def genders: RandomValue[GeneratorSettingsEnv, Nothing, String]
    def traitsPerResident: RandomValue[GeneratorSettingsEnv, Nothing, Int]
    // TODO multiple relationships per resident pair
    def residentsPerRelation
      : RandomValue[CompletedGeneratorSettingsEnv,
                    Nothing,
                    Map[(GenerationOrder, Relationship), Int]]
  }

  trait BuildingGeneratorSetting {
    def buildingType: RandomVIO[Nothing, String]
    def buildingSubType: RandomValue[Has[String], Nothing, Option[String]]
  }
}
