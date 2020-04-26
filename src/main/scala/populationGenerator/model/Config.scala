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
import zio.Has

object Config {

  trait GeneratorSettings {
    def firstNames: RandomValue[Has[TempResident], Nothing, String]
    def familyNames: RandomValue[Has[TempResident], Nothing, String]
    def races: RandomValue[Has[TempResident], Nothing, String]
    def socioeconomicStatuses: RandomValue[Has[TempResident], Nothing, SES]
    def ages: RandomValue[Has[TempResident], Nothing, Age]
    def traits: RandomValue[Has[TempResident], Nothing, String]
    def jobs: RandomValue[Has[TempResident], Nothing, Option[String]]
    def genders: RandomValue[Has[TempResident], Nothing, String]
    def traitsPerResident: RandomValue[Has[TempResident], Nothing, Int]
    def childrenPerFamily: RandomVIO[Nothing, Int]
    def hasSpouse: RandomValue[Has[TempResident], Nothing, Boolean]
  }

  trait FamilyMemberGeneratorSettings[GS <: GeneratorSettings, E] {
    type Environment = Has[GS] with Has[TempResident] with E

    type FMValue[A] = RandomValue[Environment, Nothing, A]

    def firstName: FMValue[String]
    def familyName: FMValue[String]
    def gender: FMValue[String]
    def age: FMValue[Age]
    def race: FMValue[String]
    def job: FMValue[Option[String]]
    def traitsPerResident: FMValue[Int]
    def traits: FMValue[String]
    def socioeconomicStatus: FMValue[SES]
  }
  trait SpouseSettings[GS <: GeneratorSettings]
      extends FamilyMemberGeneratorSettings[GS, Has[Resident]]

  trait ChildSettings[GS <: GeneratorSettings]
      extends FamilyMemberGeneratorSettings[GS, Has[Iterable[Resident]]]

  trait BuildingGeneratorSetting {
    def buildingType: RandomVIO[Nothing, String]
    def buildingSubType: RandomValue[Has[String], Nothing, Option[String]]
  }
}
