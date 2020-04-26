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
