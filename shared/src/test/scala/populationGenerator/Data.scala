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

package populationGenerator

import braianideroo.random.value.{RandomVIO, RandomValue}
import populationGenerator.model.Config.{
  BuildingGeneratorSetting,
  CompletedGeneratorSettingsEnv,
  GenerationOrder,
  GeneratorSettings,
  GeneratorSettingsEnv
}
import populationGenerator.model.Resident.TempResident
import populationGenerator.model.{
  Adult,
  Age,
  Child,
  Elderly,
  Relationship,
  Resident,
  SES,
  TempFamily
}
import zio.{Has, ZIO}

object Data {

  val races: RandomVIO[Nothing, String] =
    RandomValue
      .fromSimpleIterable[String](
        List(
          ("dwarf", 5),
          ("elf", 4),
          ("gnome", 3),
          ("half-elf", 3),
          ("halfling", 5),
          ("half-orc", 1),
          ("human", 20)
        )
      )
      .map(_.get)

  val socioeconomicStatuses: RandomVIO[Nothing, SES] =
    RandomValue
      .fromSimpleIterable[SES](
        List(
          (SES.Rich, 1),
          (SES.Affluent, 10),
          (SES.Comfortable, 20),
          (SES.Struggling, 40),
          (SES.Poor, 23)
        )
      )
      .map(_.get)

  val ages: RandomVIO[Nothing, Age] =
    RandomValue
      .fromSimpleIterable[Age](List((Elderly, 20), (Adult, 78), (Child, 2)))
      .map(_.get)

  val traits: RandomVIO[Nothing, String] =
    RandomValue
      .fromIterable(
        List(
          "Accessible",
          "Active",
          "Adaptable",
          "Admirable",
          "Adventurous",
          "a",
          "b",
          "c",
          "d",
          "e",
          "f",
          "g"
        )
      )
      .map(_.get)

  val firstNames: RandomValue[Has[TempResident], Nothing, String] =
    for {
      resident <- ZIO.access[Has[TempResident]](_.get)
      maybeRace <- resident.maybeRace.get
      aux <- maybeRace match {
        case Some(value) =>
          value match {
            case "dwarf" =>
              RandomValue.fromIterable[Any, String](
                List("Mohamed", "Youssef", "Ahmed", "Mahmoud", "Mustafa")
              )
            case "elf" =>
              RandomValue.fromIterable[Any, String](
                List("Santiago", "Mateo", "Juan")
              )
            case "gnome" =>
              RandomValue.fromIterable[Any, String](
                List(
                  "Stevenson",
                  "Stanley",
                  "Samuel",
                  "Peterson",
                  "Daniel",
                  "Wilson"
                )
              )
            case "half-elf" =>
              RandomValue.fromIterable[Any, String](
                List("Emma", "Olivia", "Ava", "Isabella", "Sophia")
              )
            case "halfling" =>
              RandomValue.fromIterable[Any, String](
                List("Marc", "Eric", "Jan", "Daniel", "Enzo")
              )
            case "half-orc" =>
              RandomValue.fromIterable[Any, String](
                List(
                  "Antoni",
                  "Jakub",
                  "Jan",
                  "Szymon",
                  "Aleksander",
                  "Franciszek"
                )
              )
            case "human" =>
              RandomValue.fromIterable[Any, String](
                List("Jack", "James", "Oliver", "Lewis", "Logan")
              )
            case _ =>
              RandomValue.fromIterable[Any, String](List("shouldnt happen"))
          }
        case None =>
          RandomValue
            .fromIterable[Any, String](
              List(
                "John",
                "Matias",
                "Hans",
                "Ivan",
                "Li",
                "Pierre",
                "Taller John"
              )
            )
      }
      res = aux.get
    } yield res

  val jobs: RandomVIO[Nothing, Option[String]] =
    RandomValue
      .fromIterable(
        List(
          Some("nobility"),
          Some("land owner"),
          Some("shopkeep"),
          Some("artisan"),
          Some("trader"),
          Some("landlord"),
          Some("service"),
          Some("tavern"),
          Some("guard"),
          Some("temple"),
          Some("worker"),
          Some("field hand"),
          Some("beggar"),
          None
        )
      )
      .map(_.get)

  val genders: RandomVIO[Nothing, String] =
    RandomValue
      .fromSimpleIterable[String](List(("male", 5), ("female", 5)))
      .map(_.get)

  val childrenPerResident: RandomVIO[Nothing, Int] =
    RandomValue
      .fromSimpleIterable[Int](List((1, 20), (2, 40), (3, 20), (4, 15), (5, 5)))
      .map(_.get)

  val traitsPerResident: RandomVIO[Nothing, Int] =
    RandomValue
      .fromSimpleIterable[Int](List((1, 20), (2, 30), (3, 40), (4, 10)))
      .map(_.get)

  val hasSpouse: RandomVIO[Nothing, Boolean] =
    RandomValue
      .fromSimpleIterable[Boolean](List((true, 80), (false, 20)))
      .map(_.get)

  val residentsPerRelation
    : RandomVIO[Nothing, Map[(GenerationOrder, Relationship), Int]] =
    RandomValue
      .fromSimpleIterable[Map[(GenerationOrder, Relationship), Int]](
        List(
          (
            Map(
              ((1, Relationship.parentRelationship), 2),
              ((2, Relationship.spouseRelationship), 1)
            ),
            1
          )
        )
      )
      .map(_.get)

  val testConfiguration: GeneratorSettings = new GeneratorSettings {
    override def firstNames
      : RandomValue[GeneratorSettingsEnv, Nothing, String] =
      Data.firstNames

    override def familyNames
      : RandomValue[GeneratorSettingsEnv, Nothing, String] =
      for {
        maybeRelationship <- ZIO
          .access[Has[Option[(Relationship, Resident)]]](_.get)
        res <- maybeRelationship match {
          case Some(relationship) =>
            relationship._1.originName match {
              case "parent" | "spouse" =>
                ZIO.succeed(relationship._2.familyName)
              case _ => Data.firstNames
            }
          case None => Data.firstNames
        }
      } yield res

    override def races: RandomValue[GeneratorSettingsEnv, Nothing, String] =
      Data.races

    override def socioeconomicStatuses
      : RandomValue[GeneratorSettingsEnv, Nothing, SES] =
      for {
        maybeRelationship <- ZIO
          .access[Has[Option[(Relationship, Resident)]]](_.get)
        res <- maybeRelationship match {
          case Some(relationship) =>
            relationship._1.originName match {
              case "parent" | "spouse" =>
                ZIO.succeed(relationship._2.ses)
              case _ => Data.socioeconomicStatuses
            }
          case None => Data.socioeconomicStatuses
        }
      } yield res

    override def ages: RandomValue[GeneratorSettingsEnv, Nothing, Age] =
      for {
        maybeRelationship <- ZIO
          .access[Has[Option[(Relationship, Resident)]]](_.get)
        res <- maybeRelationship match {
          case Some(relationship) =>
            relationship._1.originName match {
              case "spouse" =>
                ZIO.succeed(relationship._2.age)
              case "parent" =>
                ZIO.succeed(relationship._2.age.previous.get)
              case _ => Data.ages
            }
          case None => Data.ages
        }
      } yield res

    override def traits: RandomValue[GeneratorSettingsEnv, Nothing, String] =
      Data.traits

    override def jobs
      : RandomValue[GeneratorSettingsEnv, Nothing, Option[String]] =
      for {
        maybeRelationship <- ZIO
          .access[Has[Option[(Relationship, Resident)]]](_.get)
        res <- maybeRelationship match {
          case Some(relationship) =>
            relationship._1.originName match {
              case "parent" | "spouse" =>
                ZIO.succeed(relationship._2.job)
              case _ => Data.jobs
            }
          case None => Data.jobs
        }
      } yield res

    override def genders: RandomValue[GeneratorSettingsEnv, Nothing, String] =
      Data.genders

    override def traitsPerResident
      : RandomValue[GeneratorSettingsEnv, Nothing, Int] =
      Data.traitsPerResident

    override def residentsPerRelation
      : RandomValue[CompletedGeneratorSettingsEnv,
                    Nothing,
                    Map[(GenerationOrder, Relationship), Int]] =
      for {
        family <- ZIO.access[Has[TempFamily]](_.get)
        residents <- family.tempResidents.get
        res <- if (residents.nonEmpty)
          ZIO.succeed(Map[(GenerationOrder, Relationship), Int]())
        else
          Data.residentsPerRelation
      } yield res
  }

  val testBuildingConfiguration: BuildingGeneratorSetting =
    new BuildingGeneratorSetting {
      override def buildingType: RandomVIO[Nothing, String] =
        RandomValue
          .fromSimpleIterable[String](
            List(
              ("residence", 45),
              ("merchant", 20),
              ("artisan", 15),
              ("temple", 5),
              ("shopkeep", 5),
              ("tavern", 5),
              ("none", 5)
            )
          )
          .map(_.get)

      override def buildingSubType
        : RandomValue[Has[String], Nothing, Option[String]] =
        for {
          buildingType <- ZIO.access[Has[String]](_.get)
          res <- buildingType match {
            case "residence" => ZIO.none
            case "merchant"  => ZIO.none
            case "artisan" =>
              RandomValue
                .fromIterable[Any, String](
                  List(
                    "bookmaker",
                    "scribe",
                    "carpenter",
                    "tailor",
                    "glassblower",
                    "jewelry maker",
                    "artist",
                    "potter",
                    "cobbler",
                    "stonemason"
                  )
                )
            case "temple" =>
              RandomValue
                .fromIterable[Any, String](
                  List(
                    "LG temple",
                    "NG temple",
                    "CG temple",
                    "LN temple",
                    "N temple",
                    "CN temple"
                  )
                )
            case "shopkeep" =>
              RandomValue
                .fromIterable[Any, String](
                  List("general goods", "magical goods", "armor and weapons")
                )
            case "tavern" => ZIO.none
            case "none" =>
              RandomValue.fromIterable[Any, String](List("street", "shack"))
            case _ => ZIO.none
          }
        } yield res
    }

}
