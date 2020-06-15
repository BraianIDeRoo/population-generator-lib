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
import populationGenerator.model.Config.GeneratorSettings
import populationGenerator.model.error.GeneratorError
import zio.{Ref, ZIO, ZLayer}

case class Resident(ses: SES,
                    age: Age,
                    gender: String,
                    race: String,
                    firstName: String,
                    familyName: String,
                    traits: Iterable[String],
                    job: Option[String])

object Resident {

  case class TempResident(maybeSes: Ref[Option[SES]],
                          maybeAge: Ref[Option[Age]],
                          maybeGender: Ref[Option[String]],
                          maybeRace: Ref[Option[String]],
                          maybeFirstName: Ref[Option[String]],
                          maybeFamilyName: Ref[Option[String]],
                          maybeTraits: Ref[Option[Iterable[String]]],
                          maybeJob: Ref[Option[Option[String]]]) {
    def toResident: ZIO[Any, Nothing, Option[Resident]] = {
      for {
        ses <- maybeSes.get
        age <- maybeAge.get
        gender <- maybeGender.get
        race <- maybeRace.get
        firstName <- maybeFirstName.get
        familyName <- maybeFamilyName.get
        traits <- maybeTraits.get
        job <- maybeJob.get
        res <- if (ses.isDefined &&
                   age.isDefined &&
                   gender.isDefined &&
                   race.isDefined &&
                   firstName.isDefined &&
                   familyName.isDefined &&
                   traits.isDefined &&
                   job.isDefined)
          ZIO.some(
            Resident(
              ses.get,
              age.get,
              gender.get,
              race.get,
              firstName.get,
              familyName.get,
              traits.get,
              job.get
            )
          )
        else ZIO.none
      } yield res
    }
  }

  object TempResident {
    def make: ZIO[Any, Nothing, TempResident] =
      for {
        maybeSes <- Ref.make[Option[SES]](None)
        maybeAge <- Ref.make[Option[Age]](None)
        maybeGender <- Ref.make[Option[String]](None)
        maybeRace <- Ref.make[Option[String]](None)
        maybeFirstName <- Ref.make[Option[String]](None)
        maybeFamilyName <- Ref.make[Option[String]](None)
        maybeTraits <- Ref.make[Option[Iterable[String]]](None)
        maybeJob <- Ref.make[Option[Option[String]]](None)
      } yield
        TempResident(
          maybeSes,
          maybeAge,
          maybeGender,
          maybeRace,
          maybeFirstName,
          maybeFamilyName,
          maybeTraits,
          maybeJob
        )
  }

  def apply(
    generatorSettings: GeneratorSettings,
    tempFamily: TempFamily,
    relationship: Option[(Relationship, Resident)]
  ): ZIO[SeedRandom, GeneratorError, Resident] = {

    for {
      tempResident <- TempResident.make
      tempResidentLayer = (ZLayer.succeed(tempResident) ++ ZLayer.succeed(
        tempFamily
      )) ++ ZLayer.succeed(relationship)
      _ <- generatorSettings.socioeconomicStatuses.provideSomeLayer[SeedRandom](
        tempResidentLayer
      ) >>= (x => tempResident.maybeSes.set(Some(x)))
      _ <- generatorSettings.ages.provideSomeLayer[SeedRandom](
        tempResidentLayer
      ) >>= (x => tempResident.maybeAge.set(Some(x)))
      _ <- generatorSettings.races.provideSomeLayer[SeedRandom](
        tempResidentLayer
      ) >>= (x => tempResident.maybeRace.set(Some(x)))
      _ <- generatorSettings.firstNames
        .provideSomeLayer[SeedRandom](tempResidentLayer) >>= (
        x => tempResident.maybeFirstName.set(Some(x))
      )
      _ <- generatorSettings.familyNames
        .provideSomeLayer[SeedRandom](tempResidentLayer) >>= (
        x => tempResident.maybeFamilyName.set(Some(x))
      )
      traits <- generatorSettings.traitsPerResident
        .provideSomeLayer[SeedRandom](tempResidentLayer)
      tr <- ZIO.foreach(0 until traits)(
        _ =>
          generatorSettings.traits
            .provideSomeLayer[SeedRandom](tempResidentLayer)
      )
      _ <- tempResident.maybeTraits.set(Some(tr))
      _ <- generatorSettings.jobs.provideSomeLayer[SeedRandom](
        tempResidentLayer
      ) >>= (x => tempResident.maybeJob.set(Some(x)))
      _ <- generatorSettings.genders.provideSomeLayer[SeedRandom](
        tempResidentLayer
      ) >>= (x => tempResident.maybeGender.set(Some(x)))
      resident <- tempResident.toResident
      res = resident.get
      _ <- relationship match {
        case Some(value) =>
          tempFamily.addTwoWayRelationship(value._2, res, value._1)
        case None => ZIO.unit
      }
    } yield res

  }
}
