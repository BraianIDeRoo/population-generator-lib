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
import populationGenerator.Data
import populationGenerator.model.SES.{Poor, Rich}
import populationGenerator.PopulationGenerator
import populationGenerator.PopulationGenerator.PopulationConfigLive
import zio.test.Assertion._
import zio.test._
import zio.{Has, Layer, ZLayer}

object ResidentSpec extends DefaultRunnableSpec {

  val myRandom: ZLayer[Any, Nothing, SeedRandom] =
    ZLayer.succeed(3000L) >>> SeedRandom.live
  val conf: PopulationConfigLive =
    PopulationConfigLive(Data.testConfiguration, Data.testBuildingConfiguration)
  val confLayer: Layer[Nothing, Has[PopulationConfigLive]] =
    ZLayer.succeed(conf)
  val myGenerator: ZLayer[Any, Nothing, PopulationGenerator] =
    confLayer ++ myRandom >>> PopulationGenerator.live

  val tempResidentSuite: Spec[Any, TestFailure[Nothing], TestSuccess] =
    suite("TemResident suite")(
      testM("Can create a temp resident") {
        for {
          res <- Resident.TempResident.make
          maybeSes <- res.maybeSes.get
          maybeAge <- res.maybeAge.get
          maybeGender <- res.maybeGender.get
          maybeRace <- res.maybeRace.get
          maybeFirstName <- res.maybeFirstName.get
          maybeFamilyName <- res.maybeFamilyName.get
          maybeTraits <- res.maybeTraits.get
          maybeJob <- res.maybeJob.get
        } yield
          assert(maybeSes)(isNone) &&
            assert(maybeAge)(isNone) &&
            assert(maybeGender)(isNone) &&
            assert(maybeRace)(isNone) &&
            assert(maybeFirstName)(isNone) &&
            assert(maybeFamilyName)(isNone) &&
            assert(maybeTraits)(isNone) &&
            assert(maybeJob)(isNone)
      },
      testM(
        "Should not create a resident from a temp resident if there are values missing"
      ) {
        for {
          resident <- Resident.TempResident.make
          nothing <- resident.toResident
        } yield assert(nothing)(isNone)
      },
      testM(
        "Should create a resident from a temp resident if all its values are defined"
      ) {
        for {
          tempResident <- Resident.TempResident.make
          _ <- tempResident.maybeSes.set(Some(Rich))
          _ <- tempResident.maybeAge.set(Some(Adult))
          _ <- tempResident.maybeGender.set(Some("male"))
          _ <- tempResident.maybeRace.set(Some("gnome"))
          _ <- tempResident.maybeFirstName.set(Some("Alberto"))
          _ <- tempResident.maybeFamilyName.set(Some("Gonzales"))
          _ <- tempResident.maybeTraits.set(Some(List()))
          _ <- tempResident.maybeJob.set(Some(None))
          resident <- tempResident.toResident
        } yield
          assert(resident)(isSome) &&
            assert(resident.get.ses)(equalTo(Rich)) &&
            assert(resident.get.age)(equalTo(Adult)) &&
            assert(resident.get.gender)(equalTo("male")) &&
            assert(resident.get.race)(equalTo("gnome")) &&
            assert(resident.get.firstName)(equalTo("Alberto")) &&
            assert(resident.get.familyName)(equalTo("Gonzales")) &&
            assert(resident.get.traits)(equalTo(List())) &&
            assert(resident.get.job)(isNone)
      }
    )

  val residentSuite: Spec[Any, TestFailure[error.GeneratorError], TestSuccess] =
    suite("Resident suite")(
      testM(
        "Can generate a resident from a configuration and an empty temp family"
      ) {
        for {
          family <- TempFamily.apply
          resident <- Resident(Data.testConfiguration, family, None)
            .provideLayer(myRandom)

        } yield
          assert(resident.ses)(equalTo(Poor)) &&
            assert(resident.age)(equalTo(Adult)) &&
            assert(resident.gender)(equalTo("female")) &&
            assert(resident.race)(equalTo("half-elf")) &&
            assert(resident.firstName)(equalTo("Olivia")) &&
            assert(resident.familyName)(equalTo("Isabella")) &&
            assert(resident.traits)(equalTo(List("b", "Admirable", "g"))) &&
            assert(resident.job)(isSome(equalTo("field hand")))
      },
      testM("Can generate a resident from a configuration and a temp family") {
        for {
          family <- TempFamily.apply
          old <- Resident(Data.testConfiguration, family, None)
            .provideLayer(myRandom)
          newResident <- Resident(Data.testConfiguration, family, None)
            .provideLayer(myRandom)
        } yield
          assert(newResident.ses)(equalTo(Poor)) &&
            assert(newResident.age)(equalTo(Adult)) &&
            assert(newResident.gender)(equalTo("female")) &&
            assert(newResident.race)(equalTo("half-elf")) &&
            assert(newResident.firstName)(equalTo("Olivia")) &&
            assert(newResident.familyName)(equalTo("Isabella")) &&
            assert(newResident.traits)(equalTo(List("b", "Admirable", "g"))) &&
            assert(newResident.job)(isSome(equalTo("field hand")))
      },
      testM(
        "Can generate a resident from a configuration, a temp family and an existing relationship"
      ) {
        for {
          family <- TempFamily.apply
          old <- Resident(Data.testConfiguration, family, None)
            .provideLayer(myRandom)
          newResident <- Resident(
            Data.testConfiguration,
            family,
            Some(Relationship.parentRelationship, old)
          ).provideLayer(myRandom)
        } yield
          assert(newResident.ses)(equalTo(Poor)) &&
            assert(newResident.age)(equalTo(Child)) &&
            assert(newResident.gender)(equalTo("female")) &&
            assert(newResident.race)(equalTo("human")) &&
            assert(newResident.firstName)(equalTo("Lewis")) &&
            assert(newResident.familyName)(equalTo("Isabella")) &&
            assert(newResident.traits)(equalTo(List("Adventurous", "d"))) &&
            assert(newResident.job)(isSome(equalTo("field hand")))
      }
    )

  override def spec: ZSpec[zio.test.environment.TestEnvironment, Any] =
    suite("All resident tests")(tempResidentSuite, residentSuite)
}
