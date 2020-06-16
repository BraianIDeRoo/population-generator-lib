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

import populationGenerator.Data
import populationGenerator.model.ResidentSpec.myRandom
import zio.test.Assertion._
import zio.test._

object FamilySpec extends DefaultRunnableSpec {

  val tempFamilySuite
    : Spec[Any, TestFailure[error.GeneratorError], TestSuccess] =
    suite("TempFamily suite")(
      testM("Can create a temp family") {
        for {
          family <- TempFamily.apply
          residents <- family.tempResidents.get
          relations <- family.tempRelationships.get
        } yield
          assert(residents)(isEmpty) &&
            assert(relations)(isEmpty)
      },
      testM("Can generate a family") {
        for {
          tempFamily <- TempFamily.apply
          family <- tempFamily.toFamily
        } yield
          assert(family.relationships)(isEmpty) &&
            assert(family.residents)(isEmpty)
      },
      testM("Can add residents to the family") {
        for {
          tempFamily <- TempFamily.apply
          resident <- Resident(Data.testConfiguration, tempFamily, None)
            .provideLayer(myRandom)
          _ <- tempFamily.addResident(resident)
          residents <- tempFamily.tempResidents.get
          relations <- tempFamily.tempRelationships.get
        } yield
          assert(residents)(contains(resident)) &&
            assert(relations)(isEmpty)
      },
      testM("Can add a one way relationship between 2 residents") {
        for {
          tempFamily <- TempFamily.apply
          resident <- Resident(Data.testConfiguration, tempFamily, None)
            .provideLayer(myRandom)
          _ <- tempFamily.addResident(resident)
          resident2 <- Resident(Data.testConfiguration, tempFamily, None)
            .provideLayer(myRandom)
          _ <- tempFamily.addResident(resident2)
          relationship = Relationship.parentRelationship
          _ <- tempFamily
            .addOneWayRelationship(resident, resident2, relationship)
          residents <- tempFamily.tempResidents.get
          relations <- tempFamily.tempRelationships.get
        } yield
          assert(residents)(contains(resident)) &&
            assert(residents)(contains(resident2)) &&
            assert(relations)(
              contains(
                ResidentPair(resident, resident2) -> List(
                  Relationship.parentRelationship
                )
              )
            )
      },
      testM("Can add a two way relationship between 2 residents") {
        for {
          tempFamily <- TempFamily.apply
          resident <- Resident(Data.testConfiguration, tempFamily, None)
            .provideLayer(myRandom)
          _ <- tempFamily.addResident(resident)
          resident2 <- Resident(Data.testConfiguration, tempFamily, None)
            .provideLayer(myRandom)
          _ <- tempFamily.addResident(resident2)
          relationship = Relationship.spouseRelationship
          _ <- tempFamily
            .addTwoWayRelationship(resident, resident2, relationship)
          residents <- tempFamily.tempResidents.get
          relations <- tempFamily.tempRelationships.get
        } yield
          assert(residents)(contains(resident)) &&
            assert(residents)(contains(resident2)) &&
            assert(relations)(
              contains(
                ResidentPair(resident, resident2) -> List(
                  Relationship.spouseRelationship
                )
              )
            ) &&
            assert(relations)(
              contains(
                ResidentPair(resident2, resident) -> List(
                  Relationship.spouseRelationship
                )
              )
            )
      }
    )

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    tempFamilySuite
}
