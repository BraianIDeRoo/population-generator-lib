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

import populationGenerator.model.{ResidentSpec, error}
import populationGenerator.PopulationGenerator
import zio.test.Assertion._
import zio.test._

object PopulationGeneratorSpec extends DefaultRunnableSpec {
  // TODO more tests
  val populationSuite
    : Spec[Any, TestFailure[error.GeneratorError], TestSuccess] =
    suite("Population generator suite")(testM("can generate a population") {
      for {
        population <- PopulationGenerator
          .generatePopulation(20)
          .provideLayer(ResidentSpec.myGenerator)
      } yield assert(population.allResidents.size)(isGreaterThanEqualTo(20))
    })

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    populationSuite
}
