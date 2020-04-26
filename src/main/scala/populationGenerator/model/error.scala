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

object error {
  trait GeneratorError extends Exception {
    def msg: String
    def fullMsg: String = this.getClass.getSimpleName + " : " + msg
  }

  object GeneratorError {

    case class Simple(msg: String) extends GeneratorError

    object Simple {
      def fromThrowable(throwable: Throwable): Simple =
        Simple(throwable.getMessage)
    }

    trait BaseChained[A <: GeneratorError] extends GeneratorError {
      def cause: A
      def hint: String
      override def msg: String = s"$hint;\n\t cause was: ${cause.fullMsg}"
    }

    case class Chained[A <: GeneratorError](hint: String, cause: A)
        extends BaseChained[A]

    case class Accumulated[A <: GeneratorError](all: Seq[A])
        extends GeneratorError {
      override def msg: String = all.map(_.fullMsg).toList.mkString(" ; ")
    }
  }

}
