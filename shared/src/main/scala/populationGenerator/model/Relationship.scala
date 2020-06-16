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

import zio.{Has, ZIO}

case class Relationship(
  originName: String,
  targetName: String,
  onAdded: ZIO[Has[TempFamily] with Has[(Resident, Resident)], Nothing, Unit]
)

object Relationship {

  val parentRelationship: Relationship =
    Relationship("parent", "child", ZIO.unit)

  val spouseRelationship: Relationship =
    Relationship(
      "spouse",
      "spouse",
      for {
        tempFamily <- ZIO.access[Has[TempFamily]](_.get)
        affectedResidents <- ZIO.access[Has[(Resident, Resident)]](_.get)
        tempRelations <- tempFamily.tempRelationships.get
        aux = tempRelations.filter(x => x._1._1 == affectedResidents._2)
        aux2 = aux.filter(
          x =>
            x._2.exists(y => y.originName == "parent") && !tempRelations
              .contains((affectedResidents._1, x._1._2))
        )
        _ <- ZIO.foreach_(aux2)(
          x =>
            tempFamily.addTwoWayRelationship(
              affectedResidents._1,
              x._1._2,
              parentRelationship
          )
        )
        /*
        _ <- ZIO.collect(tempRelations) {
          case ((affectedResidents._2, other), relation)
              if relation.originName == "parent" && !tempRelations
                .contains((affectedResidents._1, other)) =>
            tempFamily.addTwoWayRelationship(
              affectedResidents._1,
              other,
              parentRelationship
            )
          case _ => ZIO.fail(None)
        }

       */

      } yield ()
    )
}
