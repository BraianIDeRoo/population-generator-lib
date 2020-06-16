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
  onAdded: ZIO[Has[TempFamily] with Has[ResidentPair], Nothing, Unit]
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
        affectedResidents <- ZIO.access[Has[ResidentPair]](_.get)
        tempRelations <- tempFamily.tempRelationships.get
        aux = tempRelations.filter(
          pairRelations => pairRelations._1.origin == affectedResidents.target
        )
        aux2 = aux.filter(
          pairRelations =>
            pairRelations._2
              .exists(_.originName == "parent") && !tempRelations
              .contains(
                ResidentPair(affectedResidents.origin, pairRelations._1.target)
            )
        )
        _ <- ZIO.foreach_(aux2) { pairRelations =>
          tempFamily.addTwoWayRelationship(
            affectedResidents.origin,
            pairRelations._1.target,
            parentRelationship
          )
        }

      } yield ()
    )
}
