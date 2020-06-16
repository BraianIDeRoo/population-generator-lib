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

import zio.{IO, Ref, ZIO, ZLayer}

case class ResidentPair(origin: Resident, target: Resident)

case class Family(residents: Vector[Resident],
                  relationships: Map[(Resident, Resident), List[Relationship]])

case class TempFamily(
  tempResidents: Ref[Vector[Resident]],
  tempRelationships: Ref[Map[(Resident, Resident), List[Relationship]]]
) {
  def toFamily: ZIO[Any, Nothing, Family] =
    for {
      residents <- tempResidents.get
      relationships <- tempRelationships.get
    } yield Family(residents, relationships)

  def addResident(resident: Resident): IO[Nothing, Unit] =
    tempResidents.update(_ :+ resident)

  private def updateRelationships(): ZIO[Any, Nothing, Unit] = {
    for {
      relationships <- tempRelationships.get
      familyLayer = ZLayer.succeed(this)
      _ <- ZIO.foreach_(relationships) { x =>
        val affectedResidentsLayer = ZLayer.succeed(x._1)
        val fullLayer = familyLayer ++ affectedResidentsLayer
        ZIO.foreach_(x._2)(_.onAdded.provideLayer(fullLayer))
      }
    } yield ()
  }

  def addOneWayRelationship(
    origin: Resident,
    target: Resident,
    relationShip: Relationship
  ): ZIO[Any, Nothing, Unit] =
    tempRelationships.update(
      x =>
        x + ((origin, target) -> (
          x.get((origin, target)) match {
            case Some(value) =>
              if (value.contains(relationShip)) value
              else value :+ relationShip
            case None => List(relationShip)
          }
        ))
    ) *> updateRelationships()

  def addTwoWayRelationship(
    origin: Resident,
    target: Resident,
    relationShip: Relationship
  ): ZIO[Any, Nothing, Unit] =
    addOneWayRelationship(origin, target, relationShip) *>
      addOneWayRelationship(target, origin, relationShip)

}

object TempFamily {
  def apply: ZIO[Any, Nothing, TempFamily] =
    for {
      tempResidents <- Ref.make[Vector[Resident]](Vector())
      tempRelationships <- Ref
        .make[Map[ResidentPair, List[Relationship]]](Map())
    } yield TempFamily(tempResidents, tempRelationships)
}
