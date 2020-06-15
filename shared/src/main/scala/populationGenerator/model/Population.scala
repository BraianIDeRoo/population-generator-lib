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

import zio.{Ref, ZIO}

case class Population(sectors: Map[TownSector, Vector[Building]]) {
  def allResidents: Iterable[Resident] =
    sectors.values.flatMap(_.flatMap(_.family.residents))

  private def updateMap[A](map: Ref[Map[A, Int]],
                           x: Option[A]): ZIO[Any, Nothing, Unit] =
    for {
      m <- map.get
      _ <- x match {
        case Some(b) =>
          map.update(_ + (b -> (m.get(b) match {
            case Some(number) => number + 1
            case None         => 1
          })))
        case None => ZIO.unit
      }
    } yield ()

  def xDistribution[A](
    filter: Resident => Option[A]
  ): ZIO[Any, Nothing, Map[A, Int]] =
    for {
      map <- Ref.make[Map[A, Int]](Map())
      _ <- ZIO.foreach_(allResidents)(x => updateMap(map, filter(x)))
      res <- map.get
    } yield res

  def xDistributionByY[A, B](
    getA: Resident => Option[A],
    getB: Resident => Option[B]
  ): ZIO[Any, Nothing, Map[A, Map[B, Int]]] =
    for {
      map <- Ref.make[Map[A, Ref[Map[B, Int]]]](Map())
      _ <- ZIO.foreach_(allResidents)(
        x =>
          for {
            m <- map.get
            _ <- getA(x) match {
              case Some(a) =>
                m.get(a) match {
                  case Some(ref) =>
                    updateMap(ref, getB(x))
                  case None =>
                    for {
                      _ <- getB(x) match {
                        case Some(b) =>
                          for {
                            ref <- Ref.make[Map[B, Int]](Map(b -> 1))
                            _ <- map.update(_ + (a -> ref))
                          } yield ()
                        case None => ZIO.unit
                      }
                    } yield ()
                }
              case None => ZIO.unit
            }
          } yield ()
      )
      resMap <- map.get
      res <- ZIO.foreach(resMap)(x => x._2.get.map(y => (x._1, y))).map(_.toMap)
    } yield res

}
