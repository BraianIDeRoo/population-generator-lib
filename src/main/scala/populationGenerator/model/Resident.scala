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
import populationGenerator.model.Config.{
  ChildSettings,
  GeneratorSettings,
  SpouseSettings
}
import populationGenerator.model.error.GeneratorError
import zio.{Has, Layer, Ref, ZIO, ZLayer}

case class Resident(ses: SES,
                    age: Age,
                    gender: String,
                    race: String,
                    firstName: String,
                    familyName: String,
                    parents: Iterable[Resident],
                    children: Iterable[Resident],
                    spouse: Option[Resident],
                    traits: Iterable[String],
                    job: Option[String]) {}

object Resident {

  case class TempResident(maybeSes: Ref[Option[SES]],
                          maybeAge: Ref[Option[Age]],
                          maybeGender: Ref[Option[String]],
                          maybeRace: Ref[Option[String]],
                          maybeFirstName: Ref[Option[String]],
                          maybeFamilyName: Ref[Option[String]],
                          maybeParents: Ref[Option[Iterable[Resident]]],
                          maybeChildren: Ref[Option[Iterable[Resident]]],
                          maybeSpouse: Ref[Option[Option[Resident]]],
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
        parents <- maybeParents.get
        children <- maybeChildren.get
        spouse <- maybeSpouse.get
        traits <- maybeTraits.get
        job <- maybeJob.get
        res <- if (ses.isDefined &&
                   age.isDefined &&
                   gender.isDefined &&
                   race.isDefined &&
                   firstName.isDefined &&
                   familyName.isDefined &&
                   parents.isDefined &&
                   children.isDefined &&
                   spouse.isDefined &&
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
              parents.get,
              children.get,
              spouse.get,
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
        maybeParents <- Ref.make[Option[Iterable[Resident]]](None)
        maybeChildren <- Ref.make[Option[Iterable[Resident]]](None)
        maybeSpouse <- Ref.make[Option[Option[Resident]]](None)
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
          maybeParents,
          maybeChildren,
          maybeSpouse,
          maybeTraits,
          maybeJob
        )
  }

  def fromConfig(
    generatorSettings: GeneratorSettings,
    parents: Iterable[Resident],
    children: Iterable[Resident],
    spouse: Option[Resident]
  ): ZIO[SeedRandom, GeneratorError, Resident] =
    Resident.apply(generatorSettings, parents, children, spouse)

  def fromSpouseConfig(
    spouse: Resident,
    generatorSettings: GeneratorSettings,
    spouseSettings: SpouseSettings[GeneratorSettings]
  ): ZIO[SeedRandom, Nothing, Option[Resident]] = {
    def addLayerRef[E, A](
      layer: ZLayer[Any, Nothing, Has[GeneratorSettings] with Has[Resident] with Has[
        TempResident
      ]],
      action: ZIO[SeedRandom with Has[GeneratorSettings] with Has[Resident] with Has[
        TempResident
      ], E, A],
      ref: Ref[Option[A]]
    ): ZIO[SeedRandom, E, Unit] =
      action.provideSomeLayer[SeedRandom](layer) >>= (x => ref.set(Some(x)))

    def addLayer[E, A](layer: ZLayer[Any, Nothing, Has[GeneratorSettings] with Has[
                         Resident
                       ] with Has[TempResident]],
                       action: ZIO[SeedRandom with Has[GeneratorSettings] with Has[
                         Resident
                       ] with Has[TempResident], E, A]) =
      action.provideSomeLayer[SeedRandom](layer)

    for {
      tempSpouse <- TempResident.make
      tempSpouseLayer = ZLayer.succeed(tempSpouse)
      hasSpouse <- generatorSettings.hasSpouse
        .provideSomeLayer[SeedRandom](tempSpouseLayer)
      spouse <- if (hasSpouse) {
        val resLayer = ZLayer.succeed(spouse)
        val genLayer = ZLayer.succeed(generatorSettings)
        val layer = genLayer ++ resLayer ++ tempSpouseLayer
        import spouseSettings._
        for {
          _ <- addLayerRef(layer, socioeconomicStatus, tempSpouse.maybeSes)
          _ <- addLayerRef(layer, age, tempSpouse.maybeAge)
          _ <- addLayerRef(layer, race, tempSpouse.maybeRace)
          _ <- addLayerRef(layer, firstName, tempSpouse.maybeFirstName)
          _ <- addLayerRef(layer, familyName, tempSpouse.maybeFamilyName)
          traitNumber <- addLayer(layer, traitsPerResident)
          traits <- ZIO.foreach(0 until traitNumber)(
            _ => addLayer(layer, traits)
          )
          _ <- tempSpouse.maybeTraits.set(Some(traits))
          _ <- addLayerRef(layer, job, tempSpouse.maybeJob)
          _ <- addLayerRef(layer, gender, tempSpouse.maybeGender)
          aux <- tempSpouse.toResident
        } yield aux
      } else ZIO.none
    } yield spouse
  }

  def fromChildSettings(
    parents: Iterable[Resident],
    generatorSettings: GeneratorSettings,
    childSettings: ChildSettings[GeneratorSettings]
  ): ZIO[SeedRandom, GeneratorError, Resident] = {
    type Env = SeedRandom
      with Has[GeneratorSettings]
      with Has[Iterable[Resident]]
      with Has[TempResident]

    def addLayerRef[E, A](
      layer: ZLayer[Any, Nothing, Has[GeneratorSettings] with Has[
        Iterable[Resident]
      ] with Has[TempResident]],
      action: ZIO[Env, E, A],
      ref: Ref[Option[A]]
    ): ZIO[SeedRandom, E, Unit] =
      action.provideSomeLayer[SeedRandom](layer) >>= (x => ref.set(Some(x)))

    def addLayer[E, A](
      layer: ZLayer[Any, Nothing, Has[GeneratorSettings] with Has[
        Iterable[Resident]
      ] with Has[TempResident]],
      action: ZIO[Env, E, A]
    ): ZIO[SeedRandom, E, A] =
      action.provideSomeLayer[SeedRandom](layer)

    val resLayer: Layer[Nothing, Has[Iterable[Resident]]] =
      ZLayer.succeed(parents)
    val genLayer: Layer[Nothing, Has[GeneratorSettings]] =
      ZLayer.succeed(generatorSettings)

    import childSettings._
    for {
      tempChild <- TempResident.make
      tempChildLayer = ZLayer.succeed(tempChild)

      layer = genLayer ++ resLayer ++ tempChildLayer
      _ <- addLayerRef(layer, socioeconomicStatus, tempChild.maybeSes)
      _ <- addLayerRef(layer, age, tempChild.maybeAge)
      _ <- addLayerRef(layer, race, tempChild.maybeRace)
      _ <- addLayerRef(layer, firstName, tempChild.maybeFirstName)
      _ <- addLayerRef(layer, familyName, tempChild.maybeFamilyName)
      traitNumber <- addLayer(layer, traitsPerResident)
      traits <- ZIO.foreach(0 until traitNumber)(_ => addLayer(layer, traits))
      _ <- tempChild.maybeTraits.set(Some(traits))
      _ <- addLayerRef(layer, job, tempChild.maybeJob)
      _ <- addLayerRef(layer, gender, tempChild.maybeGender)
      _ <- tempChild.maybeSpouse.set(Some(None))
      _ <- tempChild.maybeParents.set(Some(parents))
      _ <- tempChild.maybeChildren.set(Some(List()))
      child <- tempChild.toResident
    } yield child.get
  }

  def apply(
    generatorSettings: GeneratorSettings,
    parents: Iterable[Resident],
    children: Iterable[Resident],
    spouse: Option[Resident]
  ): ZIO[SeedRandom, GeneratorError, Resident] = {

    for {
      tempResident <- TempResident.make
      tempResidentLayer = ZLayer.succeed(tempResident)
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
      _ <- tempResident.maybeSpouse.set(Some(spouse))
      _ <- tempResident.maybeParents.set(Some(parents))
      _ <- tempResident.maybeChildren.set(Some(children))
      resident <- tempResident.toResident
    } yield resident.get

  }
}
