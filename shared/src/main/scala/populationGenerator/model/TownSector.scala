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

sealed trait TownSector

object TownSector {
  case object Slums extends TownSector
  case object Poor extends TownSector
  case object MiddleClass extends TownSector
  case object UpperMiddleClass extends TownSector
  case object Exclusive extends TownSector
  case object PrivateState extends TownSector
}
