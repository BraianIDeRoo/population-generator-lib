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

sealed trait Age {
  def previous: Option[Age]
  def next: Option[Age]
}
case object Child extends Age {
  override def previous: Option[Age] = None

  override def next: Option[Age] = Some(Adult)
}
case object Adult extends Age {
  override def previous: Option[Age] = Some(Child)

  override def next: Option[Age] = Some(Elderly)
}
case object Elderly extends Age {
  override def previous: Option[Age] = Some(Adult)

  override def next: Option[Age] = None
}
