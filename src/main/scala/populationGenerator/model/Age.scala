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
