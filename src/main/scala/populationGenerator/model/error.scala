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
