package necromancy

import wisteria.*
import anticipation.*
import rudiments.*
import gossamer.*
import spectacular.*
import vacuous.*
import prepositional.*

object Askable extends ProductDerivable[Askable]:
  inline def join[DerivationType <: Product: ProductReflection]: DerivationType is Askable =
    def ask(value: DerivationType, prefix: Optional[Text]): Map[Optional[Text], Text] =
      fields(value):
        [FieldType] => field => context.ask(field, prefix.lay(label)(_+t".$label"))

      . foldLeft(Map[Optional[Text], Text]())(_ ++ _)

    def answer(answers: Map[Optional[Text], Text], prefix: Optional[Text]): DerivationType =
      construct:
        [FieldType] => context => context.answer(answers, prefix.lay(label)(_+t".$label"))

    Askable[DerivationType](ask, answer)

  def apply[ValueType]
     (ask0:    (ValueType, Optional[Text]) => Map[Optional[Text], Text],
      answer0: (Map[Optional[Text], Text], Optional[Text]) => ValueType)
          : ValueType is Askable =
    new Askable:
      type Self = ValueType

      def ask(value: ValueType, prefix: Optional[Text]): Map[Optional[Text], Text] =
        ask0(value, prefix)

      def answer(answers: Map[Optional[Text], Text], prefix: Optional[Text]): ValueType =
        answer0(answers, prefix)

  given [ValueType](using Decoder[ValueType], ValueType is Encodable in Text)
      => ValueType is Askable:

    def ask(value: ValueType, prefix: Optional[Text]): Map[Optional[Text], Text] =
      Map(prefix -> value.encode)

    def answer(answers: Map[Optional[Text], Text], prefix: Optional[Text]): ValueType =
      answers(prefix).decode

trait Askable:
  type Self
  def ask(value: Self, prefix: Optional[Text] = Unset): Map[Optional[Text], Text]
  def answer(answers: Map[Optional[Text], Text], prefix: Optional[Text]): Self
