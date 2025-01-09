/*
    Legerdemain, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package legerdemain

import anticipation.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*
import wisteria.*

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
