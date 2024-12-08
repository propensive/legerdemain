package necromancy

extension [ValueType: Askable](value: ValueType)
  def ask: Inquiry[ValueType] = Inquiry(ValueType.ask(value))
