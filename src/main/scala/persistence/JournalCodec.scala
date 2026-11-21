package rizoma.persistence

import io.circe.syntax.*
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import rizoma.domain.crop.activity.{
  Disposition,
  InputSource,
  InputUse,
  Journal,
  Labor,
  Money,
  Output,
  Quantity,
  StockLink
}
import rizoma.domain.inventory.StockId
import rizoma.prelude.Id

private[persistence] object JournalCodec:
  private def validated[A](value: Either[String, A], cursor: HCursor): Decoder.Result[A] =
    value.left.map(error => DecodingFailure(error, cursor.history))

  given Encoder[Quantity] =
    Encoder.instance(q => Json.obj("amount" -> q.amount.asJson, "unit" -> q.unit.asJson))
  given Decoder[Quantity] = Decoder.instance(c =>
    for
      amount <- c.get[BigDecimal]("amount")
      unit <- c.get[String]("unit")
      quantity <- validated(Quantity.from(amount, unit), c)
    yield quantity
  )

  given Encoder[Money] =
    Encoder.instance(m => Json.obj("amount" -> m.amount.asJson, "currency" -> m.currency.asJson))
  given Decoder[Money] = Decoder.instance(c =>
    for
      amount <- c.get[BigDecimal]("amount")
      currency <- c.get[String]("currency")
      money <- validated(Money.from(amount, currency), c)
    yield money
  )

  given Encoder[InputSource] = Encoder.encodeString.contramap(_.toString)
  given Decoder[InputSource] = Decoder.decodeString.emap(value =>
    InputSource.values.find(_.toString == value).toRight(s"Unknown input source: $value")
  )
  given Encoder[StockLink] = Encoder.instance(s =>
    Json.obj("stockId" -> s.stockId.toString.asJson, "location" -> s.location.asJson)
  )
  given Decoder[StockLink] = Decoder.instance(c =>
    for
      raw <- c.get[String]("stockId")
      id <- validated(Id.parse(raw).left.map(_.toString), c)
      location <- c.get[String]("location")
      link <- validated(StockLink.from(StockId.from(id), location), c)
    yield link
  )
  given Encoder[InputUse] = Encoder.instance(i =>
    Json.obj(
      "name" -> i.name.asJson,
      "quantity" -> i.quantity.asJson,
      "method" -> i.method.asJson,
      "cost" -> i.cost.asJson,
      "source" -> i.source.asJson,
      "stock" -> i.stock.asJson
    )
  )
  given Decoder[InputUse] = Decoder.instance(c =>
    for
      name <- c.get[String]("name")
      quantity <- c.get[Quantity]("quantity")
      method <- c.get[Option[String]]("method")
      cost <- c.get[Option[Money]]("cost")
      source <- c.get[Option[InputSource]]("source")
      stock <- c.get[Option[StockLink]]("stock")
      input <- validated(
        InputUse.from(name, quantity, method, cost, source.getOrElse(InputSource.Unknown), stock),
        c
      )
    yield input
  )

  given Encoder[Labor] = Encoder.instance(l =>
    Json.obj("worker" -> l.worker.asJson, "hours" -> l.hours.asJson, "cost" -> l.cost.asJson)
  )
  given Decoder[Labor] = Decoder.instance(c =>
    for
      worker <- c.get[String]("worker")
      hours <- c.get[BigDecimal]("hours")
      cost <- c.get[Option[Money]]("cost")
      labor <- validated(Labor.from(worker, hours, cost), c)
    yield labor
  )

  given Encoder[Disposition] = Encoder.encodeString.contramap(_.toString)
  given Decoder[Disposition] = Decoder.decodeString.emap(value =>
    Disposition.values.find(_.toString == value).toRight(s"Unknown output disposition: $value")
  )
  given Encoder[Output] = Encoder.instance(o =>
    Json.obj(
      "name" -> o.name.asJson,
      "quantity" -> o.quantity.asJson,
      "disposition" -> o.disposition.asJson,
      "value" -> o.value.asJson
    )
  )
  given Decoder[Output] = Decoder.instance(c =>
    for
      name <- c.get[String]("name")
      quantity <- c.get[Quantity]("quantity")
      disposition <- c.get[Disposition]("disposition")
      value <- c.get[Option[Money]]("value")
      output <- validated(Output.from(name, quantity, disposition, value), c)
    yield output
  )

  given Encoder[Journal] = Encoder.instance(j =>
    Json.obj(
      "operator" -> j.operator.asJson,
      "notes" -> j.notes.asJson,
      "inputs" -> j.inputs.asJson,
      "labor" -> j.labor.asJson,
      "outputs" -> j.outputs.asJson
    )
  )
  given Decoder[Journal] = Decoder.instance(c =>
    for
      operator <- c.get[Option[String]]("operator")
      notes <- c.get[Option[String]]("notes")
      inputs <- c.get[List[InputUse]]("inputs")
      labor <- c.get[List[Labor]]("labor")
      outputs <- c.get[List[Output]]("outputs")
      journal <- validated(Journal.from(operator, notes, inputs, labor, outputs), c)
    yield journal
  )
