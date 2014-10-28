package models

import java.math.BigDecimal
import norm.{NormCompanion, Norm}

case class Product(
  var name:        String,
  var description: Option[String] = None,
  var price:       BigDecimal,

  var taxRange:    Int,
  var inStock:     Boolean,
  id:              Option[Long] = None) extends Norm[Product]


object Product extends NormCompanion[Product]
