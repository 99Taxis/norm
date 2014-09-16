Norm
=======

Norm is an utility built on top of Anorm to be used in playframework scala projects. It's not a complete ORM with a lot of complex things.

Why
===============

1. Because repeating yourself is boring. For simple cases, when you create 2 or 3 classes(models?) to access the database, you'll end up writting a lot of the same things.
2. Because to learn even a new idiomatic way to write the same old sql is not a great innovation

Should I use it?
================

1. If you're writting a very complex model (joins, inheritance,...) - for now - No
2. If you're not comfortable with unchecked type - No
3. If you want transaction between models - for now - No
4. If you have a lot of models with simple access pattern and don't want to learn a new way to access SQL dbs - yes


Getting Start
-------------

1.  Add Norm to your project:

This project was forked from [Ricardo Lazaro's norm](https://github.com/ricardolazaro/norm) but it diverged too much and is not published yet, so just copy the file [Norm.scala](https://github.com/gcaliari/norm/blob/master/app/franeworks/norm/Norm.scala) to your project.

2. Create a model class extending Norm:

  ```scala
    import models.frameworks.{NormCompanion, Norm}
    import play.api.libs.json.{JsObject, JsValue, Json}

    case class Product(
                           name:              String,
                           otherEntityId:     Long,
                           monetaryValue:     BigDecimal,
                           config:            JsValue,
                           enabled:           Boolean = true,
                           priority:          Int = 0,
                           id:                Pk[Long] = NotAssigned
                           ) extends Norm[Product] {


    object Product extends NormCompanion[Product]{
      implicit val ProductFormat = Json.format[Product]
      def findByOtherEntity(otherEntity: OtherEntity): List[Product] = findByOtherEntity(otherEntity.id.get)
      def findByOtherEntity(otherEntityId: Long):      List[Product] = findByProperties(Map("otherEntityId" -> otherEntityId, "enabled" -> true))
    }
  ```

3. Inserting a product:
    ```scala
        val product = Product(someName, otherEntityId, someMonetaryValue, someJson).save
    ```

Or

    ```scala
        val optionId = Product.create(
          Map(
            "name"          ->  "productName",
            "otherEntityId" ->  1l,
            "monetaryValue" ->  new BigDecimal("10.00"),
            "config"        ->  JsNull
          )
        )
    ```

4. Updating a product

    ```scala
        product.name          = newProductName
        product.otherEntityId = otherProductId
        product.update
    ```

5. Partial update

    ```scala
        product.update(
          Map(
            "name"        -> newProductName,
            "otherEntityId" -> otherProductId
          )
        )
    ```
    
6. Find a product

  ```scala
    val product = Product.find(2l) // by id 2l
  ```
  
  ```scala
    val productOption = Product.findOption(2l) // by id 2l
  ```

  ```scala
    val products = Product.findByProperty("name", someName)
  ```

  ```scala
    val products = Product.findByProperties(Map("name" -> someName, "enabled" -> true))
  ```

