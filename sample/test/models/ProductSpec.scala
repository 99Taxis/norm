package models

import java.math.BigDecimal

import play.api.test.{FakeApplication, PlaySpecification, WithApplication}

class ProductSpec extends PlaySpecification {

  "Product.create" should {

    "create a valid product with name and description" in new WithApplication(FakeApplication()) {
      val productName        = "ProductName"
      val productDescription = Some("Text")
      val price              = new BigDecimal("10.00")
      val taxRange           = 2

      val productSave = Product(
        name        =  productName,
        description =  productDescription,
        price       =  price,
        taxRange    =  taxRange,
        inStock     = true
      ).save.get


      val product: Product = Product.find(productSave.id.get)
      product.id          must equalTo(productSave.id)
      product.name        must equalTo(productName)
      product.description must equalTo(productDescription)
      product.price       must equalTo(price)
      product.taxRange    must equalTo(taxRange)
      product.inStock     must beTrue



//      DB.withConnection { implicit connection =>
//
//        val unsafe: Seq[NamedParameter] = Seq("name" -> "name", "description" -> Some("text"),"price" -> new BigDecimal("10.00"), "taxRange" -> 3, "inStock" -> true)
//
//        SQL("""insert into Products(name, description, price, taxRange, inStock)
//          values ({name}, {description}, {price}, {taxRange}, {inStock})""")
//          .on(values:_*).executeInsert()
//      }


    }

//    def values: Seq[NamedParameter] = {
//      Seq[NamedParameter]("name" -> "name", "description" -> Some("text"),"price" -> new BigDecimal("10.00"), "taxRange" -> 3, "inStock" -> true)
//    }

    "create a valid product with option properties equals to None" in new WithApplication(FakeApplication()) {
      val productName        = "ProductName"
      val price              = new BigDecimal("11.00")
      val taxRange           = 3

      val productSave = Product(
        name     =  productName,
        price    =  price,
        taxRange =  taxRange,
        inStock  =  true
      ).save.get

      val product = Product.find(productSave.id.get)
      product.id          must equalTo(productSave.id)
      product.name        must equalTo(productName)
      product.description must beNone
      product.price       must equalTo(price)
      product.taxRange    must equalTo(taxRange)
      product.inStock     must beTrue

    }
  }

  "Product.update" should {

    "update partially a database entry" in new WithApplication(FakeApplication()) {

      val productName    = "ProductName"
      val price          = new BigDecimal("11.00")
      val taxRange       = 3
      val description    = Some("description")
      val inStock        = false

      val newProductName = "NewProductName"
      val newDescription = Some("NewDescription")

      val productId = Product(
          name        =  productName,
          price       =  price,
          taxRange    =  taxRange,
          description =  description,
          inStock     =  inStock
      ).save.get.id.get

      val product = Product.find(productId)
      product.name        = newProductName
      product.description = newDescription
      product.price       = new BigDecimal("17.00")
      product.taxRange    = 20
      product.inStock     = true

      product.update(
          "name"        -> newProductName,
          "description" -> newDescription
      )

      // updates only the specified fields
      val updatedProduct = Product.find(productId)
      updatedProduct.name        must equalTo(newProductName)
      updatedProduct.description must equalTo(newDescription)

      // this properties should not be updated
      updatedProduct.price       must equalTo(price)
      updatedProduct.taxRange    must equalTo(taxRange)
      updatedProduct.inStock     must equalTo(inStock)
    }
  }

  "Product.foreach" should {

    "perform action in all entries" in new WithApplication(FakeApplication()) {

      val productName1    = "ProductName1"
      val price1          = new BigDecimal("11.00")
      val taxRange1       = 3
      val description1    = Some("description")
      val inStock1        = false

      val productName2    = "ProductName2"
      val price2          = new BigDecimal("11.00")
      val taxRange2       = 3
      val description2    = Some("description")
      val inStock2        = false

      val productId1 = Product(
          name        =  productName1,
          price       =  price1,
          taxRange    =  taxRange1,
          description =  description1,
          inStock     =  inStock1
      ).save.get.id.get
      val productId2 = Product(
          name        =  productName2,
          price       =  price2,
          taxRange    =  taxRange2,
          description =  description2,
          inStock     =  inStock2
      ).save.get.id.get

      val nameAfterUpdated: String = "nameUpdated"
      Product.foreach { p =>
        p.update("name" -> nameAfterUpdated)
      }

      Product.find(productId1).name must equalTo(nameAfterUpdated)
      Product.find(productId2).name must equalTo(nameAfterUpdated)
    }
  }


}
