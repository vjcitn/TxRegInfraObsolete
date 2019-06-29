library(TxRegInfra)

context("test ATLAS") # ATLAS service is used
test_that("listAllCollections succeeds", {
if (verifyHasMongoCmd()) {
  c1 <- listAllCollections(url=URL_txregInAWS(), db="txregnet")
  expect_true(length(c1)>=2640)
  }
})

context("test collection names as anticipated")
test_that("collection names are as anticipated", {
if (verifyHasMongoCmd()) {
  c1 <- listAllCollections(url=URL_txregInAWS(), db="txregnet")
  expect_true(all.equal(c1, rownames(basicColData)))
  }
})

