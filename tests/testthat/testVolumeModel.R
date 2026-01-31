##########################
# Test for volume model
##########################

require(CFT)

volumeSAB <- getUnderbarkMerchantableVolumeDm3("SAB", 16, 13)

volumeSAB <- getUnderbarkMerchantableVolumeDm3("SAB", as.integer(16), as.integer(13))

test_that("Expect error message", {
  expect_error(getMerchantableVolumeDm3(1, 16, 13), "speciesCode argument")
  expect_error(getMerchantableVolumeDm3("SAB", "error", 13), "dbhCm argument")
  expect_error(getMerchantableVolumeDm3("SAB", 16, "error"), "heightM argument")
  expect_error(getUnderbarkMerchantableVolumeDm3("XXX", 16, 13), "does not support species XXX")
  expect_error(getUnderbarkMerchantableVolumeDm3(c("SAB", "XXX2", "XX2", "XX2", "EPR"), 16, 13), "does not support species XXX2")
})

test_that("Testing volume SAB", {
  expect_equal(volumeSAB, 105.5542, tolerance = 1E-4)
})

volumeSAB <- getUnderbarkMerchantableVolumeDm3("SAB", c(16, 25), c(13,18))

test_that("Testing volume SAB", {
  expect_equal(volumeSAB[1], 105.5542, tolerance = 1E-4)
  expect_equal(volumeSAB[2], 361.4733, tolerance = 1E-4)
})

test_that("Testing available species codes", {
  expect_equal(suppressWarnings(length(getUnderbarkMerchantableVolumeSpeciesList())), 26)
})


test_that("Testing available Latin names", {
  expect_equal(length(getMerchantableVolumeSpeciesList(latinName = T)), 26)
})


#load("./tests/testthat/DummyData.RData")
load("DummyData.RData")

volPredNew <- getUnderbarkMerchantableVolumeDm3(Dummy$ESSENCE, Dummy$DHP, Dummy$Hauteur)

#save(volPred, file = "./tests/testthat/refVolPred.RData")

#load("./tests/testthat/refVolPred.RData")
load("refVolPred.RData")

test_that("Testing that Gabriel's bug (REpicea Bug #63) is fixed", {
  expect_true(all(abs(volPred - volPredNew) < 1E-8))
})


volumeSAB <- getMerchantableVolumeDm3("SAB", 16, 13, overbark = T)

test_that("Testing volume SAB", {
  expect_equal(volumeSAB, 118.2206, tolerance = 1E-4)
})



shutdownJava()
