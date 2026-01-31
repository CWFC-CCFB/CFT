##########################
# Test for biomass model
##########################

require(CFT)

# refDataBiomass3parms <- read.csv("...")
# save(refDataBiomass3parms, file = "./tests/testthat/refDataBiomass3parms.RData")

# refDataBiomass2parms <- read.csv("...")
# matchTable <- as.data.frame(table(refDataBiomass3parms$ess,refDataBiomass3parms$esslat))
# matchTable <- matchTable[which(matchTable$Freq > 0),]
# colnames(matchTable) <- c("ess", "esslat", "Freq")
# refDataBiomass2parms <- merge(refDataBiomass2parms, matchTable[,c("ess", "esslat")], by = "ess")
# save(refDataBiomass2parms, file = "./tests/testthat/refDataBiomass2parms.RData")

# nbTrees <- 1000000
# speciesCode <- rep("AbiesBalsamea",nbTrees)
# dbhCm <- runif(min = 10, max = 30, n = nbTrees)
# heightM <- runif(min = 10, max = 30, n = nbTrees)


test_that("Expect error message", {
  expect_error(getAboveGroundBiomassMg(1, 16, 13), "speciesCode argument")
  expect_error(getAboveGroundBiomassMg("SAB", "error", 13), "dbhCm argument")
  expect_error(getAboveGroundBiomassMg("SAB", 16, "error"), "heightM argument")
  expect_error(getAboveGroundBiomassMg("XXX", 16, 13), "is not recognized!")
})

getAboveGroundBiomassMg("AbiesBalsamea", as.integer(12))
getAboveGroundBiomassMg("AbiesBalsamea", as.integer(12), as.integer(10))
getAbovegroundBiomassSpeciesList()

load("refDataBiomass3parms.RData")
# load("./tests/testthat/refDataBiomass3parms.RData")

biomass <- getAboveGroundBiomassMg(refDataBiomass3parms$esslat, refDataBiomass3parms$dbh, refDataBiomass3parms$height)
test_that("Testing biomass with complete model against ground truth", {
  expect_true(all(abs(biomass - refDataBiomass3parms$pred_total * 0.001) < 1E-8))
})

load("refDataBiomass2parms.RData")
# load("./tests/testthat/refDataBiomass2parms.RData")
biomass <- getAboveGroundBiomassMg(refDataBiomass2parms$esslat, refDataBiomass2parms$dbh)
test_that("Testing biomass with complete model against ground truth", {
  expect_true(all(abs(biomass - refDataBiomass2parms$pred_total * 0.001) < 1E-8))
})

shutdownJava()
