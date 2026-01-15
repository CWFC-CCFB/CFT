###################################################
# Aboveground biomass model (Lambert et al. 2005)
###################################################

.lambert2005SpeciesCodes <- NULL
.biomassPackageName <- "canforservutility.predictor.biomass.lambert2005."

#'
#' Calculate the aboveground biomass of a list of trees
#'
#'
#' @param speciesCode the species code (see the getAbovegroundBiomassSpeciesList function)
#' @param dbhCm diameter at breast height (cm)
#' @param heightM tree height (m)
#'
#' @return the aboveground biomass (Mg of dry matter)
#'
#' @export
getAboveGroundBiomassMg <- function(speciesCode, dbhCm, heightM=NULL) {
  .connectToCFT()
  areRecognized <- speciesCode %in% getAbovegroundBiomassSpeciesList()
  if (any(!areRecognized)) {
    indices <- which(!areRecognized)
    notRecognized <- unique(speciesCode[indices])
    if (length(notRecognized) == 1) {
      stop(paste("This species code:", notRecognized, "is not recognized! Please use the getUnderbarkMerchantableVolumeSpeciesList function to get the list of possible codes."))
    } else {
      stop(paste("These species codes:", paste(notRecognized, collapse = ","), "are not recognized! Please use the getUnderbarkMerchantableVolumeSpeciesList function to get the list of possible codes."))
    }
  }
  predictor <- J4R::createJavaObject(paste0(.biomassPackageName, "Lambert2005BiomassPredictor"))
  if (is.null(heightM)) {
    biomassPred <- predictor$predictTotalBiomassMg(speciesCode, dbhCm)
  } else {
    biomassPred <- predictor$predictTotalBiomassMg(speciesCode, dbhCm, heightM)
  }
  return(biomassPred)
}


#'
#' Provide the list of species codes for Lambert et al.'s model.
#'
#' Provide all of the possible species code that can be used with the
#' getAboveGroundBiomassMg function.
#'
#' @export
getAbovegroundBiomassSpeciesList <- function() {
  if (is.null(.lambert2005SpeciesCodes)) {
    .connectToCFT()
    species <- J4R::callJavaMethod(paste0(.biomassPackageName, "Lambert2005Tree$Lambert2005Species"), "values")
    individualSpecies <- J4R::getAllValuesFromArray(species)
    .lambert2005SpeciesCodes <- individualSpecies$name()
  }
  return(.lambert2005SpeciesCodes)
}

.getTotalBiomassIndex <- function() {
  if (is.null(.totalBiomassIndex)) {
    .connectToCFT()
    totalCompartment <- J4R::createJavaObject(paste0(.biomassPackageName, "Lambert2005BiomassPredictor$BiomassCompartment"), "TOTAL")
    .totalBiomassIndex <- totalCompartment$ordinal()
  }
  return(.totalBiomassIndex)
}
