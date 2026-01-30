#######################
# Volume model (Fortin et al. 2007)
#######################



speciesCodes <- c("BOG", "BOJ", "BOP", "CET", "CHR", "EPB", "EPN", "EPR",
                  "ERR", "ERS", "FRA", "FRN", "HEG", "MEL", "ORA", "OSV",
                  "PEB", "PEG", "PET", "PIB", "PIG", "PIR", "PRU", "SAB",
                  "THO", "TIL")

#'
#' Calculate the volume of a list of trees
#'
#'
#' @param speciesCode either the Latin name or a three-character
#' species code (e.g. "SAB" or "EPB")
#' @param dbhCm diameter at breast height (cm)
#' @param heightM tree height (m)
#' @param overbark true to get the overbark volume (set to FALSE by default)
#'
#' @return the underbark merchantable volume (dm3)
#'
#' @export
getMerchantableVolumeDm3 <- function(speciesCode, dbhCm, heightM, overbark = F) {
  .connectToCFT()
  if (!class(dbhCm) %in% c("numeric", "integer")) {
    stop("The dbhCm argument should be a numeric or an integer!")
  }
  if (!class(heightM) %in% c("numeric", "integer")) {
    stop("The heightM argument should be a numeric or an integer!")
  }
  if (!class(overbark) %in% c("logical")) {
    stop("The overbark argument should be a logical!")
  }
  if (!class(speciesCode) %in% c("character")) {
    stop("The speciesCode argument should be characters!")
  }

  # areRecognized <- speciesCode %in% speciesCodes
  # if (any(!areRecognized)) {
  #   indices <- which(!areRecognized)
  #   notRecognized <- unique(speciesCode[indices])
  #   if (length(notRecognized) == 1) {
  #     stop(paste("This species code:", notRecognized, "is not recognized! Please use the getUnderbarkMerchantableVolumeSpeciesList function to get the list of possible codes."))
  #   } else {
  #     stop(paste("These species codes:", paste(notRecognized, collapse = ","), "are not recognized! Please use the getUnderbarkMerchantableVolumeSpeciesList function to get the list of possible codes."))
  #   }
  # }
  # uselessStand <- J4R::createJavaObject("quebecmrnfutility.predictor.volumemodels.merchantablevolume.VolumableStandImpl")
  predictor <- J4R::createJavaObject("quebecmrnfutility.predictor.volumemodels.merchantablevolume.MerchantableVolumePredictor")
  volume <- predictor$predictDeterministicTreeCommercialVolumeDm3(speciesCode, as.numeric(dbhCm), as.numeric(heightM), overbark)
  return(volume)
}


#'
#' Calculate the underbark volume of a list of trees
#'
#'
#' @param speciesCode a three-character species code (e.g. "SAB" or "EPB")
#' @param dbhCm diameter at breast height (cm)
#' @param heightM tree height (m)
#'
#' @return the underbark merchantable volume (dm3)
#'
#' @export
getUnderbarkMerchantableVolumeDm3 <- function(speciesCode, dbhCm, heightM) {
  return(getMerchantableVolumeDm3(speciesCode, dbhCm, heightM))
}


#'
#' Provide the list of species codes
#' @param latinName a logical TRUE to get the eligible Latin names or
#' FALSE to get the eligible species codes.
#' Provide all of the possible species code that can be used with the
#' getUnderbarkMerchantableVolumeDm3 function.
#'
#' @export
getMerchantableVolumeSpeciesList <- function(latinName = F) {
  .connectToCFT()
  if (latinName) {
    array <- J4R::callJavaMethod("quebecmrnfutility.predictor.volumemodels.merchantablevolume.MerchantableVolumePredictor", "getEligibleSpecies")
    return(array$get(as.integer(seq(0,array$size()-1,by=1))))
  } else {
    return(speciesCodes)
  }
}

#'
#' Provide the list of species codes
#' @param latinName a logical TRUE to get the eligible Latin names or
#' FALSE to get the eligible species codes.
#' Provide all of the possible species code that can be used with the
#' getUnderbarkMerchantableVolumeDm3 function.
#'
#' @export
getUnderbarkMerchantableVolumeSpeciesList <- function(latinName = F) {
  .Deprecated("getMerchantableVolumeSpeciesList")
  return(getMerchantableVolumeSpeciesList(latinName))
}
