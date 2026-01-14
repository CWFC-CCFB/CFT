########################################################
# Basic R function for the package.
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: April 2019
########################################################

jarFilenames <- c("repicea-1.17.2.jar", "repicea-mathstats-1.8.1.jar", "repicea-simulation-1.3.17.jar", "cfsforesttools-1.11.0.jar")

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to CFT!")
  packageStartupMessage("The CFT package implements some biometric models applied to Canadian forests!")
  packageStartupMessage("Please, make sure that Java (version 8 or later) is installed on your computer.")
  packageStartupMessage("For more information, visit https://sourceforge.net/p/mrnfforesttools/wiki/CFT/ .")
}


.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

.onUnload <- function(libpath) {
  shutdownJava()
}

.onDetach <- function(libpath) {
  shutdownJava()
}


#'
#' Extends the shutdownJava function of the J4R package
#'
#' @export
shutdownJava <- function() {
  if (J4R::isConnectedToJava()) {
    J4R::shutdownClient()
  }
}

.connectToCFT <- function(memSize = NULL) {
  if (J4R::isConnectedToJava()) {
    for (jarName in jarFilenames) {
      if (!J4R::checkIfClasspathContains(jarName)) {
        stop(paste("It seems J4R is running but the class path does not contain this library: ", jarName, ". Shut down J4R using the shutdownClient function first and then re-run your code."))
      }
    }
  } else {
    rootPath <- system.file(package = "CFT")
    J4R::connectToJava(extensionPath = paste(rootPath, "*", sep="/"))
#    loggerName <- J4R::getJavaField("repicea.stats.estimators.MaximumLikelihoodEstimator", "LOGGER_NAME")
#    logger <- J4R::callJavaMethod("repicea.util.REpiceaLogManager", "getLogger", loggerName)
#    level <- J4R::getJavaField("java.util.logging.Level", "WARNING")
#    logger$setLevel(level)
  }
}




.addToArray <- function(refArray, array) {
  if (length(refArray) != length(array)) {
    stop("Incompatible array length!")
  } else {
    for (i in 1:length(array)) {
      refArray[[i]] <- c(refArray[[i]], array[[i]])
    }
  }
  return(refArray)
}

.convertJavaDataSetIntoDataFrame <- function(dataSetObject) {
  refArray <- NULL
  observations <- J4R::callJavaMethod(dataSetObject, "getObservations")
  observations <- J4R::getAllValuesFromListObject(observations)
  for (obs in observations) {
    array <- J4R::callJavaMethod(obs, "toArray")
    array <- as.list(J4R::getAllValuesFromArray(array))
    if (is.null(refArray)) {
      refArray <- array
    } else {
      refArray <- .addToArray(refArray, array)
    }
  }
  dataFrame <- NULL
  for (i in 1:length(refArray)) {
    dataFrame <- as.data.frame(cbind(dataFrame, refArray[[i]]))
  }
  colnames(dataFrame) <- J4R::getAllValuesFromListObject(J4R::callJavaMethod(dataSetObject, "getFieldNames"))
  return(dataFrame)
}



.getSimpleJavaVersion <- function() {
  version <- suppressMessages(J4R::getJavaVersion()$version)
  dotIndices <- gregexpr("\\.", version)
  firstDot <- dotIndices[[1]][1]
  firstInt <- as.integer(substr(version, 1, firstDot-1))
  if (firstInt == 1) {
    secondDot <- dotIndices[[1]][2]
    secondInt <- as.integer(substr(version, firstDot + 1, secondDot - 1))
    return(secondInt)
  } else {
    return(firstInt)
  }
}

