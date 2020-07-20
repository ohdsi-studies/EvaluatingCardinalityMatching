
computePreferenceScore <- function(data, unfilteredData = NULL) {
  if (is.null(unfilteredData)) {
    proportion <- sum(data$treatment)/nrow(data)
  } else {
    proportion <- sum(unfilteredData$treatment)/nrow(unfilteredData)
  }
  propensityScore <- data$propensityScore
  propensityScore[propensityScore > 0.9999999] <- 0.9999999
  x <- exp(log(propensityScore/(1 - propensityScore)) - log(proportion/(1 - proportion)))
  data$preferenceScore <- x/(x + 1)
  return(data)
}


#' Create propensity scores
#'
#' @description
#' \code{createPs} creates propensity scores using a regularized logistic regression.
#'
#' @param cohortMethodData         An object of type \code{cohortMethodData} as generated using
#'                                 \code{getDbCohortMethodData}.
#' @param population               A data frame describing the population. This should at least have a
#'                                 'rowId' column corresponding to the rowId column in the
#'                                 \code{cohortMethodData} covariates object and a 'treatment' column.
#'                                 If population is not specified, the full population in the
#'                                 \code{cohortMethodData} will be used.
#' @param excludeCovariateIds      Exclude these covariates from the propensity model.
#' @param includeCovariateIds      Include only these covariates in the propensity model.
#' @param maxCohortSizeForFitting  If the target or comparator cohort are larger than this number, they
#'                                 will be downsampled before fitting the propensity model. The model
#'                                 will be used to compute propensity scores for all subjects. The
#'                                 purpose of the sampling is to gain speed. Setting this number to 0
#'                                 means no downsampling will be applied.
#' @param errorOnHighCorrelation   If true, the function will test each covariate for correlation with
#'                                 the treatment assignment. If any covariate has an unusually high
#'                                 correlation (either positive or negative), this will throw and
#'                                 error.
#' @param stopOnError              If an error occurrs, should the function stop? Else, the two cohorts
#'                                 will be assumed to be perfectly separable.
#' @param prior                    The prior used to fit the model. See
#'                                 \code{\link[Cyclops]{createPrior}} for details.
#' @param control                  The control object used to control the cross-validation used to
#'                                 determine the hyperparameters of the prior (if applicable). See
#'                                 \code{\link[Cyclops]{createControl}} for details.
#'
#' @details
#' \code{createPs} creates propensity scores using a regularized logistic regression.
#'
#' @examples
#' data(cohortMethodDataSimulationProfile)
#' cohortMethodData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = 1000)
#' ps <- createPs(cohortMethodData)
#'
#' @export
createPs2 <- function(cohortMethodData,
                     population,
                     excludeCovariateIds = c(),
                     includeCovariateIds = c(),
                     maxCohortSizeForFitting = 250000,
                     errorOnHighCorrelation = TRUE,
                     stopOnError = TRUE,
                     prior = createPrior("laplace", exclude = c(0), useCrossValidation = TRUE),
                     control = createControl(noiseLevel = "silent",
                                             cvType = "auto",
                                             seed = 1,
                                             tolerance = 2e-07,
                                             cvRepetitions = 10,
                                             startingVariance = 0.01)) {
  if (missing(population))
    population <- cohortMethodData$cohorts
  if (!("rowId" %in% colnames(population)))
    stop("Missing column rowId in population")
  if (!("treatment" %in% colnames(population)))
    stop("Missing column treatment in population")
  
  start <- Sys.time()
  population <- population[order(population$rowId), ]
  if (is.data.frame(cohortMethodData$covariates)) {
    error <- "No covariate data, so cannot fit model"
    sampled <- FALSE
    ref <- NULL
  } else {
    covariates <- FeatureExtraction::filterByRowId(cohortMethodData$covariates, population$rowId)
    if (length(includeCovariateIds) != 0) {
      idx <- !is.na(ffbase::ffmatch(covariates$covariateId, ff::as.ff(includeCovariateIds)))
      covariates <- covariates[ffbase::ffwhich(idx, idx == TRUE), ]
    }
    if (length(excludeCovariateIds) != 0) {
      idx <- is.na(ffbase::ffmatch(covariates$covariateId, ff::as.ff(excludeCovariateIds)))
      covariates <- covariates[ffbase::ffwhich(idx, idx == TRUE), ]
    }
    covariateData <- FeatureExtraction::tidyCovariateData(covariates = covariates,
                                                          covariateRef = cohortMethodData$covariateRef,
                                                          populationSize = nrow(population),
                                                          minFraction = 0.02,
                                                          normalize = TRUE,
                                                          removeRedundancy = TRUE)
    covariates <- covariateData$covariates
    attr(population, "metaData")$deletedInfrequentCovariateIds <- covariateData$metaData$deletedInfrequentCovariateIds
    attr(population, "metaData")$deletedRedundantCovariateIds <- covariateData$metaData$deletedRedundantCovariateIds
    sampled <- FALSE
    if (maxCohortSizeForFitting != 0) {
      set.seed(0)
      targetRowIds <- population$rowId[population$treatment == 1]
      if (length(targetRowIds) > maxCohortSizeForFitting) {
        ParallelLogger::logInfo(paste0("Downsampling target cohort from ", length(targetRowIds), " to ", maxCohortSizeForFitting, " before fitting"))
        targetRowIds <- sample(targetRowIds, size = maxCohortSizeForFitting, replace = FALSE)
        sampled <- TRUE
      }
      comparatorRowIds <- population$rowId[population$treatment == 0]
      if (length(comparatorRowIds) > maxCohortSizeForFitting) {
        ParallelLogger::logInfo(paste0("Downsampling comparator cohort from ", length(comparatorRowIds), " to ", maxCohortSizeForFitting, " before fitting"))
        comparatorRowIds <- sample(comparatorRowIds, size = maxCohortSizeForFitting, replace = FALSE)
        sampled <- TRUE
      }
      if (sampled) {
        fullPopulation <- population
        fullCovariates <- covariates
        population <- population[population$rowId %in% c(targetRowIds, comparatorRowIds), ]
        covariates <- FeatureExtraction::filterByRowId(covariates, population$rowId)
      }
    }
    if (!Cyclops::isSorted(population, "rowId")) {
      population <- population[order(population$rowId), ]
    }
    outcomes <- ff::as.ffdf(population)
    colnames(outcomes)[colnames(outcomes) == "treatment"] <- "y"
    floatingPoint <- getOption("floatingPoint")
    if (is.null(floatingPoint)) {
      floatingPoint <- 64
    } else {
      ParallelLogger::logInfo("Cyclops using precision of ", floatingPoint)
    }
    cyclopsData <- convertToCyclopsData(outcomes, covariates, modelType = "lr", quiet = TRUE, floatingPoint = floatingPoint)
    ff::close.ffdf(outcomes)
    ff::close.ffdf(covariates)
    rm(outcomes)
    rm(covariates)
    rm(covariateData)
    error <- NULL
    ref <- NULL
    if (errorOnHighCorrelation) {
      suspect <- Cyclops::getUnivariableCorrelation(cyclopsData, threshold = 0.5)
      suspect <- suspect[!is.na(suspect)]
      if (length(suspect) != 0) {
        covariateIds <- as.numeric(names(suspect))
        idx <- !is.na(ffbase::ffmatch(cohortMethodData$covariateRef$covariateId,
                                      ff::as.ff(covariateIds)))
        ref <- ff::as.ram(cohortMethodData$covariateRef[ffbase::ffwhich(idx, idx == TRUE), ])
        ParallelLogger::logInfo("High correlation between covariate(s) and treatment detected:")
        ParallelLogger::logInfo(paste(colnames(ref), collapse = "\t"))
        ref$covariateName <- as.character(ref$covariateName)
        for (i in 1:nrow(ref))
          ParallelLogger::logInfo(paste(ref[i, ], collapse = "\t"))
        message <- "High correlation between covariate(s) and treatment detected. Perhaps you forgot to exclude part of the exposure definition from the covariates?"
        if (stopOnError) {
          stop(message)
        } else {
          error <- message
        }
      }
    }
  }
  if (is.null(error)) {
    cyclopsFit <- tryCatch({
      Cyclops::fitCyclopsModel(cyclopsData, prior = prior, control = control)
    }, error = function(e) {
      e$message
    })
    if (is.character(cyclopsFit)) {
      if (stopOnError) {
        stop(cyclopsFit)
      } else {
        error <- cyclopsFit
      }
    } else if (cyclopsFit$return_flag != "SUCCESS") {
      if (stopOnError) {
        stop(cyclopsFit$return_flag)
      } else {
        error <- cyclopsFit$return_flag
      }
    }
  }
  if (is.null(error)) {
    error <- "OK"
    cfs <- coef(cyclopsFit)
    if (all(cfs[2:length(cfs)] == 0)) {
      warning("All coefficients (except maybe the intercept) are zero. Either the covariates are completely uninformative or completely predictive of the treatment. Did you remember to exclude the treatment variables from the covariates?")
    }
    if (sampled) {
      # Adjust intercept to non-sampled population:
      y.bar <- mean(population$treatment)
      y.odds <- y.bar/(1 - y.bar)
      y.bar.new <- mean(fullPopulation$treatment)
      y.odds.new <- y.bar.new/(1 - y.bar.new)
      delta <- log(y.odds) - log(y.odds.new)
      cfs[1] <- cfs[1] - delta  # Equation (7) in King and Zeng (2001)
      cyclopsFit$estimation$estimate[1] <- cfs[1]
      fullOutcomes <- ff::as.ffdf(fullPopulation)
      population <- fullPopulation
      population$propensityScore <- predict(cyclopsFit, newOutcomes = fullOutcomes, newCovariates = fullCovariates)
      ff::close.ffdf(fullOutcomes)
      ff::close.ffdf(fullCovariates)
      rm(fullOutcomes)
      rm(fullCovariates)
    } else {
      population$propensityScore <- predict(cyclopsFit)
    }
    attr(population, "metaData")$psModelCoef <- coef(cyclopsFit)
    attr(population, "metaData")$psModelPriorVariance <- cyclopsFit$variance[1]
  } else {
    if (sampled) {
      population <- fullPopulation
      ff::close.ffdf(fullCovariates)
      rm(fullCovariates)
    }
    population$propensityScore <- population$treatment
    attr(population, "metaData")$psError <- error
    if (!is.null(ref)) {
      attr(population, "metaData")$psHighCorrelation <- ref
    }
  }
  population <- computePreferenceScore(population)
  delta <- Sys.time() - start
  ParallelLogger::logDebug("Propensity model fitting finished with status ", error)
  ParallelLogger::logInfo(paste("Creating propensity scores took", signif(delta, 3), attr(delta, "units")))
  return(population)
}

