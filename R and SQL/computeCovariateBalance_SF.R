


quickSum <- function(data, squared = FALSE, cm = FALSE) {
  if(cm == TRUE){
    if (squared) {
      x <- bySumFf((data$covariateValue)^2, data$covariateId)
      colnames(x) <- c("covariateId", "sumSqr")
    } else {
      x <- bySumFf((data$covariateValue  != 0 | data$covariateValue  == 0), data$covariateId)
      colnames(x) <- c("covariateId", "sum")
    }
  } else{
    if (squared) {
      x <- bySumFf(data$covariateValue^2, data$covariateId)
      colnames(x) <- c("covariateId", "sumSqr")
    } else {
      x <- bySumFf(data$covariateValue, data$covariateId)
      colnames(x) <- c("covariateId", "sum")
    }
  }
  x$covariateId <- as.numeric(x$covariateId)
  return(x)
}


computeMeanAndSd <- function(cohorts, covariates, treatment, cm = FALSE) {
  if (!is.null(cohorts$stratumId)) {
    # Rownames needs to be null or else next command will crash
    rownames(cohorts) <- NULL
    t <- cohorts[cohorts$treatment == treatment, ]
    t <- aggregate(rowId ~ stratumId, t, length)
  }
  if (!is.null(cohorts$stratumId) && max(t$rowId) > 1) {
    # Variable strata sizes detected: weigh by size of strata set
    w <- t
    w$weight <- 1/w$rowId
    w$rowId <- NULL
    w <- merge(w, cohorts[cohorts$treatment == treatment, ])
    w <- w[, c("rowId", "weight")]
    w$weight <- w$weight/sum(w$weight)  # Normalize so sum(w) == 1
    covariatesSubset <- covariates[ffbase::`%in%`(covariates$rowId,
                                                  cohorts$rowId[cohorts$treatment == treatment]), ]
    covariatesSubset <- ffbase::merge.ffdf(covariatesSubset, ff::as.ffdf(w))
    covariatesSubset$wValue <- covariatesSubset$weight * covariatesSubset$covariateValue
    covariatesSubset$wValueSquared <- covariatesSubset$wValue * covariatesSubset$covariateValue
    
    # Compute sum
    result <- bySumFf(covariatesSubset$covariateValue, covariatesSubset$covariateId)
    colnames(result)[colnames(result) == "bins"] <- "covariateId"
    colnames(result)[colnames(result) == "sums"] <- "sum"
    
    # Compute weighted mean (no need to divide by sum(w) because it is 1)
    wMean <- bySumFf(covariatesSubset$wValue, covariatesSubset$covariateId)
    colnames(wMean)[colnames(wMean) == "bins"] <- "covariateId"
    colnames(wMean)[colnames(wMean) == "sums"] <- "mean"
    result <- merge(result, wMean)
    
    # Compute weighted standard deviation
    wValueSquared <- bySumFf(covariatesSubset$wValueSquared, covariatesSubset$covariateId)
    colnames(wValueSquared)[colnames(wValueSquared) == "bins"] <- "covariateId"
    colnames(wValueSquared)[colnames(wValueSquared) == "sums"] <- "wValueSquared"
    result <- merge(result, wMean)
    sumW <- 1
    sumWSquared <- sum(w$weight^2)
    result <- merge(result, wValueSquared)
    result$variance <- (result$wValueSquared - result$mean^2) * sumW/(sumW^2 - sumWSquared)
    result$sd <- sqrt(result$variance)
  } else if("weight" %in% colnames(cohorts)) {
    w <- cohorts[cohorts$treatment == treatment, c("rowId", "weight")]
    covariatesSubset <- covariates[ffbase::`%in%`(covariates$rowId,
                                                  cohorts$rowId[cohorts$treatment == treatment]), ]
    covariatesSubset <- ffbase::merge.ffdf(covariatesSubset, ff::as.ffdf(w))
    covariatesSubset$wValue <- covariatesSubset$weight * covariatesSubset$covariateValue
    covariatesSubset$wValueSquared <- covariatesSubset$wValue * covariatesSubset$covariateValue
    
    # Compute sum
    result <- bySumFf(covariatesSubset$covariateValue, covariatesSubset$covariateId)
    colnames(result)[colnames(result) == "bins"] <- "covariateId"
    colnames(result)[colnames(result) == "sums"] <- "sum"
    
    # Compute weighted mean (no need to divide by sum(w) because it is 1)
    wMean <- bySumFf(covariatesSubset$wValue, covariatesSubset$covariateId)
    colnames(wMean)[colnames(wMean) == "bins"] <- "covariateId"
    colnames(wMean)[colnames(wMean) == "sums"] <- "mean"
    result <- merge(result, wMean)
    
    # Compute weighted standard deviation
    wValueSquared <- bySumFf(covariatesSubset$wValueSquared, covariatesSubset$covariateId)
    colnames(wValueSquared)[colnames(wValueSquared) == "bins"] <- "covariateId"
    colnames(wValueSquared)[colnames(wValueSquared) == "sums"] <- "wValueSquared"
    result <- merge(result, wMean)
    sumW <- 1
    sumWSquared <- sum(w$weight^2)
    result <- merge(result, wValueSquared)
    result$variance <- (result$wValueSquared - result$mean^2) * sumW/(sumW^2 - sumWSquared)
    result$sd <- sqrt(result$variance)
  }else {
    # Used uniform strata size, no need to weight
    personCount <- ffbase::sum.ff(cohorts$treatment == treatment)
    covariatesSubset <- covariates[ffbase::`%in%`(covariates$rowId,
                                                  cohorts$rowId[cohorts$treatment == treatment]), ]
    result <- quickSum(covariatesSubset, cm = cm)
    resultSqr <- quickSum(covariatesSubset, squared = TRUE, cm = cm)
    result <- merge(result, resultSqr)
    result$sd <- sqrt((result$sumSqr - (result$sum^2/personCount))/personCount)*(sqrt(personCount/(personCount-1)))
    result$mean <- result$sum/personCount
  }
  return(result)
}

computeMeansPerGroup <- function(cohorts, covariates, cm = FALSE) {
  # nOverall <- nrow(cohorts)
  # nTarget <- ffbase::sum.ff(cohorts$treatment == 1)
  # nComparator <- nOverall - nTarget
  
  target <- computeMeanAndSd(cohorts, covariates, treatment = 1, cm = cm)
  colnames(target)[colnames(target) == "sum"] <- "sumTarget"
  colnames(target)[colnames(target) == "mean"] <- "meanTarget"
  colnames(target)[colnames(target) == "sd"] <- "sdTarget"
  
  comparator <- computeMeanAndSd(cohorts, covariates, treatment = 0, cm = cm)
  colnames(comparator)[colnames(comparator) == "sum"] <- "sumComparator"
  colnames(comparator)[colnames(comparator) == "mean"] <- "meanComparator"
  colnames(comparator)[colnames(comparator) == "sd"] <- "sdComparator"
  
  result <- merge(target[,
                         c("covariateId", "meanTarget", "sumTarget", "sdTarget")],
                  comparator[,
                             c("covariateId", "meanComparator", "sumComparator", "sdComparator")],
                  all = TRUE)
  result$sd <- sqrt((result$sdTarget^2 + result$sdComparator^2)/2)
  result <- result[, c("covariateId",
                       "meanTarget",
                       "meanComparator",
                       "sumTarget",
                       "sumComparator",
                       "sd")]
  return(result)
}



computeCovariateBalance_SF <- function(population, cohortMethodData, rowIds = NULL, 
                                       subgroupCovariateId = NULL, rowIds2 = NULL, cm = FALSE) {
  ParallelLogger::logTrace("Computing covariate balance")
  start <- Sys.time()
  if(!is.null(rowIds2)){
    cohorts <- ff::as.ffdf(cohortMethodData$cohorts[cohortMethodData$cohorts$rowId %in% rowIds2, c("rowId", "treatment")])
  } else{
    cohorts <- ff::as.ffdf(cohortMethodData$cohorts[, c("rowId", "treatment")])
  }
  if(is.null(rowIds)){
    if("weight" %in% colnames(population)){
      cohortsAfterMatching <- ff::as.ffdf(population[, c("rowId", "treatment", "weight")])
    } else{
      cohortsAfterMatching <- ff::as.ffdf(population[, c("rowId", "treatment", "stratumId")])
    }
  }else{
    cohortsAfterMatching <- ff::as.ffdf(population[, c("rowId", "treatment")])
  }
  covariates <- cohortMethodData$covariates
  
  if (!is.null(subgroupCovariateId)) {
    idx <- covariates$covariateId == subgroupCovariateId
    if (!ffbase::any.ff(idx)) {
      stop("Cannot find covariate with ID ", subgroupCovariateId)
    }
    subGroupRowIds <- covariates$rowId[idx]
    row.names(cohorts) <- NULL
    idx <- ffbase::`%in%`(cohorts$rowId, subGroupRowIds)
    if (!ffbase::any.ff(idx)) {
      stop("Cannot find covariate with ID ", subgroupCovariateId, " in population before matching/trimming")
    }
    cohorts <- cohorts[idx, ]
    sumTreatment <- sum(cohorts$treatment)
    if (sumTreatment == 0 || sumTreatment == nrow(cohorts)) {
      stop("Subgroup population before matching/trimming doesn't have both target and comparator")
    }
    row.names(cohortsAfterMatching) <- NULL
    idx <- ffbase::`%in%`(cohortsAfterMatching$rowId, subGroupRowIds)
    if (!ffbase::any.ff(idx)) {
      stop("Cannot find covariate with ID ", subgroupCovariateId, " in population after matching/trimming")
    }
    cohortsAfterMatching <- cohortsAfterMatching[idx, ]
    sumTreatment <- sum(cohortsAfterMatching$treatment)
    if (sumTreatment == 0 || sumTreatment == nrow(cohortsAfterMatching)) {
      stop("Subgroup population before matching/trimming doesn't have both target and comparator")
    }
  }
  beforeMatching <- computeMeansPerGroup(cohorts, covariates, cm = cm)
  afterMatching <- computeMeansPerGroup(cohortsAfterMatching, covariates, cm = cm)
  
  ff::close.ffdf(cohorts)
  ff::close.ffdf(cohortsAfterMatching)
  rm(cohorts)
  rm(cohortsAfterMatching)
  
  colnames(beforeMatching)[colnames(beforeMatching) == "meanTarget"] <- "beforeMatchingMeanTarget"
  colnames(beforeMatching)[colnames(beforeMatching) == "meanComparator"] <- "beforeMatchingMeanComparator"
  colnames(beforeMatching)[colnames(beforeMatching) == "sumTarget"] <- "beforeMatchingSumTarget"
  colnames(beforeMatching)[colnames(beforeMatching) == "sumComparator"] <- "beforeMatchingSumComparator"
  colnames(beforeMatching)[colnames(beforeMatching) == "sd"] <- "beforeMatchingSd"
  colnames(afterMatching)[colnames(afterMatching) == "meanTarget"] <- "afterMatchingMeanTarget"
  colnames(afterMatching)[colnames(afterMatching) == "meanComparator"] <- "afterMatchingMeanComparator"
  colnames(afterMatching)[colnames(afterMatching) == "sumTarget"] <- "afterMatchingSumTarget"
  colnames(afterMatching)[colnames(afterMatching) == "sumComparator"] <- "afterMatchingSumComparator"
  colnames(afterMatching)[colnames(afterMatching) == "sd"] <- "afterMatchingSd"
  balance <- merge(beforeMatching, afterMatching)
  balance <- merge(balance, ff::as.ram(cohortMethodData$covariateRef))
  balance$covariateName <- as.character(balance$covariateName)
  balance$beforeMatchingStdDiff <- (balance$beforeMatchingMeanTarget - balance$beforeMatchingMeanComparator)/balance$beforeMatchingSd
  balance$afterMatchingStdDiff <- (balance$afterMatchingMeanTarget - balance$afterMatchingMeanComparator)/balance$beforeMatchingSd
  balance$beforeMatchingStdDiff[balance$beforeMatchingSd == 0] <- 0
  balance$afterMatchingStdDiff[balance$beforeMatchingSd == 0] <- 0
  balance <- balance[order(-abs(balance$beforeMatchingStdDiff)), ]
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Computing covariate balance took", signif(delta, 3), attr(delta, "units")))
  return(balance)
}





