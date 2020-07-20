
# ==================================================================================================================
# Create connection
# ==================================================================================================================

# Sets Java Home
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")

# Package availability check and installation
if (!require("pacman")) install.packages("pacman")
if (!require('gurobi')){install.packages("/gurobi_9.0-1.zip", repos = NULL)}
pacman::p_load(devtools, drat, CohortMethod, SqlRender, DatabaseConnector, CohortDiagnostics, grid, reshape2, dplyr,
               designmatch, caret, rdist, gurobi, EmpiricalCalibration, caTools, openxlsx)

# Set connection details
drat::addRepo("OHDSI")
connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             user = "joe",
                                             password = "secret",
                                             server = "myserver")
connection <- connect(connectionDetails)

# Set timezone
Sys.setenv(TZ='GMT')


# ==================================================================================================================
# Define parameters
# ==================================================================================================================

# Set working directory
wd <- "C:/myworkingdirectory"
setwd(wd)

# The name of the database schema where the CDM data can be found:
cdmDatabaseSchema <- "CDM_IBM_CCAE_v1103.dbo"

# The cdmVersion of the database
cdmVersion <- "1103"

# The name of the database schema and tables where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <- "Scratch.dbo"
cohortOutcomeTable <- "Outcome_Table"

# For Oracle: define a schema that can be used to emulate temp tables:
oracleTempSchema <- NULL

# Specify the location of stored study population data (see step 5):
cmDataFolder <- "C:/cmDataFolder"

# For each analysis, specify the percent of the study population to sample and the number of cohorts to include in analyses
splitRatio <- 1.00
num_cohorts <- 1

# Define cohort ids
cohort_id_1 <- 14705      # Comparator 1
cohort_id_2 <- 14706      # Comparator 2
cohort_id_3 <- 14707      # Outcome

# Define negative control concept ids
ncs <- c(434165,436409,199192,4088290,4092879,44783954,75911,137951,77965,
         376707,4103640,73241,133655,73560,434327,4213540,140842,81378,
         432303,4201390,46269889,134438,78619,201606,76786,4115402,
         45757370,433111,433527,4170770,4092896,259995,40481632,4166231,
         433577,4231770,440329,4012570,4012934,441788,4201717,374375,
         4344500,139099,444132,196168,432593,434203,438329,195873,4083487,
         4103703,4209423,377572,40480893,136368,140648,438130,4091513,
         4202045,373478,46286594,439790,81634,380706,141932,36713918,
         443172,81151,72748,378427,437264,194083,140641,440193,4115367,
         36713926,79864,36717682,438120,440704,441277,436373,4002818,
         73754,136773,376382,439776,4248728,440021,434626,4241530,
         36717115,437969,4119307,195590,42873170,80502,
         436785,74464,72404,439935,437092,442306,378424)


# ==================================================================================================================
# Create outcome tables
# ==================================================================================================================

if(!file.exists(paste(wd, "/cohort_", 1, "/cmData/outcomes.rds", sep = ""))){
  print("Creating outcome table")
  
  # Create outcome cohort table
  createCohortTable(connectionDetails = connectionDetails,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable = cohortOutcomeTable)
  
  # Populate cohort table
  sql <- readSql("create_cohorts_outcome_100.sql")
  sql <- render(sql,
                cdmDatabaseSchema = strsplit(cdmDatabaseSchema, "[.]")[[1]][1],
                cohortDatabaseSchema = cohortDatabaseSchema,
                cohortOutcomeTable = cohortOutcomeTable,
                cohort_id_3 = cohort_id_3)
  sql <- translate(sql, targetDialect = connectionDetails$dbms)
  executeSql(connection, sql)
  
  # Populate negative controls
  sql <- readSql("create_negative_controls.sql")
  sql <- render(sql,
                cdmDatabaseSchema = strsplit(cdmDatabaseSchema, "[.]")[[1]][1],
                cohortDatabaseSchema = cohortDatabaseSchema,
                cohortOutcomeTable = cohortOutcomeTable,
                ncs = ncs)
  sql <- translate(sql, targetDialect = connectionDetails$dbms)
  executeSql(connection, sql)
  
  rm(sql)
} else{
  print("Outcome table already created")
}

# # Test cohorts appropriately generated
# sql <- paste("SELECT cohort_definition_id, COUNT(*) AS count",
#              "FROM @cohortDatabaseSchema.@cohortOutcomeTable",
#              "GROUP BY cohort_definition_id")
# sql <- render(sql,
#               cohortDatabaseSchema = cohortDatabaseSchema,
#               cohortOutcomeTable = cohortOutcomeTable)
# sql <- translate(sql, targetDialect = connectionDetails$dbms)
# querySql(connection, sql)

rm(cohort_id_3)


# ==================================================================================================================
# Create cohort tables
# ==================================================================================================================

# Define study population
cmData <- loadCohortMethodData(cmDataFolder)
studyPop <- createStudyPopulation(cohortMethodData = cmData,
                                  outcomeId = 3,
                                  firstExposureOnly = FALSE,
                                  restrictToCommonPeriod = FALSE,
                                  washoutPeriod = 365,
                                  removeDuplicateSubjects = "remove all",
                                  removeSubjectsWithPriorOutcome = TRUE,
                                  minDaysAtRisk = 1,
                                  riskWindowStart =1,
                                  startAnchor = "cohort start",
                                  riskWindowEnd = 0,
                                  endAnchor = "cohort end")
studyPop_t <- studyPop[studyPop$treatment == 1, ]
studyPop_c <- studyPop[studyPop$treatment == 0, ]
rm(cmData, studyPop)

# Target and comparator ingredient concepts
aceI <- c(1335471,1340128,1341927,1363749,1308216,1310756,1373225, 1331235,1334456,1342439)
thz <- c(1395058,974166,978555,907013)

# Define which types of covariates must be constructed
cs <- createDefaultCovariateSettings(excludedCovariateConceptIds = c(aceI, thz),
                                     addDescendantsToExclude = TRUE)
rm(aceI, thz)

# Create cohorts on SQL server, and local cmData objects
for(i in 1:num_cohorts){
  
  # Check whether already created
  if(file.exists(paste(wd, "/cohort_", i, "/cmData/outcomes.rds", sep = ""))){
    print(paste("Cohort", i, "exists - moving to next cohort"))
    next
  } else{
    print(paste("Creating cohort", i))
  }
 
  set.seed(i)
  
  # Create split
  if(splitRatio == 1.00){
    t <- studyPop_t
    c <- studyPop_c
  }else{
    split_t <- sample.split(studyPop_t$rowId, SplitRatio = splitRatio)
    split_c <- sample.split(studyPop_c$rowId, SplitRatio = splitRatio)
    t <- subset(studyPop_t, split_t == TRUE)
    c <- subset(studyPop_c, split_c == TRUE)
  }
  subjectIds <- c(t$subjectId, c$subjectId)
  
  # Create cohort table
  createCohortTable(connectionDetails = connectionDetails,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable = paste("SF_Methods_1_", i, sep=""))
  
  for(k in 1:ceiling(length(subjectIds)/30000)){
    if(k == ceiling(length(subjectIds)/30000)){
      range_x <- c(((k-1)*30000+1):(length(subjectIds)))
    }else{
      range_x <- c(((k-1)*30000+1):(k*30000))
    }
    
    print(k)

    # Populate cohort table
    sql <- readSql("create_cohorts_100.sql")
    sql <- render(sql,
                  cdmDatabaseSchema = strsplit(cdmDatabaseSchema, "[.]")[[1]][1],
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTable = paste("SF_Methods_1_", i, sep=""),
                  cohort_id_1 = cohort_id_1,
                  cohort_id_2 = cohort_id_2,
                  subjectIds = subjectIds[range_x])
    sql <- translate(sql, targetDialect = connectionDetails$dbms)
    executeSql(connection, sql)
    
    rm(range_x)
  }

  # # Test cohorts appropriately generated
  # sql <- paste("SELECT cohort_definition_id, COUNT(*) AS count",
  #              "FROM @cohortDatabaseSchema.@cohortTable",
  #              "GROUP BY cohort_definition_id")
  # sql <- render(sql,
  #               cohortDatabaseSchema = cohortDatabaseSchema,
  #               cohortTable = paste("SF_Methods_1_", i, sep=""))
  # sql <- translate(sql, targetDialect = connectionDetails$dbms)
  # querySql(connection, sql)
  
  # Create cmData
  cmData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                  oracleTempSchema = oracleTempSchema,
                                  targetId = 1,
                                  comparatorId = 2,
                                  outcomeIds = 3,
                                  studyStartDate = "",
                                  studyEndDate = "",
                                  exposureDatabaseSchema = cohortDatabaseSchema,
                                  exposureTable = paste("SF_Methods_1_", i, sep=""),
                                  outcomeDatabaseSchema = cohortDatabaseSchema,
                                  outcomeTable = cohortOutcomeTable,
                                  cdmVersion = cdmVersion,
                                  firstExposureOnly = FALSE,
                                  removeDuplicateSubjects = FALSE,
                                  restrictToCommonPeriod = FALSE,
                                  washoutPeriod = 365,
                                  covariateSettings = cs)
  
  # Save cmData
  if(!dir.exists(paste(wd, "/cohort_", i, sep = ""))){
    dir.create(paste(wd, "/cohort_", i, sep = ""))
  }
  saveCohortMethodData(cmData, paste(wd, "/cohort_", i, "/cmData", sep = ""))
  
  rm(split_t, split_c, t, c, subjectIds, sql, cmData)
}
rm(studyPop_t, cohort_id_1, cohort_id_2, studyPop_c, cs, splitRatio)


# ==================================================================================================================
# Run Analyses for PSM
# ==================================================================================================================

# Specifying hypotheses of interest
tcos <- createTargetComparatorOutcomes(targetId = 1,
                                       comparatorId = 2,
                                       outcomeIds = c(3, ncs))
targetComparatorOutcomesList <- list(tcos)

# Target and comparator ingredient concepts
aceI <- c(1335471,1340128,1341927,1363749,1308216,1310756,1373225, 1331235,1334456,1342439)
thz <- c(1395058,974166,978555,907013)

# Define which types of covariates must be constructed
cs <- createDefaultCovariateSettings(excludedCovariateConceptIds = c(aceI, thz),
                                     addDescendantsToExclude = TRUE)

# Define Args
cmdArgs <- createGetDbCohortMethodDataArgs(
  studyStartDate = "",
  studyEndDate = "",
  firstExposureOnly = FALSE,
  removeDuplicateSubjects = FALSE,
  restrictToCommonPeriod = FALSE,
  washoutPeriod = 365,
  covariateSettings = cs)
spArgs <- createCreateStudyPopulationArgs(
  firstExposureOnly = FALSE,
  restrictToCommonPeriod = FALSE,
  washoutPeriod = 365,
  removeDuplicateSubjects = "remove all",
  removeSubjectsWithPriorOutcome = TRUE,
  minDaysAtRisk = 1,
  startAnchor = "cohort start",
  addExposureDaysToStart = FALSE,
  endAnchor = "cohort end",
  addExposureDaysToEnd = TRUE)
psArgs <- createCreatePsArgs()
matchArgs1 <- createMatchOnPsArgs(
  caliper = 0.1,
  caliperScale = "standardized logit",
  maxRatio = 1)
matchArgs2 <- createMatchOnPsArgs(
  caliper = 0.2,
  caliperScale = "standardized logit",
  maxRatio = 1)
fomArgsT <- createFitOutcomeModelArgs(
  modelType = "cox",
  stratified = FALSE)

# Define analyses
cmAnalysis1 <- createCmAnalysis(
  analysisId = 1,
  description = "PSM, caliper=0.10",
  getDbCohortMethodDataArgs = cmdArgs,
  createStudyPopArgs = spArgs,
  createPs = TRUE,
  createPsArgs = psArgs,
  matchOnPs = TRUE,
  matchOnPsArgs = matchArgs1,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fomArgsT)
cmAnalysis2 <- createCmAnalysis(
  analysisId = 2,
  description = "PSM, caliper=0.20",
  getDbCohortMethodDataArgs = cmdArgs,
  createStudyPopArgs = spArgs,
  createPs = TRUE,
  createPsArgs = psArgs,
  matchOnPs = TRUE,
  matchOnPsArgs = matchArgs2,
  fitOutcomeModel = TRUE,
  fitOutcomeModelArgs = fomArgsT)
cmAnalysisList <- list(cmAnalysis1, cmAnalysis2)

rm(tcos, aceI, thz, cs,
   cmdArgs, spArgs, psArgs, matchArgs1, matchArgs2, fomArgsT,
   cmAnalysis1, cmAnalysis2)

# Run cmAnalyses
for(i in 1:num_cohorts){
  
  # Define output folder
  outputFolder <- paste(wd, paste("cohort", i, sep="_"), "Results_1", sep="/")
  
  if(file.exists(paste(outputFolder, "analysisSummary.rds", sep="/"))){
    rm(outputFolder)
    print(paste("Analyses completed for cohort", i, "- moving to next cohort"))
    next
  } else{
    print(paste("Performing analyses for cohort", i))
  }
  
  result <- runCmAnalyses(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          exposureDatabaseSchema = cohortDatabaseSchema,
                          exposureTable = paste("SF_Methods_1_", i, sep=""),
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = cohortOutcomeTable,
                          cdmVersion = cdmVersion,
                          outputFolder = outputFolder,
                          cmAnalysisList = cmAnalysisList,
                          targetComparatorOutcomesList = targetComparatorOutcomesList)
  
  analysisSum <- summarizeAnalyses(result, outputFolder)
  saveRDS(analysisSum, paste(outputFolder, "analysisSummary.rds", sep="/"))

  rm(outputFolder, result, analysisSum)
}
rm(cmAnalysisList, targetComparatorOutcomesList)


# ==================================================================================================================
# Create separate ps objects
# ==================================================================================================================

for(i in 1:num_cohorts){
  if(file.exists(paste0(wd, "/cohort_", i, "/ps_3.rds"))){
    next
  } else{
    # Define parameters
    cmData <- loadCohortMethodData(paste(wd, "/cohort_", i, "/cmData", sep = ""))
    studyPop <- createStudyPopulation(cohortMethodData = cmData,
                                      outcomeId = 3,
                                      firstExposureOnly = FALSE,
                                      restrictToCommonPeriod = FALSE,
                                      washoutPeriod = 365,
                                      removeDuplicateSubjects = "remove all",
                                      removeSubjectsWithPriorOutcome = TRUE,
                                      minDaysAtRisk = 1,
                                      riskWindowStart =1,
                                      startAnchor = "cohort start",
                                      riskWindowEnd = 0,
                                      endAnchor = "cohort end")
  }
  
  if(!file.exists(paste0(wd, "/cohort_", i, "/ps_2.rds"))){
    source("createPs2.R")
    ps <- createPs2(cohortMethodData = cmData, population = studyPop)
    saveRDS(ps, file = paste0(wd, "/cohort_", i, "/ps_2.rds"))
    rm(ps, createPs2)
  }
  if(!file.exists(paste0(wd, "/cohort_", i, "/ps_3.rds"))){
    source("createPs3.R")
    ps <- createPs3(cohortMethodData = cmData, population = studyPop)
    saveRDS(ps, file = paste0(wd, "/cohort_", i, "/ps_3.rds"))
    rm(ps, createPs3)
  }
  rm(cmData, studyPop)
}


# ==================================================================================================================
# Create data frame for cardinality matching
# ==================================================================================================================

for(i in 1:num_cohorts){
  if(file.exists(paste(wd, "/cohort_", i, "/cm_df_3.csv", sep = ""))){
    print(paste("Cardinality matching data frame already created for cohort", i, "- moving to next cohort"))
    next
  } else{
    print(paste("Creating cardinality matching data frame for cohort", i))
  }
  
  for(k in 1:3){
    # Define parameters
    cmData <- loadCohortMethodData(paste(wd, "/cohort_", i, "/cmData", sep = ""))
    if(k == 1){
      ps <- readRDS(file = paste(wd, "/cohort_", i, "/Results_1/Ps_l1_s1_p1_t1_c2.rds", sep = ""))
    }else if(k == 2){
      ps <- readRDS(paste0(wd, "/cohort_", i, "/ps_2.rds"))
    } else{
      ps <- readRDS(paste0(wd, "/cohort_", i, "/ps_3.rds"))
    }
    propensityModel <- getPsModel(ps, cmData)
    covariateId <- propensityModel$covariateId[propensityModel$covariateName != "(Intercept)"]
    
    # Create data frame for cardinality mtaching
    cm_df <- cmData$covariates[cmData$covariates[, "covariateId"] %in% covariateId, ]
    cm_df <- dcast(cm_df, rowId ~ covariateId, value.var = "covariateValue")
    cm_df <- merge(cm_df, cmData$cohorts[, c("rowId", "treatment")], by="rowId")
    cm_df[is.na(cm_df)] <- 0
    cm_df <- cm_df[cm_df$rowId %in% ps$rowId, ]
    if(sum(ps$rowId %in% cm_df$rowId == FALSE) > 0){
      x <- ps[ps$rowId %in% cm_df$rowId == FALSE, ]
      for(k in 1:nrow(x)){
        temp <- c(x$rowId[k],
                  rep(0, (ncol(cm_df)-2)),
                  x$treatment[k])
        cm_df <- rbind(cm_df, temp)
        rm(temp)
      }
      rm(x)
    }
    cm_df <- cm_df[order(cm_df$treatment, decreasing = TRUE), ]
    
    # Print ncol/nrow
    print(ncol(cm_df))
    print(nrow(cm_df))
    
    # Save file
    if(k == 1){
      write.csv(cm_df, paste(wd, "/cohort_", i, "/cm_df.csv", sep = ""), row.names = FALSE)
    }else if(k == 2){
      write.csv(cm_df, paste(wd, "/cohort_", i, "/cm_df_2.csv", sep = ""), row.names = FALSE)
    } else{
      write.csv(cm_df, paste(wd, "/cohort_", i, "/cm_df_2.csv", sep = ""), row.names = FALSE)
    }
    
    rm(cmData, ps, propensityModel, covariateId, cm_df)
  }
}


# ==================================================================================================================
# Perform cardinality matching
# ==================================================================================================================

# Perform cardinality matching
datafile <- c("mom_001.rds", "mom_005.rds", "mom_010.rds",
              "mom_001_atc.rds", "mom_005_atc.rds", "mom_010_atc.rds")
for(i in 1:num_cohorts){
  
  if(file.exists(paste0(wd, "/cohort_", i, "/", datafile[length(datafile)]))){
    print(paste("Cardinality matching already complete for cohort", i, "- moving to next cohort"))
    next
  }
  
  # Define parameters
  t_max = 360*60
  
  
  if(!file.exists(paste(wd, paste0("cohort_", i), "fine.rds", sep="/"))){
    # Fine matching
    print(paste("Performing fine matching for cohort", i))
    
    cm_df <- read.csv(paste(wd, "/cohort_", i, "/cm_df_3.csv", sep = ""))
    t_ind <- cm_df[, "treatment"]
    
    # Fine balance
    fine_covs <- cm_df[, -which(colnames(cm_df) %in% c("treatment", "rowId"))]
    fine <- list(covs = fine_covs)
    
    # Solver options
    name <- "gurobi"
    approximate <- 0
    solver <- list(name = name, t_max = t_max, approximate = approximate,
                   round_cplex = 0, trace = 0)
    
    # Match (fine balance)                
    out_1 <- cardmatch(t_ind, fine = fine, solver = solver)
    
    print(length(out_1$t_id))
    print(length(out_1$c_id))
    
    saveRDS(out_1, file = paste(wd, "/cohort_", i, "/fine.rds", sep = ""))
    
    rm(fine_covs, fine, name, approximate, solver, out_1)
  } else{
    print(paste("Fine matching already completed for cohort", i))
  }
  
  # MOM matching
  for(j in 1:length(datafile)){
    
    if(file.exists(paste0(wd, "/cohort_", i, "/", datafile[j]))){
      print(paste0("MOM matching (", j, ") already completed for cohort ", i))
      next
    } else{
      print(paste0("Performing MOM matching (", j, ") for cohort ", i))
    }
    
    if(j == 1){
      cm_df <- read.csv(paste(wd, "/cohort_", i, "/cm_df_2.csv", sep = ""))
      t_ind <- cm_df[, "treatment"]
    }
    
    if(j == 4){
      cm_df <- read.csv(paste(wd, "/cohort_", i, "/cm_df_2.csv", sep = ""))
      t_ind <- cm_df[, "treatment"]
    }
    
    # Set max_stdiff
    if(j == 1 | j == 4){
      max_stdiff <- 0.01
    } else if(j == 2 | j == 5){
      max_stdiff <- 0.05
    } else {
      max_stdiff <- 0.10
    }
    
    # Set target estimand
    if(j == 1 | j == 2 | j == 3){
      target <- NULL
    } else{
      target <- "ATC"
    }
    
    # Moment balance matrix
    mom_covs <- cm_df[, -which(colnames(cm_df) %in% c("treatment", "rowId"))]
    
    # Moment balance
    if(is.null(target)){
      mom_targets=NULL
      mom_tols = absstddif(mom_covs, t_ind, max_stdiff)
    } else if(target %in% c('ATC', 'ATE', 'ATT')){
      # Calculate estimand targets
      ATC <- apply(mom_covs[t_ind == 0, ], 2, mean)
      ATT <- apply(mom_covs[t_ind == 1, ], 2, mean)
      ATE <- apply(mom_covs, 2, mean)
      
      mom_targets=get(target)
      mom_tols = absstddif(mom_covs, t_ind, max_stdiff/2)
      
      rm(ATC, ATT, ATE)
    } else{
      print("Error: target should be set to NULL, 'ATC', 'ATE' or 'ATT'")
    }
    mom=list(covs=mom_covs, tols=mom_tols, targets=mom_targets)
    
    # Solver options
    t_max = t_max
    name = "gurobi"
    approximate = 0
    solver = list(name = name, t_max = t_max, approximate = approximate,
                  round_cplex = 0, trace = 0)
    
    # Perform match (moment balance)
    out_1 <- cardmatch(t_ind, mom = mom, solver = solver)
    
    print(length(out_1$t_id))
    print(length(out_1$c_id))
    
    # Save results
    saveRDS(out_1, paste0(wd, "/cohort_", i, "/", datafile[j]))
    
    rm(max_stdiff, target, mom_covs, mom_targets, mom_tols, mom, name, approximate, solver, out_1)
  }
  
  rm(ps, cm_df, t_max, t_ind)
}


# Verify results cardinality matching
no_matches <- data.frame(matrix(0, nrow = 0, ncol = 2, dimnames = list(NULL, c("cohort", "cm"))))
unbal_matches <- data.frame(matrix(0, nrow = 0, ncol = 4, 
                                   dimnames = list(NULL, c("cohort", "cm", "t_id", "c_id"))))
for(i in 1:num_cohorts){
  for(j in 0:length(datafile)){
    
    # Get rowIds of interest
    if(j == 0){
      out_1 <- readRDS(paste0(wd, "/cohort_", i, "/fine.rds"))
    } else{
      out_1 <- readRDS(paste0(wd, "/cohort_", i, "/", datafile[j]))
    }
    t_id <- out_1$t_id
    c_id <- out_1$c_id
    
    if(length(t_id) == 0){
      no_matches <- rbind(no_matches, c(i, j))
      colnames(no_matches) <- c("cohort", "cm")
    }
    
    if(length(t_id) != length(c_id)){
      unbal_matches <- rbind(unbal_matches, c(i, j, length(t_id), length(c_id)))
      colnames(unbal_matches) <- c("cohort", "cm", "t_id", "c_id")
    }
    
    rm(out_1, t_id, c_id)
  }
}
no_matches
unbal_matches

rm(datafile)


# ==================================================================================================================
# Populate SQL tables with Cardinality Matching Populations
# ==================================================================================================================

# datafile <- c("mom_001.rds", "mom_005.rds", "mom_010.rds",
#               "mom_001_atc.rds", "mom_005_atc.rds", "mom_010_atc.rds", "fine.rds")
# cohort_id <- list(c(5,6), c(7,8), c(9,10), c(11,12), c(13,14), c(15,16), c(17, 18))
# for(i in 1:num_cohorts){
# 
#   print(paste("Adding cardinality matching cohorts to SQL table for cohort", i))
# 
#   cmData <- loadCohortMethodData(paste(wd, "/cohort_", i, "/cmData", sep = ""))
#   cm_df <- read.csv(paste(wd, "/cohort_", i, "/cm_df.csv", sep = ""))
# 
#   # Test cohorts appropriately generated
#   sql <- paste("SELECT cohort_definition_id",
#                "FROM @cohortDatabaseSchema.@cohortTable",
#                "GROUP BY cohort_definition_id")
#   sql <- render(sql,
#                 cohortDatabaseSchema = cohortDatabaseSchema,
#                 cohortTable = paste("SF_Methods_1_", i, sep=""))
#   sql <- translate(sql, targetDialect = connectionDetails$dbms)
#   cohortIds <- querySql(connection, sql)
# 
#   for(j in 1:length(datafile)){
# 
#     if(sum(cohort_id[[j]] %in% cohortIds$COHORT_DEFINITION_ID) == 2){
#       print(paste("Datafile", datafile[j], "already uploaded"))
#       next
#     } else{
#       print(paste("Uploading datafile", datafile[j]))
#     }
# 
#     t_cohort_id <- cohort_id[[j]][1]
#     c_cohort_id <- cohort_id[[j]][2]
# 
#     # Get rowIds of interest
#     out_1 <- readRDS(paste0(wd, "/cohort_", i, "/", datafile[j]))
#     t_id <- out_1$t_id
#     c_id <- out_1$c_id
#     t_rowId <- cm_df[t_id, "rowId"]
#     c_rowId <- cm_df[c_id, "rowId"]
# 
#     if(length(t_rowId) == 0 | length(c_rowId) == 0){
#       next
#     }
# 
#     # Determine subjectIds
#     t_subjectIds <- unique(cmData$cohorts$subjectId[cmData$cohorts$rowId %in% t_rowId])
#     c_subjectIds <- unique(cmData$cohorts$subjectId[cmData$cohorts$rowId %in% c_rowId])
# 
#     # Check data before proceeding
#     if(length(t_subjectIds) != length(c_subjectIds) |
#        length(t_subjectIds) != length(t_rowId) |
#        length(c_subjectIds) != length(c_rowId)){
#       print(sprintf("Error - mismatched target and comparator sample sizes"))
#       print(sprintf("Error - length(t_id) = %s, length(c_id) = %s",
#                     length(t_rowId), length(c_rowId)))
#       print(sprintf("Error - length(t_subjectIds) = %s, length(c_subjectIds) = %s",
#                     length(t_subjectIds), length(c_subjectIds)))
#       next
#     }
# 
#     for(k in 1:ceiling(length(t_subjectIds)/15000)){
#       if(k == ceiling(length(t_subjectIds)/15000)){
#         range_x <- c(((k-1)*15000+1):(length(t_subjectIds)))
#       }else{
#         range_x <- c(((k-1)*15000+1):(k*15000))
#       }
# 
#       print(k)
# 
#       # Populate cohort table
#       sql <- readSql("create_cm_cohorts_100.sql")
#       sql <- render(sql,
#                     cohortDatabaseSchema = cohortDatabaseSchema,
#                     cohortTable = paste("SF_Methods_1_", i, sep=""),
#                     t_cohort_id = t_cohort_id,
#                     c_cohort_id = c_cohort_id,
#                     t_subjectIds = t_subjectIds[range_x],
#                     c_subjectIds = c_subjectIds[range_x])
#       sql <- translate(sql, targetDialect = connectionDetails$dbms)
#       executeSql(connection, sql)
# 
#       rm(range_x)
#     }
# 
#     rm(out_1, t_id, c_id, t_rowId, c_rowId, t_subjectIds, c_subjectIds, sql, t_cohort_id, c_cohort_id)
#   }
#   rm(cohortIds, cm_df, cmData, out_1, t_id, c_id, t_rowId, c_rowId, t_subjectIds, c_subjectIds,
#      sql, t_cohort_id, c_cohort_id)
# }
# rm(datafile, cohort_id)
# 
# 
# # Verify counts
# mismatch <- data.frame(matrix(0, nrow = 0, ncol = 4,
#                               dimnames = list(NULL, c("cohort", "cm", "t_id", "c_id"))))
# datafile <- c("mom_001.rds", "mom_005.rds", "mom_010.rds",
#               "mom_001_atc.rds", "mom_005_atc.rds", "mom_010_atc.rds", "fine.rds")
# cohort_id <- list(c(5,6), c(7,8), c(9,10), c(11,12), c(13,14), c(15,16), c(17, 18))
# for(i in 1:num_cohorts){
#   # Test cohorts appropriately generated
#   sql <- paste("SELECT cohort_definition_id, COUNT(*) AS count",
#                "FROM @cohortDatabaseSchema.@cohortTable",
#                "WHERE cohort_definition_id IN (5,6,7,8,9,10,11,12,13,14,15,16,17,18)",
#                "GROUP BY cohort_definition_id")
#   sql <- render(sql,
#                 cohortDatabaseSchema = cohortDatabaseSchema,
#                 cohortTable = paste("SF_Methods_1_", i, sep=""))
#   sql <- translate(sql, targetDialect = connectionDetails$dbms)
#   temp <- querySql(connection, sql)
# 
#   for(j in 1:length(cohort_id)){
#     if(sum(c(cohort_id[[j]][1], cohort_id[[j]][2]) %in% temp$COHORT_DEFINITION_ID) == 0){
#       next
#     } else if(temp$COUNT[temp$COHORT_DEFINITION_ID == cohort_id[[j]][1]] !=
#               temp$COUNT[temp$COHORT_DEFINITION_ID == cohort_id[[j]][2]]){
#       mismatch <- rbind(mismatch, c(i, j,
#                                     temp$COUNT[temp$COHORT_DEFINITION_ID == cohort_id[[j]][1]],
#                                     temp$COUNT[temp$COHORT_DEFINITION_ID == cohort_id[[j]][2]]))
#       colnames(mismatch) <- c("cohort", "cm", "t_id", "c_id")
#     }
#   }
# 
#   rm(sql, temp)
# }
# rm(cohort_id, datafile)
# mismatch
# rm(mismatch)


# ==================================================================================================================
# Run Analyses for CM
# ==================================================================================================================

# Define parameters
cohort_id <- list(c(5,6), c(7,8), c(9,10), c(11,12), c(13,14), c(15,16), c(17, 18))
description <- c("CM, SMD=0.01", "CM, SMD=0.05", "CM, SMD=0.10", 
                 "CM ATC, SMD=0.01", "CM ATC, SMD=0.05", "CM ATC, SMD=0.10", 
                 "CM Fine match")

# Target and comparator ingredient concepts
aceI <- c(1335471,1340128,1341927,1363749,1308216,1310756,1373225, 1331235,1334456,1342439)
thz <- c(1395058,974166,978555,907013)

# Define which types of covariates must be constructed
cs <- createDefaultCovariateSettings(excludedCovariateConceptIds = c(aceI, thz),
                                     addDescendantsToExclude = TRUE)

# Define Args
cmdArgs <- createGetDbCohortMethodDataArgs(
  studyStartDate = "",
  studyEndDate = "",
  firstExposureOnly = FALSE,
  removeDuplicateSubjects = FALSE,
  restrictToCommonPeriod = FALSE,
  washoutPeriod = 365,
  covariateSettings = cs)
spArgs <- createCreateStudyPopulationArgs(
  firstExposureOnly = FALSE,
  restrictToCommonPeriod = FALSE,
  washoutPeriod = 365,
  removeDuplicateSubjects = "remove all",
  removeSubjectsWithPriorOutcome = TRUE,
  minDaysAtRisk = 1,
  startAnchor = "cohort start",
  addExposureDaysToStart = FALSE,
  endAnchor = "cohort end",
  addExposureDaysToEnd = TRUE)
fomArgsF <- createFitOutcomeModelArgs(
  modelType = "cox",
  stratified = FALSE)

for(j in 1:length(cohort_id)){
  assign(paste0("tcos_", j), 
         createTargetComparatorOutcomes(targetId = cohort_id[[j]][1], 
                                        comparatorId = cohort_id[[j]][2], 
                                        outcomeIds = c(3, ncs)))
  assign(paste0("targetComparatorOutcomesList_", j),
         list(get(paste0("tcos_", j))))
  assign(paste0("cmAnalysis_", j),
         createCmAnalysis(analysisId = j,
                          description = description[j],
                          getDbCohortMethodDataArgs = cmdArgs,
                          createStudyPopArgs = spArgs,
                          createPs = FALSE,
                          createPsArgs = NULL,
                          matchOnPs = FALSE,
                          matchOnPsArgs = NULL,
                          fitOutcomeModel = TRUE,
                          fitOutcomeModelArgs = fomArgsF))
  assign(paste0("cmAnalysisList", j), list(get(paste0("cmAnalysis_", j))))
}
rm(list = ls(pattern = "tcos_")); rm(list = ls(pattern = "cmAnalysis_"))
rm(cohort_id, aceI, thz, cs, description, cmdArgs, spArgs, fomArgsF)


# Run cmAnalyses
datafile <- c("mom_001.rds", "mom_005.rds", "mom_010.rds",
              "mom_001_atc.rds", "mom_005_atc.rds", "mom_010_atc.rds", "fine.rds")
for(i in 1:num_cohorts){
  print(paste("Performing analyses for cohort", i))
  
  for(j in 1:length(datafile)){
    
    if(nrow(subset(no_matches, cohort == i & cm == j))){
      next
    }
    
    # Define output folder
    outputFolder <- paste(wd, paste0("cohort_", i), paste0("Results_2_", j), sep="/")
    
    if(file.exists(paste(outputFolder, "analysisSummary.rds", sep="/"))){
      rm(outputFolder)
      print(paste("Analyses completed for cohort", i, "- datafile", datafile[j]))
      next
    } else{
      print(paste("Performing analyses for cohort", i, "- datafile", datafile[j]))
    }
    
    result <- runCmAnalyses(connectionDetails = connectionDetails,
                            cdmDatabaseSchema = cdmDatabaseSchema,
                            exposureDatabaseSchema = cohortDatabaseSchema,
                            exposureTable = paste("SF_Methods_1_", i, sep=""),
                            outcomeDatabaseSchema = cohortDatabaseSchema,
                            outcomeTable = cohortOutcomeTable,
                            cdmVersion = cdmVersion,
                            outputFolder = outputFolder,
                            cmAnalysisList = get(paste0("cmAnalysisList", j)),
                            targetComparatorOutcomesList = get(paste0("targetComparatorOutcomesList_", j)))
    
    analysisSum <- summarizeAnalyses(result, outputFolder)
    saveRDS(analysisSum, paste(outputFolder, "analysisSummary.rds", sep="/"))
    
    rm(outputFolder, result, analysisSum)
  }
}
rm(datafile); rm(list = ls(pattern = "cmAnalysisList")); rm(list = ls(pattern = "targetComparatorOutcomesList_"))


# ==================================================================================================================
# Run Analyses for Overall Sample
# ==================================================================================================================

# Define parameters
cohort_id <- list(c(1,2))
description <- c("overall Sample")

# Target and comparator ingredient concepts
aceI <- c(1335471,1340128,1341927,1363749,1308216,1310756,1373225, 1331235,1334456,1342439)
thz <- c(1395058,974166,978555,907013)

# Define which types of covariates must be constructed
cs <- createDefaultCovariateSettings(excludedCovariateConceptIds = c(aceI, thz),
                                     addDescendantsToExclude = TRUE)

# Define Args
cmdArgs <- createGetDbCohortMethodDataArgs(
  studyStartDate = "",
  studyEndDate = "",
  firstExposureOnly = FALSE,
  removeDuplicateSubjects = FALSE,
  restrictToCommonPeriod = FALSE,
  washoutPeriod = 365,
  covariateSettings = cs)
spArgs <- createCreateStudyPopulationArgs(
  firstExposureOnly = FALSE,
  restrictToCommonPeriod = FALSE,
  washoutPeriod = 365,
  removeDuplicateSubjects = "remove all",
  removeSubjectsWithPriorOutcome = TRUE,
  minDaysAtRisk = 1,
  startAnchor = "cohort start",
  addExposureDaysToStart = FALSE,
  endAnchor = "cohort end",
  addExposureDaysToEnd = TRUE)
fomArgsF <- createFitOutcomeModelArgs(
  modelType = "cox",
  stratified = FALSE)

for(j in 1:length(cohort_id)){
  assign(paste0("tcos_", j), 
         createTargetComparatorOutcomes(targetId = cohort_id[[j]][1], 
                                        comparatorId = cohort_id[[j]][2], 
                                        outcomeIds = c(3, ncs)))
  assign(paste0("targetComparatorOutcomesList_", j),
         list(get(paste0("tcos_", j))))
  assign(paste0("cmAnalysis_", j),
         createCmAnalysis(analysisId = j,
                          description = description[j],
                          getDbCohortMethodDataArgs = cmdArgs,
                          createStudyPopArgs = spArgs,
                          createPs = FALSE,
                          createPsArgs = NULL,
                          matchOnPs = FALSE,
                          matchOnPsArgs = NULL,
                          fitOutcomeModel = TRUE,
                          fitOutcomeModelArgs = fomArgsF))
  assign(paste0("cmAnalysisList", j), list(get(paste0("cmAnalysis_", j))))
}
rm(list = ls(pattern = "tcos_")); rm(list = ls(pattern = "cmAnalysis_"))
rm(cohort_id, aceI, thz, cs, description, cmdArgs, spArgs, fomArgsF)


# Run cmAnalyses
for(i in 1:num_cohorts){
  print(paste("Performing analyses for cohort", i))

  # Define output folder
  outputFolder <- paste(wd, paste0("cohort_", i), "Results_0", sep="/")
  
  if(file.exists(paste(outputFolder, "analysisSummary.rds", sep="/"))){
    rm(outputFolder)
    print(paste("Analyses completed for cohort", i, "- overall sample"))
    next
  } else{
    print(paste("Performing analyses for cohort", i, "- overall sample"))
  }
  
  result <- runCmAnalyses(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          exposureDatabaseSchema = cohortDatabaseSchema,
                          exposureTable = paste("SF_Methods_1_", i, sep=""),
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = cohortOutcomeTable,
                          cdmVersion = cdmVersion,
                          outputFolder = outputFolder,
                          cmAnalysisList = get(paste0("cmAnalysisList1")),
                          targetComparatorOutcomesList = get(paste0("targetComparatorOutcomesList_1")))
  
  analysisSum <- summarizeAnalyses(result, outputFolder)
  saveRDS(analysisSum, paste(outputFolder, "analysisSummary.rds", sep="/"))
  
  rm(outputFolder, result, analysisSum)
}
rm(list = ls(pattern = "cmAnalysisList_")); rm(list = ls(pattern = "targetComparatorOutcomesList_"))

no_matches$cm[1] <- 7



#
# 
#
#
# 
#
#
# 
#
#
# 
#




# ==================================================================================================================
# NCS plots: summary of all cohorts
# ==================================================================================================================

# Define parameters
negCons_list <- vector(mode = "list", length = 10)
hoi_list <- vector(mode = "list", length = 10)
for(j in 1:10){
  negCons_list[[j]] <- list()
  hoi_list[[j]] <- list()
}
for(i in 1:num_cohorts){
  # Skip cohorts not balanced properly
  if(i %in% unbal_matches$cohort){
    print(paste("Unbalanced matches for cohort", i, "- skipping"))
    next
  }
  
  for(j in 1:10){
    # # Check for lack a matches / balance
    # if(nrow(subset(no_matches, cohort == i & cm == (j-3)))){
    #   print(paste("No matches for cohort", i, "match", j, "- skipping"))
    #   next
    # } 
    
    # Load data
    if(j == 1){
      outputFolder <- paste(wd, paste("cohort", i, sep="_"), "Results_0", sep="/")
    }else if(j == 2 | j == 3){
      outputFolder <- paste(wd, paste("cohort", i, sep="_"), "Results_1", sep="/")
    } else{
      outputFolder <- paste(wd, paste("cohort", i, sep="_"), paste0("Results_2_", j-3), sep="/")
    }
    analysisSum <- readRDS(paste(outputFolder, "analysisSummary.rds", sep="/"))
    
    # Define parameters
    if(j == 1){
      negCons <- analysisSum[analysisSum$analysisId == j & analysisSum$outcomeId != 3, ]
      hoi <-  analysisSum[analysisSum$analysisId == j & analysisSum$outcomeId == 3, ]
    }else if(j == 2 | j == 3){
      negCons <- analysisSum[analysisSum$analysisId == (j-1) & analysisSum$outcomeId != 3, ]
      hoi <-  analysisSum[analysisSum$analysisId == (j-1) & analysisSum$outcomeId == 3, ]
    } else{
      negCons <- analysisSum[analysisSum$analysisId == (j-3) & analysisSum$outcomeId != 3, ]
      hoi <-  analysisSum[analysisSum$analysisId == (j-3) & analysisSum$outcomeId == 3, ]
    }
    negCons_list[[j]][[length(negCons_list[[j]]) + 1]] <- negCons
    hoi_list[[j]][[length(hoi_list[[j]]) + 1]] <- hoi
    
    rm(outputFolder, analysisSum, negCons, hoi)
  }
}


# Create plots
plts <- vector(mode = "list", length = 10)
nulls <- vector(mode = "list", length = 10)
p_ncs <- vector(mode = "list", length = 10)
p_hoi <- vector(mode = "list", length = 10)
models <- vector(mode = "list", length = 10)
calibrate_estimates <- vector(mode = "list", length = 10)
datafile <- c("mom_001.rds", "mom_005.rds", "mom_010.rds",
              "mom_001_atc.rds", "mom_005_atc.rds", "mom_010_atc.rds", "fine.rds")
titles <- c("maximum SMD=0.01", "maximum SMD=0.05", "maximum smd=0.10",
            "maximum SMD=0.01, target estimand=ATC", "maximum SMD=0.05, target estimand=ATC", "maximum smd=0.10, target estimand=ATC",
            "exact marginal distributional balance")
for(j in 1:10){
  print(j)
  # Define parameters
  negCons <- do.call(rbind, negCons_list[[j]])
  hoi <- do.call(rbind, hoi_list[[j]])
  #null <- fitNull(negCons$logRr, negCons$seLogRr)
  null <- fitMcmcNull(negCons$logRr, negCons$seLogRr)
  n_cohorts <- nrow(hoi)
  if(j == 1){
    title <- "Pre-Match Sample"
  }else if(j == 2 | j == 3){
    title <- sprintf("Propensity score Matching, caliper=0.%s0", (j-1))
  } else{
    title <- sprintf("Cardinality Matching, %s", titles[j-3])
  }
  
  plts[[j]] <- plotCalibrationEffect(logRrNegatives = negCons$logRr, 
                                     seLogRrNegatives = negCons$seLogRr, 
                                    # logRrPositives = hoi$logRr, 
                                    #  
                                     null = null,
                                     showExpectedSystematicError = TRUE,
                                     title = title,
                                     showCis = TRUE)
  nulls[[j]] <- null
  
  # p_ncs[[j]] <- cbind(negCons[, c("rr", "ci95lb", "ci95ub", "outcomeId")], calibrateP(null, negCons$logRr, negCons$seLogRr))
  p_ncs[[j]] <- cbind(negCons[, c("rr", "ci95lb", "ci95ub", "outcomeId", "p")])
  p_hoi[[j]] <- cbind(hoi[, c("rr", "ci95lb", "ci95ub", "outcomeId")], calibrateP(null, hoi$logRr, hoi$seLogRr))
  
  models[[j]] <- convertNullToErrorModel(null)            
  calibrate_estimates[[j]] <- cbind(calibrateConfidenceInterval(logRr = hoi$logRr,
                                                     seLogRr = hoi$seLogRr, 
                                                     model = models[[j]],
                                                     ciWidth = 0.95),
                                calibrateP(null, hoi$logRr, hoi$seLogRr))
  
  rm(negCons, hoi, title, null, n_cohorts)
}
rm(datafile, negCons_list, hoi_list)

# # Plot
# wb <- createWorkbook()
# addWorksheet(wb, "ncs", gridLines = FALSE)
# for(j in 1:10){
#   plot(plts[[j]])
#   insertPlot(wb = wb, sheet = 1, width = 6, height = 4, units = "in",
#              startRow = 4+20*j-20, startCol = 2)
# }
# saveWorkbook(wb, "ncs.xlsx", overwrite = TRUE)
# rm(wb)


# ==================================================================================================================
# Balance plots
# ==================================================================================================================

# Create balance objects
balance_files <- paste0("balance_", 1:9)
datafile <- c("mom_001.rds", "mom_005.rds", "mom_010.rds",
              "mom_001_atc.rds", "mom_005_atc.rds", "mom_010_atc.rds", "fine.rds")
source("computeCovariateBalance_SF.R")
for(i in 1:num_cohorts){
  print(paste("Performing calculations for cohort", i))
  
  # Find matching covariate ids
  if(!file.exists(paste(wd, paste0("cohort_", i), paste0("balance_", 8, ".rds"), sep="/"))){
    cmData <- loadCohortMethodData(paste(wd, "/cohort_", i, "/cmData", sep = ""))
    studyPop <- createStudyPopulation(cohortMethodData = cmData,
                                      outcomeId = 3,
                                      firstExposureOnly = FALSE,
                                      restrictToCommonPeriod = FALSE,
                                      washoutPeriod = 365,
                                      removeDuplicateSubjects = "remove all",
                                      removeSubjectsWithPriorOutcome = TRUE,
                                      minDaysAtRisk = 1,
                                      riskWindowStart =1,
                                      startAnchor = "cohort start",
                                      riskWindowEnd = 0,
                                      endAnchor = "cohort end")
    ps <- readRDS(file = paste(wd, "/cohort_", i, "/Results_1/Ps_l1_s1_p1_t1_c2.rds", sep = ""))
    propensityModel <- getPsModel(ps, cmData)
    covariateId <- propensityModel$covariateId[propensityModel$covariateName != "(Intercept)"]
    
    rm(propensityModel)
  }
  
  for(j in 1:9){
    # Check for lack a matches / balance
    if(nrow(subset(no_matches, cohort == i & cm == (j-2))) == 1 & (j-2)!=0){
      print(paste("No matches for cohort", i, "data file", datafile[j-2], "- skipping"))
      next
    } else if(nrow(subset(unbal_matches, cohort == i & cm == (j-2)))==1 & (j-2)!=0){
      print(paste("Unbalanced matches for cohort", i, "data file", datafile[j-2], "- skipping"))
      next
    }
    
    # Create balance objects
    if(!file.exists(paste(wd, paste0("cohort_", i), paste0("balance_", j, ".rds"), sep="/"))){
      if(j == 1 | j == 2){
        matchedPop <- readRDS(file = paste0(wd, "/cohort_", i, "/Results_1/StratPop_l1_s1_p1_t1_c2_s", j, "_o3.rds"))
        balance <- computeCovariateBalance_SF(population = matchedPop, cohortMethodData = cmData, 
                                              rowIds = NULL, rowIds2 = ps$rowId, cm = FALSE)
        balance$match_var <- ifelse(balance$covariateId %in% covariateId, 1, 0)
      } else{
        if(j == 3 | j == 4 | j == 5){
          cm_df <- read.csv(paste(wd, paste0("cohort_", i), "cm_df_2.csv", sep="/"))
        } else if(j == 9){
          next
        } else{
          cm_df <- read.csv(paste(wd, paste0("cohort_", i), "cm_df_2.csv", sep="/"))
        }
        out_1 <- readRDS(paste(wd, paste0("cohort_", i), datafile[(j-2)], sep="/"))
        t_id <- out_1$t_id
        c_id <- out_1$c_id
        rowIds <- cm_df[c(t_id, c_id), "rowId"]
        
        if(j == 3){
          ps <- readRDS(file = paste(wd, "/cohort_", i, "/ps_2.rds", sep = ""))
          propensityModel <- getPsModel(ps, cmData)
          covariateId <- propensityModel$covariateId[propensityModel$covariateName != "(Intercept)"]
        }
        if(j == 6){
          ps <- readRDS(file = paste(wd, "/cohort_", i, "/ps_2.rds", sep = ""))
          propensityModel <- getPsModel(ps, cmData)
          covariateId <- propensityModel$covariateId[propensityModel$covariateName != "(Intercept)"]
        }
        
        matchedPop <- studyPop[studyPop$rowId %in% rowIds, ]
        matchedPop$propensityScore <- 0.50
        matchedPop$preferenceScore <- 0.50
        
        balance <- computeCovariateBalance_SF(population = matchedPop, cohortMethodData = cmData, 
                                              rowIds = rowIds, rowIds2 = ps$rowId)
        balance$match_var <- ifelse(balance$covariateId %in% covariateId, 1, 0)
        
        rm(cm_df, out_1, t_id, c_id, rowIds)
      }
      
      if(file.exists(paste(wd, paste0("cohort_", i), paste0("balance_", j, ".rds"), sep="/"))){
        file.remove(paste(wd, paste0("cohort_", i), paste0("balance_", j, ".rds"), sep="/"))
      }
      saveRDS(balance, file = paste(wd, paste0("cohort_", i), paste0("balance_", j, ".rds"), sep="/"))
      
      rm(matchedPop, balance)
    }
  }
  rm(cmData, studyPop, covariateId, ps)
}
rm(balance_files, computeCovariateBalance_SF, computeMeanAndSd, computeMeansPerGroup, quickSum)


# Create object lists / statistics
num_cov_candidates <- vector()
num_match_cov <- vector()
num_all_cov_list <- list()
bal_list <- list()
num_imbal_cov <- list()
num_imbal_match_cov <- list()
num_imbal_cov_pre <- vector()
for(j in 1:9){
  num_all_cov_list[[j]] <- vector()
  bal_list[[j]] <- list()
  num_imbal_cov[[j]] <- vector()
  num_imbal_match_cov[[j]] <- vector()
}
for(i in 1:num_cohorts){
  # Skip cohorts improperly balanced cohorts
  if(i %in% unbal_matches$cohort){
    print(paste("Unbalanced matches for cohort", i,  "- skipping"))
    next
  }
  
  for(j in 1:9){
    # Check for lack a matches / balance
    if(nrow(subset(no_matches, cohort == i & cm == (j-2)))){
      print(paste("No matches for cohort", i, "data file", datafile[j-2], "- skipping"))
      next
    }
    
    balance <- readRDS(paste(wd, paste0("cohort_", i), paste0("balance_", j, ".rds"), sep="/"))
    
    if(j == 1){
      num_cov_candidates <- c(num_cov_candidates, nrow(balance))
      num_match_cov <- c(num_match_cov, sum(balance$match_var == 1))
      num_imbal_cov_pre <- c(num_imbal_cov_pre,
                             sum(abs(balance$beforeMatchingStdDiff[!is.na(balance$beforeMatchingStdDiff)]) > 0.10))
    }
    
    num_imbal_cov[[j]] <- c(num_imbal_cov[[j]],
                            sum(abs(balance$afterMatchingStdDiff[!is.na(balance$beforeMatchingStdDiff) & !is.na(balance$afterMatchingStdDiff)]) > 0.10))
    num_imbal_match_cov[[j]] <- c(num_imbal_match_cov[[j]],
                                  sum(abs(balance$afterMatchingStdDiff[!is.na(balance$afterMatchingStdDiff) & balance$match_var == 1]) > 0.10))
    num_all_cov_list[[j]] <- c(num_all_cov_list[[j]], 
                               nrow(balance[!is.na(balance$beforeMatchingStdDiff) & !is.na(balance$afterMatchingStdDiff), ]))
    bal_list[[j]][[length(bal_list[[j]]) + 1]] <- balance
    
    rm(balance)
  }
}

# Create balance plots
bal_match_plt_list <- list()
bal_all_plt_list <- list()
bal_match <- list()
bal_all <- list()
smd_summary_list <- vector(mode = "list", length = 10)
titles <- c("maximum SMD=0.01", "maximum SMD=0.05", "maximum smd=0.10",
            "maximum SMD=0.01, target estimand=ATC", "maximum SMD=0.05, target estimand=ATC", "maximum smd=0.10, target estimand=ATC",
            "exact marginal distributional balance")
for(j in 1:9){
  balance <- do.call(rbind, bal_list[[j]])
  balance <- balance[!is.na(balance$beforeMatchingStdDiff) & !is.na(balance$afterMatchingStdDiff), ]
  n_cohorts <- length(balance[[j]])
  
  bal_match[[j]] <- balance[balance$match_var == 1, ]
  bal_all[[j]] <- balance
  
  # Set parameters
  if(j == 1 | j == 2){
    title_match <- sprintf("Propensity Score Matching, caliper=0.%s0", j)
    title_all <- sprintf("Propensity Score Matching, caliper=0.%s0", j)
  } else{
    title_match <- sprintf("Cardinality Matching, %s", titles[j-2])
    title_all <- sprintf("Cardinality Matching, %s", titles[j-2])
  }
  
  # Create plots
  bal_match_plt_list[[j]] <- plotCovariateBalanceScatterPlot(balance = balance[balance$match_var == 1, ], 
                                                             showCovariateCountLabel = FALSE, 
                                                             showMaxLabel = FALSE,
                                                             title = title_match)
  bal_all_plt_list[[j]] <- plotCovariateBalanceScatterPlot(balance = balance, 
                                                           showCovariateCountLabel = FALSE, 
                                                           showMaxLabel = FALSE,
                                                           title = title_all)
  
  # Save summary information on SMDs
  smd_summary_list[[j]] <- cbind(summary(abs(balance$beforeMatchingStdDiff)[balance$match_var == 1]),
                                 summary(abs(balance$afterMatchingStdDiff)[balance$match_var == 1]),
                                 summary(abs(balance$beforeMatchingStdDiff)),
                                 summary(abs(balance$afterMatchingStdDiff)))
  colnames(smd_summary_list[[j]]) <- c("smd_before_match_match_vars",
                                       "smd_after_match_match_vars",
                                       "smd_before_match_all_vars",
                                       "smd_after_match_all_vars")
  smd_summary_list[[j]] <- rbind(smd_summary_list[[j]],
                                 c(sd(abs(balance$beforeMatchingStdDiff)[balance$match_var == 1]),
                                   sd(abs(balance$afterMatchingStdDiff)[balance$match_var == 1]),
                                   sd(abs(balance$beforeMatchingStdDiff)),
                                   sd(abs(balance$afterMatchingStdDiff))))
  row.names(smd_summary_list[[j]])[7] <- "sd"
  
  rm(balance, n_cohorts, title_match, title_all)
}
rm(datafile, bal_list)

# Plot
wb <- createWorkbook()
addWorksheet(wb, "smds", gridLines = FALSE)
for(j in 1:9){
  plot(bal_match_plt_list[[j]])
  insertPlot(wb = wb, sheet = 1, width = 6, height = 4, units = "in",
             startRow = 4+20*j-20, startCol = 2)
  
  plot(bal_all_plt_list[[j]])
  insertPlot(wb = wb, sheet = 1, width = 6, height = 4, units = "in",
             startRow = 4+20*j-20, startCol = 2+10)
}
saveWorkbook(wb, "smds.xlsx", overwrite = TRUE)
rm(wb)


# ==================================================================================================================
# Calibrated estimates
# ==================================================================================================================

cal_est <- as.data.frame(do.call(rbind, calibrate_estimates))
row.names(cal_est) <- c("pre-match", "ps_cal010", "ps_cal020", "mom_001", "mom_005", "mom_010",
                        "mom_001_atc", "mom_005_atc", "mom_010_atc", "fine")


# ==================================================================================================================
# Sample sizes
# ==================================================================================================================

sample_sizes <- list()
datafile <- c("mom_001.rds", "mom_005.rds", "mom_010.rds",
              "mom_001_atc.rds", "mom_005_atc.rds", "mom_010_atc.rds", "fine.rds")
for(j in 1:9){
  sample_sizes[[j]] <- vector()
}
for(i in 1:num_cohorts){
  # Skip cohorts improperly balanced cohorts
  if(i %in% unbal_matches$cohort){
    print(paste("Unbalanced matches for cohort", i,  "- skipping"))
    next
  }
  
  for(j in 1:9){
    if(j == 1 | j == 2){
      matchedPop <- readRDS(file = paste0(wd, "/cohort_", i, "/Results_1/StratPop_l1_s1_p1_t1_c2_s", j, "_o3.rds"))
      sample_sizes[[j]] <- c(sample_sizes[[j]], sum(unname(getAttritionTable(matchedPop)[7, c(2,3)])))
      
      rm(matchedPop)
    }else{
      if(!file.exists(paste(wd, paste0("cohort_", i), datafile[(j-2)], sep="/"))){
        sample_sizes[[j]] <- c(sample_sizes[[j]], 0)
        next
      }
      
      out_1 <- readRDS(paste(wd, paste0("cohort_", i), datafile[(j-2)], sep="/"))
      t_id <- out_1$t_id
      c_id <- out_1$c_id
      sample_sizes[[j]] <- c(sample_sizes[[j]], (length(t_id) + length(c_id)))
      
      rm(out_1, t_id, c_id)
    }
  }
}

# Summarize sample size information
ss_df <- data.frame(t(apply(do.call(rbind, sample_sizes), 1, function(x) summary(x))))
ss_df$sd <- apply(do.call(rbind, sample_sizes), 1, function(x) sd(x))
ss_df$num_zero <- apply(do.call(rbind, sample_sizes), 1, function(x) sum(x == 0))
row.names(ss_df) <- c("ps_cal010", "ps_cal020", "mom_001", "mom_005", "mom_010",
                      "mom_001_atc", "mom_005_atc", "mom_010_atc", "fine")



# ==================================================================================================================
# NCS summary
# ==================================================================================================================

ncs_summary <- vector(mode = "list", length = 10)
for(j in 1:10){
  # Median sample size
  if(j == 1){
    sql <- paste("SELECT cohort_definition_id, COUNT(*) AS count",
                 "FROM @cohortDatabaseSchema.@cohortTable",
                 "WHERE cohort_definition_id IN (1, 2)",
                 "GROUP BY cohort_definition_id")
    sql <- render(sql,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTable = paste("SF_Methods_1_", i, sep=""))
    sql <- translate(sql, targetDialect = connectionDetails$dbms)
    temp <- querySql(connection, sql)
    
    t <- temp$COUNT[temp$COHORT_DEFINITION_ID == 1]
    c <- temp$COUNT[temp$COHORT_DEFINITION_ID == 2]
    
    rm(temp, sql)
  } else{
    t <- ss_df[j-1, "Median"]
    c <- t
  }
  
  # Number cohorts with >0 matches
  if(j == 1){
    n_cohorts <- num_cohorts - nrow(unbal_matches)
  } else{
    n_cohorts <- num_cohorts - ss_df[j-1, "num_zero"] - nrow(unbal_matches)
  }
  
  if(is.null(p_ncs[[j]])){
    # Number ncs
    n_ncs <- 0
    
    # Number p<0.05
    n_ncs_05 <- 0
    
    # Number hoi
    n_hoi <- 0
    
    # Number hoi p<0.05
    n_hoi_05 <- 0
  } else{
    # Number ncs
    n_ncs <- sum(!is.na(p_ncs[[j]]$p))
    
    # Number p<0.05
    n_ncs_05 <- sum(!is.na(p_ncs[[j]]$p) & p_ncs[[j]]$p < 0.05)
    
    # Number hoi
    n_hoi <- sum(!is.na(p_hoi[[j]]$p))
    
    # Number hoi p<0.05
    n_hoi_05 <- sum(!is.na(p_hoi[[j]]$p) & p_hoi[[j]]$p < 0.05)
  }
  ncs_summary[[j]] <- c(t, c, n_cohorts, n_ncs, n_ncs_05, n_hoi, n_hoi_05)
 
  rm(t, c, n_cohorts, n_ncs, n_ncs_05, n_hoi, n_hoi_05)
}

ncs_summary <- data.frame(do.call(rbind, ncs_summary))
row.names(ncs_summary) <- c("overall", "ps_cal010", "ps_cal020", "mom_001", "mom_005", "mom_010",
                            "mom_001_atc", "mom_005_atc", "mom_010_atc", "fine")
colnames(ncs_summary) <- c("median_t", " median_c", "n_cohorts", "n_ncs", "n_ncs_p<05", "n_hoi", "n_hoi_p<05")


# ==================================================================================================================
# SMD Summary
# ==================================================================================================================

match_smds <- t(smd_summary_list[[1]][, 1])
all_smds <- t(smd_summary_list[[1]][, 3])
for(j in 1:(length(smd_summary_list)-1)){
  if(is.null(smd_summary_list[[j]])){
    match_smds <- rbind(match_smds, rep(NA, 7))
    all_smds <- rbind(all_smds, rep(NA, 7))
    next
  }
  match_smds <- rbind(match_smds, t(smd_summary_list[[j]][, 2]))
  all_smds <- rbind(all_smds, t(smd_summary_list[[j]][, 4]))
}
row.names(match_smds) <- c("overall", "ps_cal010", "ps_cal020", "mom_001", "mom_005", "mom_010",
                           "mom_001_atc", "mom_005_atc", "mom_010_atc", "fine")
row.names(all_smds) <- c("overall", "ps_cal010", "ps_cal020", "mom_001", "mom_005", "mom_010",
                         "mom_001_atc", "mom_005_atc", "mom_010_atc", "fine")


# ==================================================================================================================
# Number Covariates Summary
# ==================================================================================================================

num_covs <- rbind(c(summary(num_cov_candidates), 
                    sd = sd(num_cov_candidates), 
                    sum =sum(num_cov_candidates), 
                    n = length(num_cov_candidates)), 
                  c(summary(num_match_cov), 
                    sd = sd(num_match_cov), 
                    sum = sum(num_match_cov), 
                    n = length(num_match_cov)),
                  c(summary(num_imbal_cov_pre), 
                    sd = sd(num_imbal_cov_pre), 
                    sum = sum(num_imbal_cov_pre), 
                    n = length(num_imbal_cov_pre)))
for(j in 1:length(num_all_cov_list)){
  num_covs <- rbind(num_covs,
                    c(summary(num_all_cov_list[[j]][!is.na(num_imbal_match_cov[[j]])]), 
                      sd = sd(num_all_cov_list[[j]][!is.na(num_imbal_match_cov[[j]])]), 
                      sum = sum(num_all_cov_list[[j]][!is.na(num_imbal_match_cov[[j]])]), 
                      n = length(num_all_cov_list[[j]][!is.na(num_imbal_match_cov[[j]])])),
                    c(summary(num_imbal_cov[[j]][!is.na(num_imbal_match_cov[[j]])]), 
                      sd = sd(num_imbal_cov[[j]][!is.na(num_imbal_match_cov[[j]])]), 
                      sum = sum(num_imbal_cov[[j]][!is.na(num_imbal_match_cov[[j]])]), 
                      n = length(num_imbal_cov[[j]][!is.na(num_imbal_match_cov[[j]])])),
                    c(summary(num_imbal_match_cov[[j]][!is.na(num_imbal_match_cov[[j]])]), 
                      sd = sd(num_imbal_match_cov[[j]][!is.na(num_imbal_match_cov[[j]])]), 
                      sum = sum(num_imbal_match_cov[[j]][!is.na(num_imbal_match_cov[[j]])]), 
                      n = length(num_imbal_match_cov[[j]][!is.na(num_imbal_match_cov[[j]])])))
}

temp <- c(paste("Covariates observed post-match:", c("ps_cal010", "ps_cal020", "mom_001", "mom_005", "mom_010",
                                                     "mom_001_atc", "mom_005_atc", "mom_010_atc", "fine")),
          paste("Num imbal cov post-match: ", c("ps_cal010", "ps_cal020", "mom_001", "mom_005", "mom_010",
                                                "mom_001_atc", "mom_005_atc", "mom_010_atc", "fine")),
          paste("Num imbal match cov post-match: ", c("ps_cal010", "ps_cal020", "mom_001", "mom_005", "mom_010",
                                                      "mom_001_atc", "mom_005_atc", "mom_010_atc", "fine")))
temp <- temp[c(seq(1, length(temp), 9), seq(2, length(temp), 9), seq(3, length(temp), 9),
               seq(4, length(temp), 9), seq(5, length(temp), 9), seq(6, length(temp), 9),
               seq(7, length(temp), 9), seq(8, length(temp), 9), seq(9, length(temp), 9))]
row.names(num_covs) <- c("Covariate candidates", "Matching covariates", 
                         "Num imbal cov pre", temp)
rm(temp)


# ==================================================================================================================
# Absolute Systematic Errors
# ==================================================================================================================

for(j in 1:10){
  if(j == 1){
    exp_abs_sys_err <- computeExpectedSystematicError(nulls[[j]])
  } else{
    exp_abs_sys_err <- rbind(exp_abs_sys_err, computeExpectedSystematicError(nulls[[j]]))
  }
}
row.names(exp_abs_sys_err) <- c("overall", "ps_cal010", "ps_cal020", "mom_001", "mom_005", "mom_010",
                                "mom_001_atc", "mom_005_atc", "mom_010_atc", "fine")


# ==================================================================================================================
# Summary tables
# ==================================================================================================================

# Plot
wb <- createWorkbook()
addWorksheet(wb, "sample_size", gridLines = FALSE)
addWorksheet(wb, "ncs_summary", gridLines = FALSE)
addWorksheet(wb, "smds", gridLines = FALSE)
addWorksheet(wb, "covariates", gridLines = FALSE)
addWorksheet(wb, "sys_error", gridLines = FALSE)
addWorksheet(wb, "cal_est", gridLines = FALSE)

writeDataTable(wb, "sample_size", x = ss_df, startCol = 2, startRow = 2, colNames = TRUE, rowNames = TRUE)
writeDataTable(wb, "ncs_summary", x = ncs_summary, startCol = 2, startRow = 2, colNames = TRUE, rowNames = TRUE)
writeDataTable(wb, "smds", x = data.frame(match_smds), startCol = 2, startRow = 2, colNames = TRUE, rowNames = TRUE)
writeDataTable(wb, "smds", x = data.frame(all_smds), startCol = 2, startRow = 15, colNames = TRUE, rowNames = TRUE)
writeDataTable(wb, "covariates", x = data.frame(num_covs), startCol = 2, startRow = 2, colNames = TRUE, rowNames = TRUE)
writeDataTable(wb, "sys_error", x = data.frame(exp_abs_sys_err), startCol = 2, startRow = 2, colNames = TRUE, rowNames = TRUE)
writeDataTable(wb, "cal_est", x = data.frame(cal_est), startCol = 2, startRow = 2, colNames = TRUE, rowNames = TRUE)

saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
rm(wb)


# ==================================================================================================================
# Objects
# ==================================================================================================================

# smds.xlsx
# ncs.xlsx

# plts: ncs plots; list from 1:9
# p_ncs: calibrated p-values, rr and 95% ci for ncs; skips imbalanced cohorts
# p_hoi: calibrated p-values, rr and 95% ci for outcome of interest; skips imbalanced cohorts
# nulls: null objects; list from 1:9; skips imbalanced cohorts

# num_imbal_cov: list of vectors of imbalanced covariates for each cohort; list from 1:9
# num_imbal_match_cov <- list of vectors of imbalanced matching covariates for each cohort; list from 1:9
# num_imbal_cov_pre: vector of imbalanced covariates before matching for each cohort
# num_cov_candidates: number of pre-match covariate candidates for each cohort
# num_match_cov: num match covs for PSM for each cohort
# num_all_cov_list: num all covs after matching for each cohort; list from 1:9
# smd_summary_list: summary statistics before/after match for match/all vars for all cohorts; list from 1:9

# no_matches: missing matches for CM
# unbal_matches: unbalanced matches for CM

# sample_sizes: list of vector with sample sizes for all balanced cohorts, set to 0 where no matches
# ss_df: summary statistics on sample sizes

# match_smds: summary of smds for matching covariates
# all_smds: summary of smds for all covariates

# ncs_summary: summary measures relevant to negative controls

