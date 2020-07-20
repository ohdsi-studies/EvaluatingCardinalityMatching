Comparison of Cardinality Matching and Propensity Score Matching
================================================================

<img src="https://img.shields.io/badge/Study%20Status-Results%20Available-yellow.svg" alt="Study Status: Results Available"> 

- Analytics use case(s): **Population-Level Estimation** 
- Study type: **Methods Research** 
- Tags: **Cardinality Matching**
- Study lead: **Stephen Fortin**
- Study lead forums tag: **[stephenfortin](https://forums.ohdsi.org/u/stephenfortin/)**
- Study start date: **March 1, 2020**
- Study end date: **-**
- Protocol: **[Protocol PDF]( https://github.com/ohdsi-studies/EvaluatingCardinalityMatching/tree/master/Protocol)**
- Publications: **-**
- Results explorer: **-**

This study aims to evaluate the performance of cardinality matching and large-scale propensity score matching at progressively smaller sample sizes and more stringent parameter settings among new users of ACEI vs. thiazide or thiazide-like diuretic monotherapy applying the framework set forth in the LEGEND-HTN study. The performance of either matching technique is evaluated in terms of post-match sample size, matching covariate and covariate candidate balance and residual confounding.

How to run
==========

1. Make sure that you have Java installed. If you don't have Java already installed on your computed (on most computers it already is installed), go to java.com to get the latest version. (If you have trouble building with rJava below, be sure on Windows that your Path variable includes the path to jvm.dll (Windows Button --> type "path" --> Edit Environmental Variables --> Edit PATH variable, add to end ;C:/Program Files/Java/jre/bin/server) or wherever it is on your system).

2. The study will require a Gurobi license. Make sure Gurobi is installed on you local machine to perform cardinality matching. In R, use the following code to install Gurobi from your local machine:

```r
if (!require('gurobi')){install.packages("/gurobi_9.0-1.zip", repos = NULL)} # Update gurobi version and file path as necesssary
```

3. In R, use the following code to install the study package and its dependencies:

```r
install.packages("devtools")

library(devtools)
install_github("ohdsi/CohortMethod", ref = "v3.1.2")
install_github("ohdsi/EmpiricalCalibration", ref = "v2.1.0")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(drat, SqlRender, DatabaseConnector, grid, reshape2, dplyr,
               designmatch, caret, rdist, gurobi, caTools, openxlsx,
		  CohortMethod, EmpiricalCalibration)
```
      
4. Once installed, you can execute the study by modifying and using the following code:

```r
# Set working directory
wd <- "C:/myworkingdirectory"
setwd(wd)

# Details for connecting to the server:
connectionDetails <- createConnectionDetails(dbms = "postgresql",
											 user = "joe",
											 password = "secret",
											 server = "myserver")
											 
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
         436785,74464,72404,439935,437092,442306,378424)'''
```

- For details on how to configurecreateConnectionDetails in your environment type this for help:
 ?createConnectionDetails
- cdmDatabaseSchema should specify the schema name where your patient-level data in OMOP CDM format resides. Note that for SQL Server, this should include both the database and schema name, for example 'cdm_data.dbo'.
- oracleTempSchema should be used in Oracle to specify a schema where the user has write privileges for storing temporary tables.
- cmDataFolder (created in the next step) specifies the location where data on the study population is stored

5. Download all of the scripts from the 'R and SQL' folder of this repository (found [here]( https://github.com/ohdsi-studies/EvaluatingCardinalityMatching/tree/master/R%20and%20SQL)) to your working directory.


6. To generate data on the study population, execute the following code:

```r
# Create cohort table
createCohortTable(connectionDetails = connectionDetails,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTable = cohortTable)


# Generate cohorts
sql <- readSql("create_cmDataFolder.sql")
sql <- render(sql,
              cdmDatabaseSchema = strsplit(cdmDatabaseSchema, "[.]")[[1]][1],
              cohortDatabaseSchema = cohortDatabaseSchema,
              cohortTable = cohortTable,
              cohort_id_1 = cohort_id_1,
              cohort_id_2 = cohort_id_2,
              cohort_id_3 = cohort_id_3)
sql <- translate(sql, targetDialect = connectionDetails$dbms)
executeSql(connection, sql)

# Target and comparator ingredient concepts
aceI <- c(1335471,1340128,1341927,1363749,1308216,1310756,1373225, 1331235,1334456,1342439)
thz <- c(1395058,974166,978555,907013)

# Define which types of covariates must be constructed
cs <- createDefaultCovariateSettings(excludedCovariateConceptIds = c(aceI, thz),
                                     addDescendantsToExclude = TRUE)

# Load data
cmData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                cdmDatabaseSchema = cdmDatabaseSchema,
                                oracleTempSchema = oracleTempSchema,
                                targetId = 1,
                                comparatorId = 2,
                                outcomeIds = 3,
                                studyStartDate = "",
                                studyEndDate = "",
                                exposureDatabaseSchema = cohortDatabaseSchema,
                                exposureTable = cohortTable,
                                outcomeDatabaseSchema = cohortDatabaseSchema,
                                outcomeTable = cohortTable,
                                cdmVersion = cdmVersion,
                                firstExposureOnly = FALSE,
                                removeDuplicateSubjects = FALSE,
                                restrictToCommonPeriod = FALSE,
                                washoutPeriod = 365,
                                covariateSettings = cs)

# If necessary, create directory to save data
if (dir.exists(cmDataFolder) == FALSE) {
  dir.create(cmDataFolder)
}

# Save data
saveCohortMethodData(cmData, cmDataFolder)

rm(sql, aceI, thz, cs, cmData)
```

7. To execute the study, run the "generateData_studyPopulation.R" and "generateData_subsampleGroup.R" as follows:

- To execute analyses within study population, run "generateData_studyPopulation.R" with the following parameter settings:

```r
splitRatio <- 1.00
num_cohorts <- 1
```

- To execute analyses within the 10% subsample group, run "generateData_subsampleGroup.R" with the following parameter settings:

```r
splitRatio <- 0.10
num_cohorts <- 5
```

- To execute analyses within the 1% subsample group, run "generateData_subsampleGroup.R" with the following parameter settings:

```r
splitRatio <- 0.01
num_cohorts <- 51
```

- To execute analyses within the 0.5% subsample group, run "generateData_subsampleGroup.R" with the following parameter settings:

```r
splitRatio <- 0.005
num_cohorts <- 107
```

