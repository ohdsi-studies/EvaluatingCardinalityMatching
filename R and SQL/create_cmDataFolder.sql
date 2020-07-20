


USE @cdmDatabaseSchema;

INSERT INTO @cohortDatabaseSchema.@cohortTable (
    cohort_definition_id,
    cohort_start_date,
    cohort_end_date,
    subject_id
    )
SELECT 1, cohort_start_date, cohort_end_date, subject_id
FROM ohdsi_results.cohort
WHERE cohort_definition_id = @cohort_id_1

INSERT INTO @cohortDatabaseSchema.@cohortTable (
    cohort_definition_id,
    cohort_start_date,
    cohort_end_date,
    subject_id
    )
SELECT 2, cohort_start_date, cohort_end_date, subject_id
FROM ohdsi_results.cohort
WHERE cohort_definition_id = @cohort_id_2

INSERT INTO @cohortDatabaseSchema.@cohortTable (
    cohort_definition_id,
    cohort_start_date,
    cohort_end_date,
    subject_id
    )
SELECT 3, cohort_start_date, cohort_end_date, subject_id
FROM ohdsi_results.cohort
WHERE cohort_definition_id = @cohort_id_3


