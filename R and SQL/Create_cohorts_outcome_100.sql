
USE @cdmDatabaseSchema;

INSERT INTO @cohortDatabaseSchema.@cohortOutcomeTable (
    cohort_definition_id,
    cohort_start_date,
    cohort_end_date,
    subject_id
    )
SELECT 3, cohort_start_date, cohort_end_date, subject_id
FROM ohdsi_results.cohort
WHERE cohort_definition_id = @cohort_id_3;