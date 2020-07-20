
INSERT INTO @cohortDatabaseSchema.@cohortTable (
    cohort_definition_id,
    cohort_start_date,
    cohort_end_date,
    subject_id
    )
SELECT @t_cohort_id, cohort_start_date, cohort_end_date, subject_id
FROM @cohortDatabaseSchema.@cohortTable
WHERE subject_id IN (@t_subjectIds) AND
	cohort_definition_id = 1;


INSERT INTO @cohortDatabaseSchema.@cohortTable (
    cohort_definition_id,
    cohort_start_date,
    cohort_end_date,
    subject_id
    )
SELECT @c_cohort_id, cohort_start_date, cohort_end_date, subject_id
FROM @cohortDatabaseSchema.@cohortTable
WHERE subject_id IN (@c_subjectIds) AND
	cohort_definition_id = 2;


