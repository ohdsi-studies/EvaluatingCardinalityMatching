
USE @cdmDatabaseSchema;

INSERT INTO @cohortDatabaseSchema.@cohortOutcomeTable (
    cohort_definition_id,
    cohort_start_date,
    cohort_end_date,
    subject_id
    )
SELECT ancestor_concept_id AS cohort_definition_id,
    condition_start_date AS cohort_start_date,
    condition_end_date AS cohort_end_date,
    condition_occurrence.person_id AS subject_id
FROM condition_occurrence
INNER JOIN visit_occurrence
    ON condition_occurrence.visit_occurrence_id = visit_occurrence.visit_occurrence_id
INNER JOIN concept_ancestor
    ON condition_concept_id = descendant_concept_id
WHERE ancestor_concept_id IN (@ncs)
    AND visit_occurrence.visit_concept_id IN (9201, 9203);
