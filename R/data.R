#' Student-Teacher Achievement Ratio Data
#'
#' A cleaned subset of the Project STAR dataset. This processed version focuses
#' on third-grade test scores (math and reading) and includes key variables
#' for mixed-effect modeling.
#'
#' Project STAR is a large-scale experiment in Tennessee (1980s) studying the
#' effect of class size on student test performance. The original dataset tracked
#' over 7,000 students across 79 schools from kindergarten to third grade,
#' in which they were randomly assigned into one of three interventions:
#' small class (13 to 17 students per teacher), regular class (22 to 25 students
#' per teacher), and regular-with-aide class (22 to 25 students with a full-time
#' teacher's aide). The test score data analyzed in this chapter are the sum of
#' the scores on the math and reading portion of the Stanford Achievement Test.
#'
#' This processed version focuses on student performance in third grade, ensuring a
#' hierarchical structure where students are nested within schools. It includes a
#' subset of key variables related to student demographics, prior-year (2nd grade)
#' and current-year (3rd grade) test scores, class assignment, teacher qualifications,
#' and school-level identifiers. All students in this dataset have been controlled as
#' attending the same school in both 2nd and 3rd grades. This dataset is structured to
#' facilitate mixed-effects modeling, making it well-suited for evaluating school effects
#' and treatment impacts.
#'
#' @format A data frame with 4,192 rows and 13 columns:
#' \describe{
#'   \item{school_id}{Factor indicating unique school ID.}
#'   \item{system_id}{Factor indicating school system ID.}
#'   \item{sctype}{Factor indicating school type: `"inner-city"`, `"suburban"`, `"rural"`, or `"urban"`.}
#'   \item{gender}{Factor indicating student's gender: `"female"` or `"male"`.}
#'   \item{ethnicity}{Factor indicating student's ethnicity: `"cauc"` (Caucasian), `"afam"` (African-American),
#'   `"asian"` (Asian), `"hispanic"` (Hispanic), `"amindian"` (American-Indian), or `"other"`.}
#'   \item{cltype}{Factor indicating student's class type in 3rd grade: `"small"`, `"regualr"`,
#'   or `"regular-with-aide"`.}
#'   \item{tdegree}{Factor indicating highest degree of 3rd grade class teacher: `"bachelor"`, `"master"`,
#'   or `"specialist"`.}
#'   \item{tyear}{Integer years of teacher's total teaching experience in 3rd grade.}
#'   \item{lunch}{Factor indicating whether the student qualified for free lunch in 3rd grade:
#'   `"free"` or `"non-free"`.}
#'   \item{read_old}{Total reading scaled score in 2nd grade.}
#'   \item{read}{Total reading scaled score in 3rd grade.}
#'   \item{math_old}{Total math scaled score in 2nd grade.}
#'   \item{math}{Total math scaled score in 3rd grade.}
#' }
#' @source <https://search.r-project.org/CRAN/refmans/AER/html/STAR.html>
#' @references
#' Stock, J.H. and Watson, M.W. (2007). _Introduction to Econometrics_, 2nd ed. Boston: Addison Wesley.
#'
#' Data sourced from the [`AER`](https://cran.r-project.org/web/packages/AER/index.html) package.
"star"
