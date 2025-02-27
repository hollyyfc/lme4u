library(AER)
library(dplyr)
library(readr)

utils::data("STAR")

star <- STAR %>%
  # Select relevant variables for only 2nd and 3rd graders
  select(gender, ethnicity, star2,
         star3, read2, read3, math2,
         math3,lunch2, lunch3, school2,
         school3, degree2, degree3,
         experience2, experience3, system2,
         system3, schoolid2, schoolid3) %>%
  # Remove missing values
  tidyr::drop_na() %>%
  # Keep observations that attend one school consistently
  filter(schoolid2 == schoolid3) %>%
  # Remove 2nd grade details
  select(-c(school2, system2, schoolid2, star2,
            lunch2, degree2, experience2)) %>%
  # Rename variables and organize column positions
  rename(cltype = star3,
         read_old = read2, read = read3,
         math_old = math2, math = math3,
         lunch = lunch3,
         sctype = school3,
         tdegree = degree3,
         tyear = experience3,
         system_id = system3,
         school_id = schoolid3) %>%
  relocate(school_id, system_id, sctype) %>%
  relocate(tdegree, tyear, .after = cltype) %>%
  relocate(lunch, .after = tyear)

write_csv(star, "data-raw/star.csv")
usethis::use_data(star, overwrite = TRUE)
