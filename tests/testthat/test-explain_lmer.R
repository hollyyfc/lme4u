######## Reject non-lmer objects ########
test_that("explain_lmer() rejects lm objects", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  lm_model <- lm(mpg ~ hp, data = mtcars)
  expect_error(explain_lmer(lm_model),
               regexp = "The input model must be a fitted lmer() object from the lme4 package.",
               fixed = TRUE)
})
test_that("explain_lmer() rejects glmer objects", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  df_glmer <- data.frame(
    y = c(0, 1, 0, 1, 0, 1, 1, 0, 1, 0),
    x = c(1, 2, 1, 3, 2, 4, 3, 1, 2, 3),
    group = rep(c("A", "B"), each = 5)
  )

  glme_model <- suppressMessages(lme4::glmer(y ~ x + (1 | group),
                                             data = df_glmer,
                                             family = binomial(link = "logit")))
  expect_error(explain_lmer(glme_model),
               regexp = "The input model must be a fitted lmer() object from the lme4 package.",
               fixed = TRUE)
})
######## No input error ########
test_that("explain_lmer() errors when no input is provided", {
  expect_error(explain_lmer(), "argument \"model\" is missing, with no default")
})
######## Detect and reject shorthand formulas ########
test_that("explain_lmer() detects shorthand notation", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  mydata <- mtcars
  mydata$cyl <- as.factor(mydata$cyl)
  model <- lme4::lmer(mpg ~ . + (1 | cyl), data = mydata)
  expect_error(explain_lmer(model),
               "Shorthand formulas using '.' are not supported. Please specify all fixed effect terms explicitly.")
})
######## Verify details argument ########
test_that("explain_lmer() rejects invalid details argument", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  df <- data.frame(y = rnorm(10), x = rnorm(10), g = rep(1:5, each = 2))
  model <- lme4::lmer(y ~ x + (1 | g), data = df)
  expect_error(explain_lmer(model, details = "invalid_option"),
               "Invalid value for 'details'. Choose from 'general', 'fixed', or 'random'.")
})


