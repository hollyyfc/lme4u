######## Reject non-lmer objects ########
test_that("residuals functions reject lm objects", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  lm_model <- lm(mpg ~ hp, data = mtcars)
  expect_error(res_norm(lm_model),
               regexp = "The input model must be a fitted lmer() object from the lme4 package.",
               fixed = TRUE)
  expect_error(res_fit(lm_model),
               regexp = "The input model must be a fitted lmer() object from the lme4 package.",
               fixed = TRUE)
  expect_error(res_box(lm_model, "group"),
               regexp = "The input model must be a fitted lmer() object from the lme4 package.",
               fixed = TRUE)
})

test_that("residuals functions reject glmer objects", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  df_glmer <- data.frame(
    y = c(0, 1, 0, 1, 0, 1, 1, 0, 1, 0),
    x = c(1, 2, 1, 3, 2, 4, 3, 1, 2, 3),
    group = rep(c("A", "B"), each = 5)
  )

  glme_model <- suppressMessages(lme4::glmer(y ~ x + (1 | group),
                                             data = df_glmer,
                                             family = binomial(link = "logit")))
  expect_error(res_norm(glme_model),
               regexp = "The input model must be a fitted lmer() object from the lme4 package.",
               fixed = TRUE)
  expect_error(res_fit(glme_model),
               regexp = "The input model must be a fitted lmer() object from the lme4 package.",
               fixed = TRUE)
  expect_error(res_box(glme_model, "group"),
               regexp = "The input model must be a fitted lmer() object from the lme4 package.",
               fixed = TRUE)
})

######## No input error ########
test_that("residuals functions error when no model is provided", {
  expect_error(res_norm(), "argument \"model\" is missing, with no default")
  expect_error(res_fit(), "argument \"model\" is missing, with no default")
  expect_error(res_box(), "argument \"model\" is missing, with no default")
})

test_that("res_box() errors when group_var argument is missing", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  df <- data.frame(y = rnorm(10), x = rnorm(10), g = rep(1:5, each = 2))
  model <- lme4::lmer(y ~ x + (1 | g), data = df)
  expect_error(res_box(model), "argument \"group_var\" is missing, with no default")
})

######## Detect and reject shorthand formulas ########
test_that("residuals functions detect shorthand notation", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  mydata <- mtcars
  mydata$cyl <- as.factor(mydata$cyl)
  model <- suppressMessages(lme4::lmer(mpg ~ . + (1 | cyl), data = mydata))
  expect_error(res_norm(model),
               "Shorthand formulas using '.' are not supported. Please specify all fixed effect terms explicitly.")
  expect_error(res_fit(model),
               "Shorthand formulas using '.' are not supported. Please specify all fixed effect terms explicitly.")
  expect_error(res_box(model, "cyl"),
               "Shorthand formulas using '.' are not supported. Please specify all fixed effect terms explicitly.")
})

######## Detect if input values are valid ########
test_that("res_box() errors when group_var argument is invalid", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  model <- suppressMessages(lme4::lmer(math ~ math_old + cltype + (1 | school_id), data = star))

  expect_error(res_box(model, "NotAVariable"),
               regexp = "not a valid variable name in the model\'s data")
  expect_error(res_box(model, "cltype"),
               regexp = "is not part of a random intercept grouping structure")
})

######## Test returned values: ggplot objects ########
test_that("residuals functions return ggplots for valid input", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")
  skip_if_not(requireNamespace("ggplot2", quietly = TRUE), "ggplot2 package required but not installed.")

  model <- suppressMessages(lme4::lmer(math ~ math_old + cltype + (cltype | system_id/school_id), data = star))
  p1 <- res_norm(model)
  p2 <- res_fit(model)
  p3_1 <- res_box(model, "school_id")
  p3_2 <- res_box(model, "system_id")

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3_1, "ggplot")
  expect_s3_class(p3_2, "ggplot")
})
