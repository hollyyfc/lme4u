######## Reject non-lmer objects ########
test_that("lrt_lmer() rejects lm objects", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  lm_model <- lm(mpg ~ hp, data = mtcars)
  expect_error(lrt_lmer(lm_model, target = "hp", type = "fixed", data = mtcars),
               regexp = "The input model must be a fitted lmer() object from the lme4 package.",
               fixed = TRUE)
})

test_that("lrt_lmer() rejects glmer objects", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  df_glmer <- data.frame(
    y = c(0, 1, 0, 1, 0, 1, 1, 0, 1, 0),
    x = c(1, 2, 1, 3, 2, 4, 3, 1, 2, 3),
    group = rep(c("A", "B"), each = 5)
  )

  glme_model <- suppressMessages(lme4::glmer(y ~ x + (1 | group),
                                             data = df_glmer,
                                             family = binomial(link = "logit")))
  expect_error(lrt_lmer(glme_model, target = "x", type = "fixed", data = df_glmer),
               regexp = "The input model must be a fitted lmer() object from the lme4 package.",
               fixed = TRUE)
})

######## No input error ########
test_that("lrt_lmer() errors when no model is provided", {
  expect_error(lrt_lmer(), "argument \"model\" is missing, with no default")
})

test_that("lrt_lmer() errors when required arguments are missing", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  df <- data.frame(y = rnorm(10), x = rnorm(10), g = rep(1:5, each = 2))
  model <- lme4::lmer(y ~ x + (1 | g), data = df)

  expect_error(lrt_lmer(model), "You must supply `target`.")
  expect_error(lrt_lmer(model, target = "x"), "You must specify `type = 'fixed'` or `type = 'random'`.")
  expect_error(lrt_lmer(model, type = "fixed"), "You must supply `target`.")
  expect_error(lrt_lmer(model, target = "x", type = "fixed"), "You must supply the `data` used to fit the model.")
})

######## Detect and reject shorthand formulas ########
test_that("lrt_lmer() detects shorthand notation", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  mydata <- mtcars
  mydata$cyl <- as.factor(mydata$cyl)
  model <- suppressMessages(lme4::lmer(mpg ~ . + (1 | cyl), data = mydata))
  expect_error(lrt_lmer(model, target = "disp", type = "fixed", data = mydata),
               "Shorthand formulas using '.' are not supported. Please specify all fixed effect terms explicitly.")
})

######## Detect if input values are valid ########
test_that("lrt_lmer() errors when required arguments are valid", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  model <- suppressMessages(lme4::lmer(math ~ math_old + cltype + (1 | school_id), data = star))

  expect_error(lrt_lmer(model, target = "math1", type = "fixed", data = star),
               "The specified fixed effect variable is not in the model.")
  expect_error(lrt_lmer(model, target = "math_old", type = "random", data = star),
               "The specified random effect variable is not in the model.")
  expect_error(lrt_lmer(model, target = "schoo_id", type = "fixed", data = star),
               "The specified fixed effect variable is not in the model.")
  expect_error(lrt_lmer(model, target = "math_old", type = "fixef", data = star),
               "Invalid value for 'type'. Choose from 'fixed', or 'random'.")
})

######## Detect if valid model structure ########
test_that("lrt_lmer() errors when model structure is not valid", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  model1 <- suppressWarnings(lme4::lmer(math ~ math_old + cltype + (1 | school_id) + (1 | system_id), data = star))
  model2 <- suppressWarnings(lme4::lmer(math ~ math_old + cltype + (1 | system_id/school_id), data = star))
  model3 <- suppressWarnings(lme4::lmer(math ~ math_old + cltype + (cltype || school_id), data = star))
  model4 <- lme4::lmer(math ~ 1 + (1 | school_id), data = star)

  expect_error(lrt_lmer(model1, target = "math_old", type = "fixed", data = star),
               "The test currently only support models consisting of a single random term.")
  expect_error(lrt_lmer(model2, target = "math_old", type = "fixed", data = star),
               "The test currently only support models consisting of a single random term.")
  expect_error(lrt_lmer(model3, target = "math_old", type = "fixed", data = star),
               "The test currently only support models consisting of a single random term.")
  expect_error(lrt_lmer(model4, target = "1", type = "fixed", data = star),
               "The specified fixed effect variable is not in the model.")
})

######## Test returned values: Fixed ########
test_that("lrt_lmer() correctly returns the testing statistics for fixed effects", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  full_model <- lme4::lmer(math ~ math_old + cltype + (cltype | school_id), data = star, REML = F)
  reduced_model <- lme4::lmer(math ~ 1 + cltype + (cltype | school_id), data = star, REML = F)
  test_manual <- stats::anova(reduced_model, full_model)
  logLik_diff_manual <- test_manual$Chisq[2]
  df_diff_manual <- test_manual$Df[2]
  p_value_manual <- test_manual$`Pr(>Chisq)`[2]

  test_lrt <- lrt_lmer(full_model, target = "math_old", type = "fixed", data = star, verbose = F)
  expect_equal(test_lrt$logLik, logLik_diff_manual)
  expect_equal(test_lrt$df, df_diff_manual)
  expect_equal(test_lrt$pvalue, p_value_manual)
})

test_that("lrt_lmer() correctly returns the testing statistics for fixed effects with interactions", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  full_model <- lme4::lmer(math ~ math_old*cltype + (cltype | school_id), data = star, REML = F)
  reduced_model <- lme4::lmer(math ~ 1 + cltype + (cltype | school_id), data = star, REML = F)
  test_manual <- stats::anova(reduced_model, full_model)
  logLik_diff_manual <- test_manual$Chisq[2]
  df_diff_manual <- test_manual$Df[2]
  p_value_manual <- test_manual$`Pr(>Chisq)`[2]

  test_lrt <- lrt_lmer(full_model, target = "math_old", type = "fixed", data = star, has_interaction=T, verbose = F)
  expect_equal(test_lrt$logLik, logLik_diff_manual)
  expect_equal(test_lrt$df, df_diff_manual)
  expect_equal(test_lrt$pvalue, p_value_manual)
})

######## Test returned values: Random Intercept ########
test_that("lrt_lmer() correctly returns the testing statistics for random intercept effects", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  full_model <- lme4::lmer(math ~ math_old + cltype + (cltype | school_id), data = star)

  full_model_new <- lme4::lmer(math ~ math_old + cltype + (1 | school_id), data = star, REML = F)
  reduced_model <- stats::lm(math ~ math_old + cltype, data = star)
  logLik_diff_manual <- 2 * (stats::logLik(full_model_new) - stats::logLik(reduced_model))
  df_diff_manual <- 1
  p_value_manual <- ifelse(logLik_diff_manual > 0,
                           0.5 * (1 - stats::pchisq(logLik_diff_manual, df_diff_manual)), NA)

  test_lrt <- lrt_lmer(full_model, target = "school_id", type = "random", data = star, verbose = F)
  expect_equal(test_lrt$logLik, logLik_diff_manual)
  expect_equal(test_lrt$df, df_diff_manual)
  expect_equal(test_lrt$pvalue, p_value_manual)
})

######## Test returned values: Random Slope ########
test_that("lrt_lmer() correctly returns the testing statistics for random slope effects", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE), "lme4 package required but not installed.")

  full_model <- lme4::lmer(math ~ math_old + cltype + (cltype | school_id), data = star, REML = F)
  reduced_model <- lme4::lmer(math ~ math_old + cltype + (1 | school_id), data = star, REML = F)
  test_manual <- stats::anova(reduced_model, full_model)
  logLik_diff_manual <- test_manual$Chisq[2]
  df_diff_manual <- test_manual$Df[2]
  p_value_manual <- 0.5 * ( (1-stats::pchisq(logLik_diff_manual, df_diff_manual-1)) +
                              (1-stats::pchisq(logLik_diff_manual, df_diff_manual)) )

  test_lrt <- lrt_lmer(full_model, target = "cltype", type = "random", data = star, verbose = F)
  expect_equal(test_lrt$logLik, logLik_diff_manual)
  expect_equal(test_lrt$df, df_diff_manual)
  expect_equal(test_lrt$pvalue, p_value_manual)
})
