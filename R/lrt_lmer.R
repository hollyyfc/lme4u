#' Likelihood Ratio Test for Linear Mixed-Effects Model
#'
#' Hypothesis testing on a selected fixed or random effect in a fitted lmer model using
#' the Likelihood Ratio Test. Currently only support models fitted with [lme4::lmer()]
#' that contain fixed effects and a single random structure (i.e., no nested `/`,
#' double vertical `||`, or multi-level random effects).
#'
#' @section Methods:
#'
#' The hypothesis test evaluates whether the selected effect provides meaningful variance
#' explanation in the linear mixed-effects model:
#' * Null hypothesis H0: The selected variable has no valid effect (i.e., its coefficient
#' is 0 for fixed effects, or its variance component is 0 for random effects)
#' * Alternative hypothesis H1: The selected variable significantly contributes
#' to model variability
#'
#' To test this, the function removes the target variable, refits a suitable, reduced model
#' without it, and compares the fit of the full and reduced models using a Chi-square test
#' based on log-likelihood differences and calculated degrees of freedom (depending on
#' the target type).
#'
#' @section Reduced Model:
#'
#' The reduced model is constructed differently based on the type of effect being tested:
#' * Fixed effect: The target fixed variable is removed from the original `lmer()` formula,
#' and the model is refitted.
#' * Random intercept: The full model is reconstructed by modifying the original `lmer()` call
#' to retain only the random intercept (ignoring any random slopes). The reduced model is a
#' standard `lm()` model containing only the fixed effects, and the likelihood ratio test
#' follows a mixture chi-square distribution for p-value computation.
#' * Random slope: The target variable is only removed from the random slope structure.
#' The reduced model remains an `lmer()` model, and the likelihood ratio test follows a mixture
#' chi-square distribution for p-value computation.
#'
#' Any `lmer()` models fitted within this function, including the input model provided by the user,
#' will be refitted with `REML = FALSE` to ensure accurate hypothesis testing.
#'
#'
#' @param model A fitted model object from [lme4::lmer()]. Currently not supporting formula
#'   that contains shorthand `.`, such as `y ~ .`, or more than one random structure.
#' @param target A character value specifying the variable to be tested. Must match either
#'   a fixed effect or a variable within a random effect structure.
#' @param type A character string indicating whether `target` is a `"fixed"` or `"random"` effect.
#' @param data A data frame used to fit `model`.
#' @param has_interaction A logical value, `TRUE` or `FALSE` (the default), indicating whether
#'   `model` contains interaction between fixed effects. If `TRUE`, all terms containing `target`
#'   will be removed to form the reduced model.
#' @param verbose A logical value, `TRUE` (the default) or `FALSE`, controlling whether detailed
#'   test summary are printed to the console.
#'
#' @returns A named list containing key test statistics:
#'   * `logLik`: Numeric. The log-likelihood difference between the full and reduced model.
#'   * `df`: Integer. The degrees of freedom difference between the two models, representing
#'   the number of parameters removed.
#'   * `pvalue`: Numeric. The p-value from the Chi-square test.
#'
#' @examples
#' library(lme4)
#' model <- lmer(math ~ math_old + cltype + (cltype | school_id), data = star)
#'
#' # Fixed effect testing
#' fix_result <- lrt_lmer(model, target = "math_old", type = "fixed", data = star)
#'
#' fix_result
#'
#' # Random intercept testing
#' random_result1 <- lrt_lmer(model, target = "school_id", type = "random", data = star)
#'
#' # Random slope testing
#' random_result2 <- lrt_lmer(model, target = "cltype", type = "random", data = star)
#'
#' @export
lrt_lmer <- function(model, target, type, data, has_interaction = FALSE, verbose = TRUE) {

  # Basic Error Handling ////////////////////////////////////////
  # Check if the input is an lmer model
  if (!inherits(model, "lmerMod")) {
    stop("The input model must be a fitted lmer() object from the lme4 package.")
  }
  # Check no missing input for the required fields
  if (missing(target)) stop("You must supply `target`.")
  if (missing(type)) stop("You must specify `type = 'fixed'` or `type = 'random'`.")
  if (missing(data)) stop("You must supply the `data` used to fit the model.")
  # Check the type argument is valid
  if (!type %in% c("fixed", "random")) {
    stop("Invalid value for 'type'. Choose from 'fixed', or 'random'.")
  }
  # Extract formula and avoid shorthand
  full_formula <- stats::formula(model)
  if (grepl("~\\s*\\.(\\s|\\+|\\-|\\)|$)", deparse(full_formula))) {
    stop("Shorthand formulas using '.' are not supported. Please specify all fixed effect terms explicitly.")
  }

  # Extract effects
  all_terms <- attr(stats::terms(full_formula), "term.labels")
  fixed_terms <- all_terms[!grepl("\\|", all_terms)] # fixed effects (no intercept)
  random_structure <- lme4::findbars(full_formula)   # original random structures

  # Check if only one random effect is present
  if (length(random_structure) > 1) {
    stop("The test currently only support models consisting of a single random term.")
  }
  # Extract random intercept and slopes after above is met
  random_expr <- deparse(random_structure[[1]])
  split_term <- strsplit(random_expr, "\\|")[[1]]
  slope_part <- trimws(split_term[1])
  slope_var_raw <- trimws(unlist(strsplit(slope_part, "\\+")))
  slope_var <- slope_var_raw[slope_var_raw != "1"] # random slope (remove 1)
  group_var <- trimws(split_term[2])               # random intercept

  # Check if target exists in the model formula
  if (type == "fixed" && !(target %in% fixed_terms)) {
    stop("The specified fixed effect variable is not in the model.")
  }
  if (type == "random" && !(target %in% c(slope_var, group_var))) {
    stop("The specified random effect variable is not in the model.")
  }

  # Run hypothesis test ////////////////////////////////////////
  if (type == "fixed") {
    # Refit both models with REML=F
    reduced_formula <- stats::as.formula(fixed_test(model, target, has_interaction))
    reduced_model <- lme4::lmer(reduced_formula, data = data, REML = FALSE)
    full_model <- lme4::lmer(full_formula, data = data, REML = FALSE)
    # Obtain key testing statistics with anova
    test_result <- stats::anova(reduced_model, full_model)
    # Extract stats
    logLik_diff <- test_result$Chisq[2]
    df_diff <- test_result$Df[2]
    p_value <- test_result$`Pr(>Chisq)`[2]
  } else {
    if (target %in% group_var) { # random intercept
      # Refit both models
      both_text <- randint_test(model)
      reduced_formula <- stats::as.formula(both_text[[1]])
      reduced_model <- stats::lm(reduced_formula, data = data)
      full_formula <- stats::as.formula(both_text[[2]])
      full_model <- lme4::lmer(full_formula, REML = FALSE, data = data)
      # Calculate stats
      logLik_diff <- 2 * (stats::logLik(full_model) - stats::logLik(reduced_model))
      df_diff <- 1
      p_value <- ifelse(logLik_diff > 0, 0.5 * (1 - stats::pchisq(logLik_diff, df_diff)), NA)
      type <- paste(type, "intercept")
    } else { # random slope
      # Refit both models with REML=F
      reduced_formula <- stats::as.formula(randslp_test(model, target))
      reduced_model <- lme4::lmer(reduced_formula, data = data, REML = FALSE)
      full_model <- lme4::lmer(full_formula, data = data, REML = FALSE)
      # Obtain key testing statistics with anova
      test_result <- stats::anova(reduced_model, full_model)
      # Extract stats
      logLik_diff <- test_result$Chisq[2]
      df_diff <- test_result$Df[2]
      p_value <- 0.5 * ( (1-stats::pchisq(logLik_diff, df_diff-1)) + (1-stats::pchisq(logLik_diff, df_diff)) )
      type <- paste(type, "slope")
    }
  }

  # Interpretation message ////////////////////////////////////////
  if (!is.na(p_value)) {
    interpretation <- paste0(
      "Since p = ", format.pval(p_value, digits = 4), ", under alpha level of 0.05, ",
      ifelse(p_value < 0.05,
             "we reject the null hypothesis. The test suggests that the ",
             "we cannot reject the null hypothesis. The test suggests that the "),
      type, " effect of ", target,
      ifelse(p_value < 0.05,
             " is statistically significant and contributes to the model.",
             " may not significantly contribute to the model.")
    )
  } else {
    interpretation <- paste0(
      "The p value is unavailable. No conclusion is drawn."
    )
  }

  # Output verbose message ////////////////////////////////////////
  if (verbose) {
    output <- paste0(
      "Likelihood Ratio Test for '", target, "' (", type, " effect)\n",
      "Full Model: ", deparse(full_formula), "\n",
      "Reduced Model: ", deparse(reduced_formula), "\n",
      "Test Statistics:\n",
      "- Log-Likelihood Difference: ", round(logLik_diff, 4), "\n",
      "- Degrees of Freedom: ", df_diff, " (Difference in estimated parameters)\n",
      "- P-Value: ", format.pval(p_value, digits = 4), " (Chi-square test)\n",
      interpretation
    )
    cat(output)
  }

  # Store stats in attributes
  result <- list(logLik = logLik_diff, df = df_diff, pvalue = p_value)
  return(result)
}

# ---------------------------- HELPER FUNCTIONS (Internal) -------------------------------- #

# Creating fixed effect reduced formula //////////////////////
fixed_test <- function(model, target, has_interaction) {

  # Extract model information
  full_formula <- stats::formula(model)
  all_terms <- attr(stats::terms(full_formula), "term.labels")
  fixed_terms <- all_terms[!grepl("\\|", all_terms)]
  random_structure <- lme4::findbars(full_formula)
  random_expr <- deparse(random_structure[[1]])

  # Construct reduced formula text
  if (!has_interaction) { # no interaction
    reduced_fixed_terms <- setdiff(fixed_terms, target)
  } else {  # yes interaction
    is_interaction <- grepl(":", target)
    if (is_interaction) {
      # Remove interaction term itself only
      reduced_fixed_terms <- setdiff(fixed_terms, target)
    } else {
      # Remove all terms consisting of target
      reduced_fixed_terms <- fixed_terms[!grepl(paste0("(^|:)", target, "(:|$)"), fixed_terms)]
    }
  }
  # Suffice formula with 1 if no fixed effect left
  if (length(reduced_fixed_terms) == 0) {
    reduced_fixed_terms <- "1"
  }

  # Create the reduced model
  reduced_fixed_text <- stats::reformulate(reduced_fixed_terms, response = as.character(full_formula[[2]]))
  reduced_text <- paste(deparse(reduced_fixed_text), paste0("(", random_expr, ")"), sep = " + ")
  return(reduced_text)
}


# Creating random intercept reduced formula //////////////////////
randint_test <- function(model) {

  full_formula <- stats::formula(model)
  all_terms <- attr(stats::terms(full_formula), "term.labels")
  fixed_terms <- all_terms[!grepl("\\|", all_terms)]
  random_structure <- lme4::findbars(full_formula)
  random_expr <- deparse(random_structure[[1]])

  # Construct no random effect reduced model
  reduced_formula <- stats::reformulate(fixed_terms, response = as.character(full_formula[[2]]))
  reduced_formula_text <- deparse(reduced_formula)
  # Construct random intercept-only full model
  random_expr_intonly <- sub(".*\\|", "1 |", random_expr)
  full_formula_text <- paste(reduced_formula_text, paste0("(", random_expr_intonly, ")"), sep = " + ")

  return(list(reduced_formula_text, full_formula_text))
}


# Creating random slope reduced formula //////////////////////
randslp_test <- function(model, target) {

  full_formula <- stats::formula(model)
  all_terms <- attr(stats::terms(full_formula), "term.labels")
  fixed_terms <- all_terms[!grepl("\\|", all_terms)]
  random_structure <- lme4::findbars(full_formula)
  random_expr <- deparse(random_structure[[1]])
  split_term <- strsplit(random_expr, "\\|")[[1]]
  slope_part <- trimws(split_term[1])
  slope_var_raw <- trimws(unlist(strsplit(slope_part, "\\+")))
  slope_var <- slope_var_raw[slope_var_raw != "1"]
  group_var <- trimws(split_term[2])

  # Fixed-only base formula
  reduced_fixed_text <- deparse(stats::reformulate(fixed_terms, response = as.character(full_formula[[2]])))
  # Wrangle random slope terms
  reduced_slope_terms <- setdiff(slope_var, target)
  new_slope <- ifelse(length(reduced_slope_terms) > 0,
                      paste(reduced_slope_terms, collapse = " + "),
                      "1")
  new_random_expr <- paste(new_slope, "|", group_var)
  # Construct reduced random slope formula
  reduced_text <- paste(reduced_fixed_text, paste0("(", new_random_expr, ")"), sep = " + ")
  return(reduced_text)
}












