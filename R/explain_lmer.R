utils::globalVariables(c("term", "estimate", "interpretation", "."))
#' Interpret Linear Mixed-Effects Model
#'
#' Provides a user-friendly interpretation of a fitted lmer model's fixed and
#' random effects.
#'
#' @param model A fitted model object from `lme4::lmer()`.
#' @param details Character. Specify which details to return: "general",
#'   "fixed", "random", or "full". Defaults to "general".
#'
#' @returns A character string with the interpretation of the model.
#'
#' @examples
#' library(lme4)
#' model <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' explain_lmer(model)
#' @export
explain_lmer <- function(model, details = "general") {

  # Check if the input is an lmer model
  if (!inherits(model, "merMod")) {
    stop("The input model must be a fitted lmer() object from the lme4 package.")
  }

  # Extract basic model information
  model_formula <- stats::formula(model)
  grouping_vars <- names(lme4::getME(model, "flist"))
  fixed_effects <- lme4::fixef(model) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "term") %>%
    dplyr::rename(estimate = ".")
  random_effects <- lme4::ranef(model)
  random_varcorr <- lme4::VarCorr(model)

  # General model structure interpretation
  general_text <- paste0(
    "This linear mixed-effects model is specified with the following formula: ",
    deparse(model_formula), ". The model accounts for variability across grouping variable(s): ",
    paste(grouping_vars, collapse = ", "), "."
  )

  # Fixed effects interpretation using dplyr
  fixed_text <- "\n\nFixed Effects Interpretation:\n"
  fixed_effects %>%
    dplyr::mutate(
      interpretation = paste0(
        " - The effect of ", term, ": an estimated change of ", round(estimate, 3),
        " units in the response variable for a one-unit increase in ", term, "."
      )
    ) %>%
    dplyr::pull(interpretation) %>%
    paste(collapse = "\n") %>%
    {fixed_text <<- paste0(fixed_text, .)}

  # Variance-Covariance structure of random effects
  random_var_text <- "\n\nRandom Effects Variance-Covariance Structure:\n"
  for (i in seq_along(random_varcorr)) {
    grp_var <- random_varcorr[[i]]
    grp_name <- attr(random_varcorr, "names")[i]
    variances <- diag(grp_var) %>% round(3) %>% paste(collapse = ", ")
    random_var_text <- paste0(random_var_text, " - Group: ", grp_name, ", Variances: ", variances, "\n")
  }

  # Return interpretation based on user selection
  if (details == "general") {
    return(general_text)
  } else if (details == "fixed") {
    return(paste(general_text, fixed_text))
  } else if (details == "random") {
    return(paste(general_text, # random_text,
                 random_var_text))
  } else if (details == "full") {
    return(paste(general_text, fixed_text, # random_text,
                 random_var_text))
  } else {
    stop("Invalid value for 'details'. Choose from 'general', 'fixed', 'random', or 'full'.")
  }

}
