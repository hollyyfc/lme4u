#' Interpret Linear Mixed-Effects Model
#'
#' Provides a user-friendly interpretation of a fitted lmer model's fixed and
#' random effects.
#'
#' @param model A fitted model object from [lme4::lmer()]. Currently not supporting formula
#'   that contains shorthand `.`, such as `y ~ .`.
#' @param details A character specifying which details to return:
#'   * `"general"` (the default): a high-level summary of the model structure, fixed effects,
#'   and random effects with brief explanations.
#'   * `"fixed"`: a more in-depth interpretation of fixed effects in the model context, with
#'   additional details on individual fixed effect estimates.
#'   * `"random"`: a concise interpretation of each random term (including random intercept and
#'   slope), with insights from estimated variance components.
#'
#' @returns A character string with the interpretation of the model. Additional structured outputs
#'   are stored as attributes and can be accessed as follows:
#'   * for `details = "fixed"`: use `attr(, "fixed_details")` for individual fixed effect
#'   interpretation with model estimates; use `attr(, "fixed_names")` for all fixed effect
#'   variable names.
#'   * for `details = "random"`: use `attr(, "var_details")` for insights regarding estimated
#'   variance components for random effects.
#'
#' @examples
#' library(lme4)
#' model <- lmer(math ~ math_old + cltype + (cltype | school_id), data = star)
#'
#' # For high-level summary
#' explain_lmer(model)
#'
#' # For fixed-effect specific interpretation
#' fix <- explain_lmer(model, "fixed")
#'
#' # Access individual fixed effect interpretation of model estimats
#' attr(fix, "fixed_details")[["(Intercept)"]]
#'
#' # For random-effect specific interpretation
#' random <- explain_lmer(model, "random")
#'
#' # Access variance component insights
#' attr(random, "var_details")
#' @export
explain_lmer <- function(model, details = "general") {

  # Check if the input is an lmer model
  if (!inherits(model, "merMod")) {
    stop("The input model must be a fitted lmer() object from the lme4 package.")
  }
  # Check the details argument is valid
  if (!details %in% c("general", "fixed", "random")) {
    stop("Invalid value for 'details'. Choose from 'general', 'fixed', or 'random'.")
  }
  # Extract formula and detect shorthand
  model_formula <- stats::formula(model)
  if (grepl("~\\s*\\.(\\s|\\+|\\-|\\)|$)", deparse(model_formula))) {
    stop("Shorthand formulas using '.' are not supported. Please specify all fixed effect terms explicitly.")
  }

  # Generate interpretation based on user `details`
  result <- switch(
    details,
    "general" = general_explain(model),
    "fixed" = fixed_explain(model),
    "random" = random_explain(model)
  )
  cat(result)
  invisible(result)
}

# ---------------------------- HELPER FUNCTIONS (Internal) -------------------------------- #

# Construct "general" output text //////////////////////
general_explain <- function(model) {

  # Extract basic model information
  model_formula <- stats::formula(model)
  grouping_vars <- names(lme4::getME(model, "flist"))
  response_var <- as.character(attr(stats::terms(model_formula), "variables"))[[2]]
  all_terms <- attr(stats::terms(model_formula), "term.labels")
  fixed_terms <- all_terms[!grepl("\\|", all_terms)]
  random_structure <- lme4::findbars(model_formula)

  # 1. Explain the model's general purpose
  general_text <- paste0(
    "This linear mixed-effects model examines how ",
    ifelse(length(fixed_terms) > 0,
           paste(fixed_terms, collapse = ", "),
           "the overall mean"),
    " affects ", response_var,
    ", while accounting for group-level variability across ",
    paste(grouping_vars, collapse = ", "), "."
  )

  # 2. Mention fixed effects
  if (length(fixed_terms) == 0) {
    fixed_effect_text <- "The model includes only a fixed intercept, with no additional fixed effect variables. For more detailed interpretations of fixed effects, use `details = \"fixed\"`."
  } else {
    fixed_effect_text <- paste0(
      "The model adjusts for fixed effect(s), including ",
      paste(fixed_terms, collapse = ", "),
      ", which estimate the overall relationship with ",
      response_var,
      " across the entire dataset. For more detailed interpretations of fixed effects, use `details = \"fixed\"`."
    )
  }

  # 3. Mention random effects
  ## Initialize interpretation texts
  random_effect_summary <- ""
  random_intercepts <- c()
  random_slopes <- c()
  special_notes <- c()

  ## Loop through random terms to extract intercepts, slopes, and detect special structures
  for (i in seq_along(random_structure)) {
    # Split random terms into slopes and intercepts
    random_expr <- deparse(random_structure[[i]])
    split_term <- strsplit(random_expr, "\\|")[[1]]
    slope_part <- trimws(split_term[1])  # slope before '|'
    group_var <- trimws(split_term[2])   # intercept after '|'
    random_intercepts <- c(random_intercepts, group_var)
    # Detect if random slopes exist
    if (slope_part != "1") {
      slope_vars <- unlist(strsplit(slope_part, "\\+"))  # Handle multiple slopes
      random_slopes <- c(random_slopes,
                         trimws(slope_vars[!(slope_vars %in% c("0 ", "1 "))]))
    }
    # Detect special structures and add note
    if (grepl(":", group_var)) {
      special_notes <- c(special_notes, "Note: Nested random effects detected, capturing hierarchical relationships.")
    }
    if (grepl("0", slope_part)) {
      special_notes <- c(special_notes, "Note: Uncorrelated random effects detected.")
    }
  }

  ## List random effects
  random_effect_summary <-
    paste0("The model includes the following random effects across grouping variables: ",
           paste(unique(sapply(random_structure, deparse)), collapse = " and "), ".")

  ## Explain random intercepts
  if (length(random_intercepts) > 0) {
    intercept_text <- paste0(
      "*The random intercept(s), ", paste(unique(random_intercepts), collapse = ", "),
      ", account for variations in the intercept across different groups, allowing each group to have its own baseline value that deviates from the overall average."
    )
  } else {
    intercept_text <- ""
  }

  ## Explain random slopes
  if (length(random_slopes) > 0) {
    slope_text <- paste0(
      "*On top of the varying baselines, the model allows the effects of ",
      paste(unique(random_slopes), collapse = ", "), " on ", response_var,
      " to differ across their corresponding groups. This means that not only can each group start from a different baseline, but the strength and direction of the relationships between these variables and ", response_var, " can also vary from one group to another."
    )
  } else {
    slope_text <- ""
  }

  ## Add a complexity note if too many random terms are present
  if (length(random_structure) > 2) {
    special_notes <- c(special_notes, "Note: The model contains multiple random effect terms, which may complicate interpretation.")
  }
  special_notes <- c(special_notes, "Check `details = 'random'` for more interpretation.")
  special_notes_text <- paste(unique(special_notes), collapse = " ")

  ## Combine all parts into the random effects interpretation
  final_random_effect_summary <- paste0(
    random_effect_summary, "\n", " ",
    intercept_text, "\n", " ",
    slope_text, "\n",
    special_notes_text
  )

  # 4. Final general interpretation
  final_general_interpretation <- paste0(
    general_text, "\n\n",
    fixed_effect_text, "\n\n",
    final_random_effect_summary
  )
  return(final_general_interpretation)
}

# Construct "fixed" output text //////////////////////
fixed_explain <- function(model) {

  # Extract basic model information
  model_formula <- stats::formula(model)
  grouping_vars <- names(lme4::getME(model, "flist"))
  response_var <- as.character(attr(stats::terms(model_formula), "variables"))[[2]]
  # Extract fixed effects summary information
  fixed_effects_summary <- summary(model)$coefficients
  fixed_effect_names <- rownames(fixed_effects_summary)
  # Separate intercept from other fixed effects
  intercept_term <- "(Intercept)"
  non_intercept_terms <- fixed_effect_names[fixed_effect_names != intercept_term]
  # Detect interaction terms
  has_interaction <- any(grepl(":", non_intercept_terms))

  # 1. Displayed Output: General Fixed Effects Explanation
  ## If ONLY intercept present (no fixed effect)
  if (length(non_intercept_terms) == 0) {
    fixed_effect_intro <- paste0(
      "This model includes only a baseline estimate of ", intercept_term, ". ",
      "This indicates that no additional fixed effect predictors were intentionally included in the model.\n\n",
      "The intercept represents the expected value of `", response_var, "` across all groups when all predictors are set to zero. ",
      "To interpret the estimates:\n",
      "- The <Estimate> represents the expected baseline value of `", response_var, "`. \n",
      "- The <Standard Error> reflects the uncertainty around this baseline estimate.\n",
      "- The <t-value> measures the significance of the intercept relative to its variability.\n"
    )
  } else {
    ## If interaction present (add a note)
    interaction_note <- if (has_interaction) {
      "\nNote: Interaction effects were included in the model. These effects should be interpreted in conjunction with their associated main effects."
    } else {
      ""
    }
    ## If fixed effect(s) present
    fixed_effect_intro <- paste0(
      "This model includes the following fixed effects: ",
      paste(non_intercept_terms, collapse = ", "),
      ", along with the baseline estimate ", intercept_term, ".\n",
      "Fixed effects estimate the average relationship between the predictors and the response variable `", response_var, "` across all groups.\n",
      "These effects reflect how changes in each predictor influence `", response_var, "`, assuming all other variables are held constant and deviations from the baseline are measured accordingly.\n\n",
      "To interpret the estimates:\n",
      "- The <Estimate> represents the expected change in `", response_var, "` for a one-unit change in the predictor. \n",
      "- The <Standard Error> reflects uncertainty around the estimate.\n",
      "- The <t-value> measures the significance of the effect relative to its variability.\n",
      "- The <Correlation> provides insight into the relationships between fixed effect predictors.\n",
      interaction_note, "\n\n",
      "For detailed interpretations of each fixed effect, use: ",
      "`attr(, 'fixed_details')[['variable_name']]` to view specific results. ",
      "You can find the list of available fixed effect variable names using: ",
      "`attr(, 'fixed_names')` to guide your extraction."
    )
  }

  # 2. Hidden Detailed Extraction
  ## Generate a list of formatted interpretations for each fixed effect
  fixed_effect_detailed <- lapply(fixed_effect_names, function(var) {

    estimate <- round(fixed_effects_summary[var, "Estimate"], 3)
    se <- round(fixed_effects_summary[var, "Std. Error"], 3)
    t_value <- round(fixed_effects_summary[var, "t value"], 3)

    # Construct individual fixed effect interpretation
    if (var == intercept_term) {
      # intercept
      paste0(
        "The intercept estimate suggests that when all predictors are set to zero, the expected value of `", response_var, "` is approximately ", estimate,
        ". This serves as the baseline for evaluating the effects of other variables in the model."
      )
    } else if (grepl(":", var)) {
      # interaction
      interaction_vars <- strsplit(var, ":")[[1]]
      num_vars <- length(interaction_vars)
      interaction_description <- paste(paste0("'", interaction_vars, "'"), collapse = ", ")

      paste0(
        "The interaction effect among ", interaction_description,
        " suggests that the relationship between `", response_var, "` and ",
        interaction_vars[1],
        " depends on the levels of ",
        paste(interaction_vars[-1], collapse = ", "),
        ". The estimated change in `", response_var, "` for this interaction term is approximately ", estimate,
        ", with a standard error of ", se,
        " and a t-value of ", t_value,
        ". This effect should be considered in conjunction with the main effects of ",
        paste(interaction_vars, collapse = ", "),
        " and any lower-order interactions involved."
      )
    } else {
      # fixed variable
      paste0(
        "The fixed effect for '", var, "' suggests that for a one-unit increase in '", var,
        "', the expected change in `", response_var, "` is approximately ", estimate,
        ", assuming all other variables are held constant. This estimate has a standard error of ", se,
        " and a t-value of ", t_value,
        ". To assess the overall impact of this effect, consider it relative to the model's baseline intercept."
      )
    }
  })

  # Store detailed interpretation in attributes
  names(fixed_effect_detailed) <- fixed_effect_names
  attr(fixed_effect_intro, "fixed_details") <- fixed_effect_detailed
  attr(fixed_effect_intro, "fixed_names") <- fixed_effect_names

  return(fixed_effect_intro)
}

# Construct "random" output text //////////////////////
random_explain <- function(model) {

  # Extract basic model information
  model_formula <- stats::formula(model)
  response_var <- as.character(attr(stats::terms(model_formula), "variables"))[[2]]
  random_structure <- lme4::findbars(model_formula)
  # Data prep
  num_random_terms <- length(random_structure)
  is_are <- ifelse(num_random_terms == 1, "is", "are")
  term_s <- ifelse(num_random_terms == 1, "random term", "random terms")
  intro_text <- paste("There", is_are, num_random_terms, term_s,
                      "you fit in the model:\n\n")
  # Find parent group for nesting structure
  all_grouping_vars <- unlist(lapply(lme4::findbars(model_formula), function(x) {
    trimws(strsplit(deparse(x), "\\|")[[1]][2])
  }))
  group_counts <- table(unlist(strsplit(all_grouping_vars, ":")))
  max_count <- max(group_counts)
  parent_candidates <- names(group_counts[group_counts == max_count])

  # Special note for models with too many random terms
  complexity_note <- if (num_random_terms > 2) {
    "Note: This model contains multiple random effect terms, which may complicate interpretation. Implicit nesting that the function cannot detect might also be present and yield incorrect interpretations.\n"
  } else {
    ""
  }

  # 1. Individual term interpretations
  random_effect_interpretations <- c()
  seen_groups <- c()

  for (i in seq_along(random_structure)) {

    random_expr2 <- deparse(random_structure[[i]])
    split_term2 <- strsplit(random_expr2, "\\|")[[1]]
    slope_part2 <- trimws(split_term2[1])  # Before "|"
    group_var2 <- trimws(split_term2[2])   # After "|"

    # Detect nested structure
    is_nested <- grepl(":", group_var2)
    nesting_levels <- if (is_nested) strsplit(group_var2, ":")[[1]] else NULL
    # Check if the assumed parent was seen before this term
    if (is_nested) {
      parent_seen_before <- any(parent_candidates %in% seen_groups)
      is_regular <- parent_seen_before
    } else {
      is_regular <- FALSE  # Non-nested groups don't need order checking
    }
    seen_groups <- c(seen_groups, group_var2)
    # Detect uncorrelated random effects (double vertical bar)
    is_uncorrelated <- grepl("0", slope_part2)

    if (is_uncorrelated) {     # uncorrelated structure

      slope_vars2 <- trimws(unlist(strsplit(slope_part2, "\\+")))
      slope_vars2 <- slope_vars2[slope_vars2 != "0"]  # remove "0"

      interpretation_r <- paste0(
        random_expr2, ": This term specifies uncorrelated random effects for ",
        paste(slope_vars2, collapse = ", "),
        " within ", group_var2,
        ". The model assumes these effects are independent of each other, allowing for more flexible variance estimation across groups."
      )

    } else if (is_nested) {    # nested structure

      if (is_regular) { nesting_levels <- rev(nesting_levels) }
      nesting_description <- paste(
        paste0("'", nesting_levels, "'"),
        collapse = " nested within "
      )
      slope_vars2 <- trimws(unlist(strsplit(slope_part2, "\\+")))
      slope_vars2 <- slope_vars2[slope_vars2 != "1"] # remove "1"

      intercept_text2 <- paste0(  ## Interpret random intercepts
        "This term captures a nested random effect structure where observations are grouped within ", nesting_description,
        ". Your model assumes there is across-group heterogeneity in the baseline level of ", response_var,
        " across this hierarchy, allowing each lower-level group to deviate from both the overall average and the average of its immediate higher-level group."
      )
      slope_text2 <- if (length(slope_vars2) > 0) { ## Interpret random slopes
        paste0(
          " Additionally, the effects of ", paste(slope_vars2, collapse = ", "), " on ", response_var,
          " are allowed to vary within each nested grouping. This captures how the relationship between these variables and ",
          response_var, " changes across different levels of the nested groups."
        )
      } else {
        ""
      }
      # Combine intercept and slope interpretation
      interpretation_r <- paste0(random_expr2, ": ", intercept_text2, slope_text2)

    } else {    # standard

      slope_vars2 <- trimws(unlist(strsplit(slope_part2, "\\+")))
      slope_vars2 <- slope_vars2[slope_vars2 != "1"]

      intercept_text2 <- paste0(  ## Interpret random intercepts
        "Your model assumes there is across-group heterogeneity in the baseline level of ", response_var,
        " across ", group_var2,
        " (random intercepts), allowing each group to have its own starting point that deviates from the overall average."
      )
      slope_text2 <- if (length(slope_vars2) > 0) {  ## Interpret random slopes
        paste0(
          "Additionally, the effects of ", paste(slope_vars2, collapse = ", "),
          " on ", response_var, " are allowed to vary within each ", group_var2,
          " (random slopes), capturing differences in these relationships across groups."
        )
      } else {
        ""
      }
      # Combine intercept and slope interpretation
      interpretation_r <- paste0(random_expr2, ": ", intercept_text2, " ", slope_text2)

    }
    # Append interpretation to the list
    random_effect_interpretations <- c(random_effect_interpretations, interpretation_r)
  }
  # Final random interpretation output
  final_random_interpretation <- paste0(
    intro_text,
    paste(random_effect_interpretations, collapse = "\n\n"), "\n\n",
    "To explore variance contributions based on the model estimates, use: `attr(, 'var_details')`.",
    "\n", complexity_note
  )

  # 2. Calculate variance proportions and interpretations
  variance_info <- extract_variance_proportions(model)
  variance_proportions <- variance_info$variance_proportions
  high_contributors <- variance_info$high_contributors
  low_contributors <- variance_info$low_contributors
  residual_warning <- variance_info$residual_warning

  high_contrib_text <- if (length(high_contributors) > 0) { # High contributors interpretation
    paste0(
      "The following terms explain a substantial proportion of the variance: ",
      paste(high_contributors, collapse = ", "),
      ". This suggests that these random effects contribute meaningfully to capturing group-level heterogeneity."
    )
  } else {
    "No random effects appear to have a substantial contribution to the model's variability."
  }
  low_contrib_text <- if (length(low_contributors) > 0) { # Low contributors interpretation
    paste0(
      "The following terms explain a relatively small portion of the variance: ",
      paste(low_contributors, collapse = ", "),
      ". You might reconsider including these terms in your random effect structure if their contributions are negligible."
    )
  } else {
    "All random effects seem to contribute meaningfully to the model."
  }
  residual_text <- if (residual_warning) { # Residual variance warning
    "Note: The residual variance dominates, suggesting that within-group heterogeneity explains most of the variation. This could indicate a weak random effect structure."
  } else {
    ""
  }

  # 3. Combine for final output
  attr(final_random_interpretation, "var_details") <- list(
    high_var = paste(high_contrib_text, residual_text),
    low_var = low_contrib_text
  )

  return(final_random_interpretation)
}

# Helper for `random_explain()` function
extract_variance_proportions <- function(model) {

  # Extract random variance components from the model
  var_components <- lme4::VarCorr(model)
  variances <- c()
  names_list <- c()

  for (i in seq_along(var_components)) {
    var_matrix <- var_components[[i]]
    group_name <- attr(var_components, "names")[i]
    # Extract variances from the diagonal (includes intercepts and slopes)
    diag_vars <- diag(as.matrix(var_matrix))
    term_names <- rownames(var_matrix)
    # Store variance and associated group-term names
    variances <- c(variances, diag_vars)
    names_list <- c(names_list, paste0(group_name, " - ", term_names))
  }
  # Extract residual variance
  residual_variance <- attr(var_components, "sc")^2
  variances <- c(variances, residual_variance)
  names_list <- c(names_list, "Residual")

  # Calculate proportions
  total_variance <- sum(variances)
  variance_proportions <- variances / total_variance
  # Rank and classify variances
  ranked_indices <- order(variance_proportions, decreasing = TRUE)
  n_terms <- length(variance_proportions)
  # Determine thresholds for high and low variance contributors
  top_n <- ceiling(n_terms * 0.2)
  bottom_n <- floor(n_terms * 0.2)

  # Obtain the list of high or low contributors
  high_contributors <- names_list[ranked_indices[1:top_n]]
  low_contributors <- names_list[ranked_indices[(n_terms - bottom_n + 1):n_terms]]
  # Check if residual is dominant in variance
  residual_warning <- (names_list[ranked_indices[1]] == "Residual")

  # Return structured results
  list(
    variance_proportions = variance_proportions,
    high_contributors = high_contributors,
    low_contributors = low_contributors,
    residual_warning = residual_warning
  )

}
