#' Assumption Checks for Residuals
#'
#' `res_norm()`, `res_fit()`, and `res_box()` provide diagnostic plots to check
#' model assumptions at the within-group level for linear mixed-effects models
#' fitted with `lme4::lmer()`. These checks are applicable to all valid `lmer`
#' model structures (no shorthand syntax), including complex nested and crossed
#' random structure.
#'
#' @section Checking Assumptions:
#'
#' * `res_norm()` generates a quantile-quantile (QQ) plot of the Pearson residuals
#'   to assess normality within groups.
#' * `res_fit()` plots Pearson residuals against fitted values to detect
#'   funnel shapes or mean-variance trends that suggest transformation issues.
#' * `res_box()` creates a boxplot of residuals grouped by a specified random
#'   intercept grouping variable to visually inspect heteroscedasticity.
#'
#' These plots help detect violations of within-group assumptions in mixed-effect
#' models (normality, homoscedasticity, independence), which are central to
#' valid inference in hierarchical models. Between-group assumption checks are
#' not included here, due to the various natures of complex random structures.
#'
#' @importFrom rlang .data
#'
#' @param model A fitted model object from [lme4::lmer()]. Currently not
#' supporting formula that contains shorthand `.`, such as `y ~ .`.
#'
#' @returns A `ggplot` object.
#'
#' @examples
#' library(lme4)
#' library(ggplot2)
#'
#' model <- lmer(math ~ math_old + cltype + (cltype | school_id), data = star)
#'
#' res_norm(model)
#' res_fit(model)
#' res_box(model, "school_id")
#'
#' @name residuals
#' @export
res_norm <- function(model) {
  # Validate model structure
  model_check(model)
  # Extract residuals
  res <- stats::residuals(model)
  df <- data.frame(res = res)
  # Plot qqplot
  ggplot2::ggplot(df, ggplot2::aes(sample = .data$res)) +
    ggplot2::stat_qq(color = "#375E97", alpha = 0.8) +
    ggplot2::stat_qq_line(color = "#962E2A") +
    ggplot2::labs(title = "QQ Plot of Residuals",
                  subtitle = "Checking Within-Group Normality",
                  x = "Theoretical Quantiles", y = "Sample Quantiles") +
    ggplot2::theme_minimal(base_size = 8) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      axis.title = ggplot2::element_text(size = 12)
    )
}

#' @export
#' @rdname residuals
res_fit <- function(model) {
  # Validate model structure
  model_check(model)
  # Extract residuals and fitted values
  res <- stats::residuals(model)
  fit <- stats::fitted(model)
  df <- data.frame(fit = fit, res = res)
  # Plot
  ggplot2::ggplot(df, ggplot2::aes(x = .data$fit, y = .data$res)) +
    ggplot2::geom_point(alpha = 0.8, color = "#375E97") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#962E2A") +
    ggplot2::labs(title = "Residuals vs. Fitted Values",
                  subtitle = "Checking Mean-Variance Relationships",
                  x = "Fitted Values", y = "Residuals") +
    ggplot2::theme_minimal(base_size = 8) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      axis.title = ggplot2::element_text(size = 12)
    )
}

#' @export
#' @rdname residuals
#' @param group_var A character value specifying the name of the grouping variable.
#' Must be a valid random intercept in `model` and a valid variable in the data used.
res_box <- function(model, group_var) {

  # Validate model structure
  model_check(model)

  # Extract residuals and dataframe
  res <- stats::residuals(model)
  base_df <- stats::model.frame(model)
  # Validate input group_var is in the dataset
  if (!(group_var %in% names(base_df))) {
    stop("The provided `group_var` is not a valid variable name in the model's data.")
  }

  # Extract all random intercepts
  re_terms <- lme4::findbars(stats::formula(model))
  groups <- lapply(re_terms, function(term) {
    term_expr <- deparse(term)
    split_term <- strsplit(term_expr, "\\|")[[1]]
    group_vars <- trimws(split_term[2])
    if (grepl(":", group_vars)) {
      strsplit(group_vars, ":")[[1]]
    } else {
      group_vars
    }
  })
  all_group_vars <- unique(unlist(groups))
  # Validate input group_var is a random intercept
  if (!(group_var %in% all_group_vars)) {
    stop(paste0("The provided `group_var` ('",
                group_var,
                "') is not part of a random intercept grouping structure.")
    )
  }

  # Combine and plot
  df <- data.frame(group = base_df[[group_var]], res = res)
  ggplot2::ggplot(df, ggplot2::aes(x = as.factor(.data$group), y = .data$res)) +
    ggplot2::geom_boxplot(fill = "#375E97", alpha = 0.6) +
    ggplot2::labs(title = "Residuals Variation by Group",
                  subtitle = "Checking Within-Group Heteroscedasticity",
                  x = group_var, y = "Residuals") +
    ggplot2::theme_minimal(base_size = 8) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      axis.title = ggplot2::element_text(size = 12)
    )
}

# Helpers -----------------------------------------------------------
model_check <- function(model) {
  # Check if the input is an lmer model
  if (!inherits(model, "lmerMod")) {
    stop("The input model must be a fitted lmer() object from the lme4 package.")
  }
  # Check if shorthand is used
  model_formula <- stats::formula(model)
  if (grepl("~\\s*\\.(\\s|\\+|\\-|\\)|$)", paste0(deparse(model_formula), collapse = ""))) {
    stop("Shorthand formulas using '.' are not supported. Please specify all fixed effect terms explicitly.")
  }
}
