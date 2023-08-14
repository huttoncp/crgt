# Copyright 2023 Craig P. Hutton
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


# rgt stability assessment ------------------------------------------------
#n_sessions

#' Check if rats' choice preferences are stable across recent sessions of the Rat Gambling Task (RGT)
#'
#' Use a logistic mixed effects model to check if rats' choice preferences are
#' stable across the last "n" (5 by default) sessions of the cued (Barrus &
#' Winstanley, 2016) or uncued (Zeeb et al., 2009) version of the rat gambling
#' task (RGT) that has been parsed by [rgt_read()] or [rgt_read_file()] and
#' aggregated by session using [rgt_prep()], in the long format (default
#' format).
#'
#' The [formula](https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#model-specification) for the model is:
#'
#' choice_prop ~ session + choice + session:choice + (session | subject/choice)
#'
#' Where the dependent variable on the left-hand side, "choice_prop", is the
#' proportion of trials in which the rat chose a particular option ("choice"
#' variable), considered to represent its "preference" for that option. Because
#' this is a proportion out of a known total number of trials, we use a logistic
#' model for it by setting family = "binomial" for [glmmTMB::glmmTMB()], which
#' is the function we use to fit the model. [glmmTMB::glmmTMB()] is preferred
#' over [lme4::glmer()] because it tends to converge more often with fewer
#' warning/error messages in my experience.
#'
#' Choice preference stability can be evaluated at a whole-sample level by
#' examining the statistical significance of the fixed effect terms "session"
#' and "session:choice" (the session by choice interaction) on the right-hand
#' side of the model formula. The stability of RGT choice preferences for each
#' rat can be evaluated by examining the random slope parameters, or the
#' "(session | subject/choice)" part of the model formula, where we want to see
#' if there is a change in choice preference across sessions for each choice
#' option presented to each subject.
#'
#' @importFrom stringr str_remove
#' @importFrom stringr str_which
#' @importFrom stringr str_subset
#' @importFrom stringr str_glue
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble
#' @importFrom forcats fct_drop
#' @importFrom glmmTMB glmmTMB
#' @importFrom glmmTMB ranef
#' @importFrom DHARMa simulateResiduals
#' @importFrom DHARMa testOutliers
#' @importFrom car Anova
#' @importFrom emmeans emmeans
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 theme_bw
#'
#' @param .rgt_df A data frame of session-aggregated data from the cued or
#'   uncued Rat Gambling Task. That has been parsed by [rgt_read()] or
#'   [rgt_read_file()] and aggregated by session using [rgt_prep()].
#'
#' @param n_sessions you may not have enough data to evaluate stability at the
#'   individual rat level (random effects) if using fewer than 5 sessions of
#'   data.
#'
#' @param alpha Statistical significance level for p-values. Default is 0.05.
#'
#' @param diagnostics Should model diagnostic checks be performed using
#'   simulated residuals via [DHARMa::simulateResiduals()] (TRUE/FALSE)? Default
#'   is TRUE.
#'
#' @param n_sim Number of simulations to use for diagnostic evaluation of model
#'   fit via [DHARMa::simulateResiduals()]. Default is 1,000.
#'
#' @return A list containing the following objects:
#'
#' \itemize{
#'   \item **data:** A tibble (enhanced data frame) of the data used to fit the stability evaluation model
#'   \item **model:** The logistic GLMM used to evaluate choice preference stability fit with [glmmTMB::glmmTMB()]
#'   \item **sim_resid:** Simulated model residuals using [DHARMa::simulateResiduals()]. Can be graphed with plot().
#'   \item **anova:** Analysis of Variance for the model fixed effects using type III SS and sum-to-zero factor contrasts
#'   \item **random_slopes:** random slope coefficients and distribution metrics
#'   \item **random_slopes_plot:** ggplot2 graph of the random slopes for the effect of session for each subject facetted by choice option
#'   \item **posthoc_interaction:** Estimated marginal means and post-hoc pairwise contrasts statistics for the difference in linear session trends between choice options (only included if session x choice fixed effect interaction is significant)
#'   \item **posthoc_interaction_plot:** ggplot2 graph of the linear session trends for each choice option (only included if session x choice fixed effect interaction is significant)
#' }
#'
#' @references
#' Barrus, M.M., & Winstanley, C.A. (2016). Dopamine D3 receptors modulate the
#' ability of win-paired cues to increase risky choice in a rat gambling task.
#' Journal of Neuroscience, 36(3), 785-794.
#'
#' Zeeb, F.D., Robbins, T.W., & Winstanley, C.A. (2009). Serotonergic and
#' dopaminergic modulation of gambling behavior as assessed using a novel rat
#' gambling task. Neuropsychopharmacology, 34(10), 2329-2343.
#'
#' @export
rgt_stability <- function(.rgt_df, n_sessions = 5, alpha = 0.05,
                          diagnostics = TRUE, n_sim = 1000) {

  contr_options <- options("contrasts")
  options(contrasts = c("contr.sum", "contr.poly"))

  names(.rgt_df) <- stringr::str_remove(names(.rgt_df), "choice_prop_")

  .rgt_df <- .rgt_df |>
    dplyr::filter(session > max(session) - n_sessions) |>
    dplyr::select(subject, session, n_trials, P1:P4) |>
    tidyr::pivot_longer(cols = P1:P4,
                        names_to = "choice", values_to = "choice_prop") |>
    dplyr::mutate(choice = factor(choice, levels = c("P1", "P2", "P3", "P4"))) |>
    dplyr::mutate(session = as.numeric(as.character(session)),
                  subject = factor(subject, levels = sort(unique(subject), decreasing = TRUE)))

  .res <- list()
  .res[["data"]] <- tibble::as_tibble(.rgt_df)

  .m <- suppressWarnings(glmmTMB::glmmTMB(choice_prop ~ session*choice +
                                            (session | subject/choice),
                                          family = "binomial", weights = n_trials,
                                          data = .rgt_df))
  .res[["model"]] <- .m

  if(diagnostics == TRUE){
    sim_res <- suppressMessages(DHARMa::simulateResiduals(.m, n_sim))
    .res[["sim_resid"]] <- sim_res
    .res[["sim_resid_plot"]] <- suppressMessages(plot(sim_res))
    DHARMa::testOutliers(sim_res, type = "bootstrap")
  }

  .aov <- car::Anova(.m, type = 3)
  .res[["anova"]] <- .aov

  p_vals <- .aov[-1, 3]
  .terms <- rownames(.aov[-1,])


  if(p_vals[stringr::str_which(.terms, "^session$")[1]] < alpha) {
    message("main effect of session (fixed effect slope) IS significant")
    .res[["session_effect_plot"]] <- plot(emmeans::emmeans(.m, ~session, type = "response"),
                                          horizontal = FALSE,
                                          xlab = "choice preference") +
      ggplot2::theme_bw(12)

  } else {
    message("main effect of session (fixed effect slope) is NOT significant")
  }

  .res[["random_slopes"]] <- suppressWarnings(as.data.frame(glmmTMB::ranef(.m, condVar=TRUE))) |>
    dplyr::rename(beta = condval) |>
    filter(grpvar == "choice:subject", term == "session") |>
    tidyr::separate(grp, into = c("choice", "subject"), sep = ":") |>
    dplyr::mutate(OR = round(exp(beta), 2),
                  OR_LB = round(exp(beta - 2*condsd), 2),
                  OR_UB = round(exp(beta + 2*condsd), 2)) |>
    dplyr::select(subject, choice, term, beta, beta_sd = condsd, OR, OR_LB, OR_UB) |>
    dplyr::mutate(unstable = if_else(OR_LB > 1 | OR_UB < 1, "Yes", "No")) |>
    tibble::as_tibble()

  if(any(.res[["random_slopes"]]$unstable == "Yes")) {
    n_unstable <- .res[["random_slopes"]] |>
      dplyr::filter(unstable == "Yes") |>
      dplyr::pull(subject) |> unique() |> length()
    message(stringr::str_glue("one or more choice preferences may be unstable for {n_unstable} subjects"))
  } else {
    message("choice preferences seem to be reasonably stable for each subject")
  }

  .res[["random_slopes_plot"]] <- .res[["random_slopes"]] |>
    dplyr::mutate(choice = forcats::fct_drop(choice),
                  subject = factor(subject, levels = levels(.rgt_df$subject))) |>
    ggplot2::ggplot(ggplot2::aes(y = OR, x = subject)) +
    ggplot2::geom_point(size = 2, colour = "blue2") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = OR_LB, ymax = OR_UB), size = 1) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 1), linetype = "dashed",
                        linewidth = 1.2, alpha = 0.5, colour = "red2") +
    ggplot2::facet_wrap(~choice, scales = "free", ncol = 4) + ggplot2::coord_flip() +
    ggplot2::theme_bw(12) +
    ggplot2::labs(y = "odds-ratio (+/- 2 SD)")


  if(p_vals[stringr::str_which(.terms, "^session:choice$")[1]] < alpha) {
    message("session by choice interaction (fixed effect interaction) IS significant")
      .res[["posthoc_interaction"]] <- emmeans::emtrends(.m, pairwise ~ choice,
                                                         var = "session", type = "response")
      .res[["posthoc_interaction_plot"]] <- plot(.res[["posthoc_interaction"]],
                                                 horizontal = FALSE,
                                                 xlab = "session trend (linear regression slope)") +
        ggplot2::geom_vline(ggplot2::aes(xintercept = 0), linetype = "dashed",
                            linewidth = 1.2, alpha = 0.5, colour = "red2") +
        ggplot2::theme_bw(12)

  } else {
    message("session by choice interaction (fixed effect interaction) is NOT significant")
  }

  on.exit(options(contr_options)) #restore contrast options to user's prior settings
  return(.res)
}
