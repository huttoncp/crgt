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


# extract random slopes (internal) ----------------------------------------
#' @title Extract the random slopes, SEs, ORs and OR CIs from an RGT stability model.
#'
#' @description This is an internal function used by \code{\link{rgt_stability}}
#'   to extract the random slopes for the change across sessions for each
#'   subject and choice option cluster/grouping.
#'
#' N.B. the subject x choice estimates for the change across sessions provided
#' by this function use the combined fixed effect and random effect coefficients
#' and their standard errors. While the point estimates (coefficients) should be
#' accurate, this approach assumes that the random effect variances and fixed
#' effect variances are independent, which may not be the case, as discussed
#' \href{https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#confidence-intervals-on-conditional-meansblupsrandom-effects:~:text=Getting%20the%20uncertainty%20of,intercept%20for%20each%20group}{here}.
#'
#' When I have time, I will attempt to use either a Bayesian or bootstrapping
#' approach to estimate the random slope confidence intervals to see how
#' reasonable this assumption is, as recommended
#' \href{https://stackoverflow.com/questions/26198958/extracting-coefficients-and-their-standard-error-for-each-unit-in-an-lme-model-f#:~:text=Two%20alternatives%20would,the%20bootstrap%20distributions.}{here}.
#'
#' @importFrom emmeans emtrends
#' @importFrom glmmTMB ranef
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr left_join
#' @importFrom tidyr separate
#' @importFrom stats qnorm
#' @importFrom tibble as_tibble
#'
#' @param .stability_model An across-session stability model for cued or uncued
#'   Rat Gambling Task data fit using \code{\link{rgt_stability}}.
#'
#' @param ci_level confidence interval (CI) level (default = 0.95 for 95% CIs).
#'
#' @param digits determines the how many digits the results are rounded to
#'   (default = 3).
#'
#' @param n_sim Number of simulations to use for diagnostic evaluation of model
#'   fit via [DHARMa::simulateResiduals()]. Default is 1,000.
#'
#' @return A list containing the following objects:
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
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @noRd
extract_random_slopes <- function(.stability_model, ci_level = 0.95, digits = 3) {
  session_choice_fe <- emmeans::emtrends(model, ~choice, var = "session") |>
    as.data.frame() |>
    dplyr::select(choice, session_trend_fe = session.trend, se_fe = SE)

  session_choice_re <- as.data.frame(glmmTMB::ranef(model, condVar=TRUE)) |>
    dplyr::rename(beta = condval) |>
    dplyr::filter(grpvar == "choice:subject", term == "session") |>
    tidyr::separate(grp, into = c("choice", "subject"), sep = ":") |>
    dplyr::select(subject, choice, session_trend_re = beta, se_re = condsd)

  random_slope_CIs <- dplyr::left_join(session_choice_re, session_choice_fe, by = "choice") |>
    dplyr::mutate(session_beta = session_trend_fe + session_trend_re,
                  session_se = se_fe + se_re,
                  session_ci_lower = session_beta - session_se*stats::qnorm(1-((1 - ci_level)/2)),
                  session_ci_upper = session_beta + session_se*stats::qnorm(1-((1 - ci_level)/2)),
                  session_OR = exp(session_beta),
                  OR_CI_lower = exp(session_ci_lower),
                  OR_CI_upper = exp(session_ci_upper)) |>
    dplyr::mutate(across(c(session_beta, session_se, session_OR, OR_CI_lower, OR_CI_upper), ~round(.x, digits = digits))) |>
    dplyr::select(subject, choice, session_beta, session_se, session_OR, OR_CI_lower, OR_CI_upper) |>
    tibble::as_tibble()

  if(sort) {
    random_slope_CIs <- random_slope_CIs |>
      dplyr::mutate(subject = as.numeric(subject)) |> #assumes subject IDs are numeric
      dplyr::arrange(subject, choice)
  }
  return(random_slope_CIs)
}

# rgt stability assessment ------------------------------------------------
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
#' model for it by setting family = "binomial", and weights = n_trials, for
#' [glmmTMB::glmmTMB()], which is the function we use to fit the model.
#' [glmmTMB::glmmTMB()] is preferred over [lme4::glmer()] because it tends to
#' converge more often with fewer warning/error messages in my experience.
#'
#' Choice preference stability can be evaluated at a whole-sample level by
#' examining the statistical significance of the fixed effect terms "session"
#' and "session:choice" (the session by choice interaction) on the right-hand
#' side of the model formula. The stability of RGT choice preferences for each
#' rat can be evaluated by examining the random slope parameters, or the
#' "(session | subject/choice)" part of the model formula, where we want to see
#' if there is a change in choice preference across sessions for each choice
#' option presented to each subject. This function currently categorizes rats as
#' "stable" or "unstable" depending upon whether or not the confidence interval
#' for the odds ratio of the random slope coefficient that represents the extent
#' to which their choice preference varies across sessions, overlaps with 1 for
#' their preferred choice. I.e., this is intended to indicate whether or not the
#' rat has a stable preference.
#'
#' N.B. the subject x choice estimates for the change across sessions provided
#' by this function use the combined fixed effect and random effect coefficients
#' and their standard errors. While the point estimates (coefficients) should be
#' accurate, this approach assumes that the random effect variances and fixed
#' effect variances are independent, which may not be the case, as discussed
#' \href{https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#confidence-intervals-on-conditional-meansblupsrandom-effects:~:text=Getting%20the%20uncertainty%20of,intercept%20for%20each%20group}{here}.
#'
#' When I have time, I will attempt to use either a Bayesian or bootstrapping
#' approach to estimate the random slope confidence intervals to see how
#' reasonable this assumption is, as recommended
#' \href{https://stackoverflow.com/questions/26198958/extracting-coefficients-and-their-standard-error-for-each-unit-in-an-lme-model-f#:~:text=Two%20alternatives%20would,the%20bootstrap%20distributions.}{here}.
#'
#' @importFrom stringr str_remove
#' @importFrom stringr str_which
#' @importFrom stringr str_subset
#' @importFrom stringr str_glue
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr across
#' @importFrom dplyr rowwise
#' @importFrom dplyr c_across
#' @importFrom dplyr ungroup
#' @importFrom dplyr between
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
#'   uncued Rat Gambling Task that has been parsed by any of the *read()
#'   functions in this package (such as [rgt_read()]) and aggregated by session
#'   using the matching *prep() function (such as [rgt_prep()]). N.B. This
#'   function assumes that subject IDs are numeric.
#'
#' @param n_sessions you may not have enough data to evaluate stability at the
#'   individual rat level (requires random slopes) if using fewer than 5
#'   sessions of data. To evaluate stability using fewer than 5 sessions of RGT
#'   data, use [rgt_rm_aov()] instead.
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
#' @param residual_plot Set to TRUE if you want the residual diagnostic plot to
#'   be generated as this function is executed, which may slow things down a
#'   bit. You can generate the plot afterwards if you store the output and then
#'   pass the sim_resid component to the plot() function.
#'
#' @param stable_colour The colour to use to represent rats flagged as stable on
#'   their preferred choice option in the random slopes plot.
#'
#' @param unstable_colour The colour to use to represent rats flagged as
#'   unstable on their preferred choice option in the random slopes plot.
#'
#' @param other_colour The colour to use to represent each rat's non-preferred
#'   choice options in the random slopes plot.
#'
#' @param reference_line_colour The colour to use for the odds-ratio reference
#'   line (at 1) in the random slopes plot.
#'
#' @return A list containing the following objects:
#'
#' \itemize{
#'   \item **data:** A tibble (enhanced data frame) of the data used to fit the stability evaluation model
#'   \item **model:** The logistic GLMM used to evaluate choice preference stability fit with [glmmTMB::glmmTMB()]
#'   \item **sim_resid:** Simulated model residuals using [DHARMa::simulateResiduals()]. Can be graphed with plot() and evaluated with other DHARMa package functions.
#'   \item **anova:** Analysis of Variance for the model fixed effects using type III SS and sum-to-zero factor contrasts
#'   \item **random_slopes:** subject/choice-specific session coefficients and distribution metrics
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
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @export
rgt_stability <- function(.rgt_df, n_sessions = 5, alpha = 0.05,
                          diagnostics = TRUE, n_sim = 1000, residual_plot = FALSE,
                          stable_colour = "#149c4d", unstable_colour = "purple2", other_colour = "black",
                          reference_line_colour = "blue2") {

  names(.rgt_df) <- stringr::str_remove(names(.rgt_df), "choice_prop_")

  preferred_choices <- .rgt_df |>
    dplyr::arrange(subject, session_date) |>
    dplyr::mutate(session = translate(session_date,
                                      sort(unique(session_date)),
                                      seq_along(sort(unique(session_date))))) |>
    dplyr::filter(session > (max(session) - 5)) |>
    dplyr::summarise(dplyr::across(P1:P4, ~mean(.x, na.rm = TRUE)), .by = subject) |>
    dplyr::rowwise() |>
    dplyr::mutate(top_choice = which.max(dplyr::c_across(P1:P4)),
                  top_choice = paste0("P", top_choice)) |>
    dplyr::ungroup() |>
    dplyr::select(subject, top_choice)

    .rgt_df <- .rgt_df |>
      dplyr::arrange(subject, session_date) |>
      dplyr::mutate(session = translate(session_date,
                                        sort(unique(session_date)),
                                        seq_along(sort(unique(session_date))))) |>
      dplyr::filter(session > (max(session) - n_sessions)) |>
      dplyr::select(subject, session, n_trials, P1:P4) |>
      tidyr::pivot_longer(cols = P1:P4,
                          names_to = "choice", values_to = "choice_prop") |>
      dplyr::mutate(choice = factor(choice, levels = c("P1", "P2", "P3", "P4"))) |>
      dplyr::mutate(session = as.numeric(as.character(session)),
                    subject = factor(subject, levels = sort(unique(subject), decreasing = TRUE)))

  .res <- list()
  .res[["data"]] <- tibble::as_tibble(.rgt_df)

  .m <- suppressWarnings(glmmTMB::glmmTMB(choice_prop ~ session*choice +
                                            (1 + session | subject/choice),
                                          family = "binomial", weights = n_trials,
                                          data = .rgt_df,
                                          contrasts = list(choice = "named.contr.sum")))
  .res[["model"]] <- .m

  if(diagnostics == TRUE){
    sim_res <- suppressMessages(DHARMa::simulateResiduals(.m, n_sim))
    .res[["sim_resid"]] <- sim_res
  }
  if(residual_plot == TRUE) {
    suppressMessages(plot(sim_res))
  }

  .aov <- car::Anova(.m, type = "III")

  .res[["anova"]] <- .aov

  p_vals <- .aov[-1, 3]
  .terms <- rownames(.aov[-1,])

  .res[["random_slopes"]] <- extract_random_slopes(.m) |>
    dplyr::left_join(dplyr::mutate(preferred_choice, subject = as.character(subject)), by = "subject") |>
    dplyr::mutate(OR_ref = 1,
                  stable_preference = dplyr::case_when(choice == top_choice & dplyr::between(OR_ref, OR_CI_lower, OR_CI_upper) ~ "Yes",
                                                       choice == top_choice & !dplyr::between(OR_ref, OR_CI_lower, OR_CI_upper) ~ "No",
                                                       TRUE ~ "Other"),
                  .by = subject) |>
    dplyr::mutate(stable_preference = factor(stable_preference, levels = c("No", "Yes", "Other"))) |>
    tibble::as_tibble() |>
    dplyr::mutate(subject = as.numeric(subject)) |>
    dplyr::arrange(subject)

  .res[["random_slopes_plot"]] <- .res[["random_slopes"]] |>
    dplyr::mutate(choice = forcats::fct_drop(choice),
                  subject = factor(subject) |>
                    forcats::fct_rev()) |>
    ggplot2::ggplot(ggplot2::aes(y = session_OR, x = subject, colour = stable_preference)) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = OR_CI_lower, ymax = OR_CI_upper), linewidth = 1.1) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 1), linetype = "dashed",
                        linewidth = 1.2, alpha = 0.7, colour = reference_line_colour) +
    ggplot2::facet_wrap(~choice, scales = "free", ncol = 4) + ggplot2::coord_flip() +
    ggplot2::theme_bw(12) +
    ggplot2::scale_colour_manual(breaks = c("No", "Yes", "Other"),
                                 values = c(unstable_colour, stable_colour, other_colour)) +
    ggplot2::labs(y = "session odds-ratio (95% CI)", colour = "stable preference")

  .res[["unstable_subjects"]] <- .res[["random_slopes"]] |>
    dplyr::filter(stable_preference == "No") |>
    dplyr::select(subject, preferred_choice = top_choice, session_OR, OR_CI_lower, OR_CI_upper) |>
    dplyr::distinct() |>
    dplyr::mutate(subject = as.numeric(subject)) |>
    dplyr::arrange(subject)

  if(any(.res[["random_slopes"]]$stable_preference == "No")) {
    n_unstable <- nrow(.res[["unstable_subjects"]]$subject)
    message(stringr::str_glue("random slopes suggest that choice preference is unstable for {n_unstable} subjects"))
  }

  if(p_vals[stringr::str_which(.terms, "^session:choice$")[1]] < alpha) {
    message("session by choice interaction (fixed effect interaction) IS significant")
    .res[["posthoc_interaction"]] <-  emmeans::emtrends(.res[["model"]], pairwise ~ choice,
                                                        var = "session", regrid = "response",
                                                        contrasts = TRUE, infer = TRUE)
    .res[["posthoc_interaction_plot"]] <- plot(.res[["posthoc_interaction"]],
                                               horizontal = FALSE,
                                               xlab = "session trend") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = 0), linetype = "dashed",
                          linewidth = 1.2, alpha = 0.5, colour = "red2") +
      ggplot2::theme_bw(12)
  } else {
    message("session by choice interaction (fixed effect interaction) is NOT significant")
  }

  return(.res)
}

#' Check if rats' choice preferences are stable across recent sessions of the Rat Gambling Task (RGT)
#'
#' Use a repeated measures ANOVA to check if rats' choice preferences are stable
#' across the last "n" (5 by default) sessions of the cued (Barrus & Winstanley,
#' 2016) or uncued (Zeeb et al., 2009) version of the rat gambling task (RGT)
#' that has been parsed by [rgt_read()] or [rgt_read_file()] and aggregated by
#' session using [rgt_prep()], in the long format (default format). In this
#' approach, the arcsine transformation is first applied to the choice
#' preference data because it is bounded by 0 and 1 and are actually binomial in nature (preference = # times chosen/total choices; MacDonald, 2009). N.B. this
#' approach is no longer considered best statistical practice, so this function
#' is mostly intended to provide a way to check for stability to enable
#' replication and comparisons to earlier publications. Current best practice is to use a logisitic mixed effects model to evaluate stability via [rgt_stability()]
#'
#' Choice preference stability of the sample can be evaluated by examining the
#' statistical significance of the session by choice interaction term.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom tidyr pivot_longer
#' @importFrom afex aov_car
#'
#' @param .rgt_df A data frame of session-aggregated data from the cued or
#'   uncued Rat Gambling Task. That has been parsed by [rgt_read()] or
#'   [rgt_read_file()] and aggregated by session using [rgt_prep()].
#'
#' @param n_sessions number of sessions to use to evaluate stability (default = 5).
#'
#' @return A repeated measures ANOVA results table
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
#' McDonald, J. H. (2009). Handbook of biological statistics (Vol. 2, pp. 6-59).
#' Baltimore, MD: sparky house publishing. URL: https://www.biostathandbook.com/transformation.html
#'
#' @export
rgt_rm_aov <- function(.rgt_df, n_sessions = 5) {
  .rgt_df <- .rgt_df |>
    dplyr::filter(session > max(session) - n_sessions) |>
    dplyr::arrange(subject, session) |>
    dplyr::mutate(session = 1:n_sessions) |>
    dplyr::select(subject, session, n_trials, P1:P4) |>
    tidyr::pivot_longer(cols = P1:P4,
                        names_to = "choice", values_to = "choice_prop") |>
    dplyr::mutate(choice = factor(choice, levels = c("P1", "P2", "P3", "P4"))) |>
    dplyr::mutate(session = as.numeric(as.character(session)),
                  subject = factor(subject, levels = sort(unique(subject), decreasing = TRUE)),
                  choice_prop = asin(sqrt(choice_prop)))
  out <- afex::aov_car(choice_prop ~ session*choice + Error(subject/(session*choice)), data = .rgt_df)
  return(out)
}
