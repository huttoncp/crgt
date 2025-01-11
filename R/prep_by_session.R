# Copyright 2025 Craig P. Hutton
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

#' Prepare a session-aggregated version of 5CSRT data for analysis.
#'
#' Prepare an analytical version of 5 choice serial reaction time task (5CSRT;
#' Robbins, 2002) data that has been parsed by [fcsrt_read()] or
#' [fcsrt_read_file()]. Either the long or wide forms of the data can be
#' generated and there is a convenience option to export the data to a csv file.
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr if_else
#' @importFrom dplyr case_when
#' @importFrom dplyr join_by
#' @importFrom dplyr relocate
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_wider
#' @importFrom readr write_csv
#' @importFrom tidyselect last_col
#'
#' @param .df A data frame of 5CSRT data that has been imported into R using
#'   either [fcsrt_read()] or [fcsrt_read_file()].
#'
#' @param shape Aggregation format or shape to use. The default, "long" format,
#'   organizes the data with as one column per variable and represents repeated
#'   measurements for experimental subjects as rows. This "tidy" format is
#'   commonly required for analyses performed using R. The alternative "wide"
#'   format instead uses additional columns to represent repeated measurements
#'   for experimental subjects. The wide format is commonly required for
#'   analyses performed using other programs like SPSS.
#'
#' @param output_file Comma-separated variable (csv) file path and name to
#'   export the combined data to, which should end in ".csv". e.g.
#'   "5CSRT_data.csv" to save the file in your working directory.
#'
#' @param recode_session_by_date If TRUE (default) re-codes the session variable
#'   based on the session dates for each rat, in chronological order (from the
#'   perspective of the rat). Set this to FALSE if you want to retain the
#'   original session numbers from the raw MedPC file.
#'
#' @return A data frame containing analysis-ready 5CSRT data with the following columns
#'
#' \itemize{
#'   \item **subject:** rat ID number
#'   \item **session_date:** session date (yyyy-mm-dd) according to the MedPC file
#'   \item **session:** session number (subject's perspective)
#'   \item **n_trials:** number of trials initiated in the session
#'   \item **premat_n:** number of premature responses in the session
#'   \item **premat_prop:** premat_n/n_trials = premature response rate (for the session)
#'   \item **omissions:** number of omissions in the session (where the rat did not chose an option)
#'   \item **total_choices:** number of choices made in the session (successfully completed trials)
#'   \item **n_incorrect:** number of incorrect choices
#'   \item **n_correct:** number of correct choices
#'   \item **prop_correct:** n_correct/total_choices
#'   \item **stage:** The stage of the 5CSRT the rat did for the session
#'   \item **crierion_met:** TRUE if the rat achieved the 5CSRT stage's criterion performance level; FALSE if not
#'   \item **final_criterion_met:** TRUE if the rat achieved the final 5CSRT stage criterion performance level; FALSE if not
#'   \item **sessions_to_crietrion:** number of sessions the rat took to pass the final 5CSRT stage (if it passed)
#' }
#'
#' @references
#' Robbins, T. (2002). The 5-choice serial reaction time task: behavioural
#' pharmacology and functional neurochemistry. Psychopharmacology, 163(3),
#' 362-380.
#'
#' @export
fcsrt_prep <- function(.df, shape = c("long", "wide"), output_file = NULL,
                       recode_session_by_date = TRUE) {

  shape <- match.arg(shape)

  .df <- dplyr::rename(.df, session_date = start_date)

  if(recode_session_by_date == TRUE) {
    .df <- .df |>
      dplyr::arrange(subject, session_date) |>
      dplyr::mutate(session = translate(session_date,
                                        sort(unique(session_date)),
                                        seq_along(sort(unique(session_date)))))
  }

  df_fcsrt_premature <- .df  |>
    dplyr::arrange(subject, session_date) |>
    dplyr::mutate(sum_of_prematures = sum(prematures, na.rm = TRUE),
                  n_trials = sum(!is.na(offered)),
                  total_choices = max(trial, na.rm = TRUE), #total valid trials, where a choice was made
                  prop_prems = (sum_of_prematures/total_choices),
                  prop_premscam = (sum_of_prematures/n_trials),
                  .by = c(subject, session, stim_dur)) |>
    dplyr::select(subject, session_date, session, sum_of_prematures,
                  prop_prems, prop_premscam, stim_dur) |>
    dplyr::arrange(subject, session) |>
    dplyr::distinct() |>
    dplyr::mutate(premat_n = mean(sum_of_prematures, na.rm = TRUE),
                  premat_prop = mean(prop_premscam, na.rm = TRUE),
                  .by = c(subject, session)) |>
    dplyr::select(subject, session_date, session, premat_n, premat_prop) |>
    dplyr::distinct() |>
    dplyr::arrange(subject, session)

  df_fcsrt <- .df |>
    dplyr::filter(msn != "ChamberTest", prematures == 0, trial > 0) |>
    dplyr::arrange(subject, session_date) |>
    dplyr::select(subject, session_date, session, trial, stim_dur, chosen, offered, omission) |>
    dplyr::distinct() |>
    dplyr::arrange(subject, session, trial) |>
    dplyr::mutate(stage = dplyr::case_when(stim_dur == 60 ~ 0,
                                           stim_dur == 30 ~ 1,
                                           stim_dur == 20 ~ 2,
                                           stim_dur == 10 ~ 3),
                  correct = dplyr::if_else(chosen == offered, 1, 0)) |>
    dplyr::mutate(session_date = as.Date(mcv(session_date, na.rm = TRUE)),
                  n_correct = sum(correct, na.rm = TRUE),
                  n_trials = sum(!is.na(offered)),
                  total_choices = round(max(trial, na.rm = TRUE)),
                  prop_correct = n_correct/total_choices,
                  omissions = sum(omission, na.rm = TRUE),
                  n_incorrect = total_choices - (n_correct + omissions),
                  .by = c(subject, session)) |>
    dplyr::mutate(criterion_met = dplyr::case_when(stage %in% c(0:2) & n_correct >= 30 ~ TRUE,
                                                   stage == 3 & n_correct >= 50 & prop_correct >= 0.8 ~ TRUE,
                                                   !is.na(stage) ~ FALSE),
                  final_criterion_met = dplyr::case_when(stage == 3 & n_correct >= 49 & prop_correct >= 0.8 ~ TRUE,
                                                         !is.na(stage) ~ FALSE)) |>
    dplyr::select(subject, session_date, session,
                  n_trials, omissions, total_choices, n_incorrect, n_correct, prop_correct,
                  stage, criterion_met, final_criterion_met) |>
    dplyr::distinct() |>
    dplyr::arrange(subject, session)

  if(any(df_fcsrt$final_criterion_met)) {
    sessions_to_criterion <- df_fcsrt |>
      dplyr::filter(final_criterion_met == TRUE) |>
      dplyr::summarise(sessions_to_criterion = min(session, na.rm = TRUE), .by = subject)
    df_fcsrt <- df_fcsrt |>
      dplyr::left_join(sessions_to_criterion, by = dplyr::join_by(subject)) |>
      dplyr::left_join(df_fcsrt_premature, by = dplyr::join_by(subject, session_date, session)) |>
      dplyr::relocate(premat_n, premat_prop, .after = n_trials) |>
      dplyr::distinct()
  } else {
    df_fcsrt <- df_fcsrt |>
      dplyr::left_join(df_fcsrt_premature, by = dplyr::join_by(subject, session_date, session)) |>
      dplyr::relocate(premat_n, premat_prop, .after = n_trials) |>
      dplyr::distinct()
  }

  if(shape == "wide") {
    df_fcsrt <- df_fcsrt |>
      dplyr::mutate(session = paste0("s", as.character(session))) |>
      tidyr::pivot_wider(
        names_from = session, values_from = c(session_date, n_trials:tidyselect::last_col()),
        names_glue = "{.value}_{session}") |>
      dplyr::arrange(subject)
  }
  df_fcsrt <- wash_df(df_fcsrt)

  if(!missing(output_file)) {
    if(!grepl("\\.csv$", output_file)) {
      message('output file name must end in ".csv"
              \nappending ".csv" to output file name')
    }
    message("writing combined data to disk")
    df_fcsrt |> readr::write_csv(output_file)
  }
  return(df_fcsrt)
}

#' Convert a session-aggregated version of data from the 5 choice serial
#' reaction time task (5CSRT; Robbins, 2002) that has been parsed by
#' [fcsrt_read()] or [fcsrt_read_file()] and prepared by
#' [fcsrt_prep()] from wide format to long format or vice versa based
#' on its current shape. For more information on pivoting data frames between
#' long and wide formats, For more information on pivoting data frames between long and
#' wide formats, see the
#' [pivoting](https://tidyr.tidyverse.org/articles/pivot.html) article on the
#' tidyr website.
#'
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr relocate
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect last_col
#'
#' @param .df A data frame of 5CSRT data that has been imported into R using
#'   either [fcsrt_read()] or [fcsrt_read_file()].
#'
#' @return A data frame containing analysis-ready 5CSRT data with the following
#'   columns, for the long format/shape (one column per variable):
#'
#' \itemize{
#'   \item **subject:** rat ID number
#'   \item **session_date:** session date (yyyy-mm-dd) according to the MedPC file
#'   \item **session:** session number (subject's perspective)
#'   \item **msn:** the MedPC program that was run during the session
#'   \item **n_trials:** number of trials initiated in the session
#'   \item **premat_n:** number of premature responses in the session
#'   \item **premat_prop:** premat_n/n_trials = premature response rate (for the session)
#'   \item **omissions:** number of omissions in the session (where the rat did not chose an option)
#'   \item **total_choices:** number of choices made in the session (successfully completed trials)
#'   \item **n_incorrect:** number of incorrect choices
#'   \item **n_correct:** number of correct choices
#'   \item **prop_correct:** n_correct/total_choices
#'   \item **stage:** The stage of the 5CSRT the rat did for the session
#'   \item **crierion_met:** TRUE if the rat achieved the 5CSRT stage's criterion performance level; FALSE if not
#'   \item **final_criterion_met:** TRUE if the rat achieved the final 5CSRT stage criterion performance level; FALSE if not
#'   \item **sessions_to_crietrion:** number of sessions the rat took to pass the final 5CSRT stage (if it passed)
#' }
#'
#' The wide format/shape contains the same session-aggregated variables that
#' have been pivoted by session so that all repeated measurements by subject
#' appear in additional columns with names ending with with "*_s#" where "#" =
#' the session number, e.g., session_date_s1 is the date of that subject's first
#' session. The wide form of the data has one row per subject.
#'
#' @seealso \link[tidyr]{pivot_wider}, \link[tidyr]{pivot_longer}
#'
#' @references
#' Robbins, T. (2002). The 5-choice serial reaction time task: behavioural
#' pharmacology and functional neurochemistry. Psychopharmacology, 163(3),
#' 362-380.
#'
#' @export
fcsrt_pivot <- function(.df) {
  if(nrow(.df) > ncol(.df)) {
    out <- .df |>
      dplyr::mutate(session = paste0("s", as.character(session))) |>
      tidyr::pivot_wider(
        names_from = session, values_from = c(session_date, n_trials:tidyselect::last_col()),
        names_glue = "{.value}_{session}")
    out <- wash_df(out, clean_names = FALSE) |>
      dplyr::arrange(subject)
  } else {
    out <- .df |>
      tidyr::pivot_longer(cols = c(-subject),
                          names_to = c(".value", "session"),
                          names_pattern = "(.*)_s([0123456789]{0,})") |>
      dplyr::relocate(session_date, .after = subject) |>
      dplyr::filter(!is.na(total_choices))
    out <- wash_df(out, clean_names = FALSE) |>
      dplyr::arrange(subject, session)
  }
  return(out)
}

#' Prepare a session-aggregated version of cued or uncued RGT data for analysis.
#'
#' Prepare an analytical version of session-aggregated data from the cued
#' (Barrus & Winstanley, 2016) or uncued (Zeeb et al., 2009) version of the rat
#' gambling task (RGT) that has been parsed by [rgt_read()] or
#' [rgt_read_file()]. Either the long or wide forms of the data can be generated
#' and there is a convenience option to export the data to a csv file.
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr if_else
#' @importFrom dplyr case_when
#' @importFrom dplyr join_by
#' @importFrom dplyr relocate
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_wider
#' @importFrom readr write_csv
#' @importFrom tidyselect last_col
#' @importFrom stringr str_detect
#'
#' @param .df A data frame of cued or uncued rGT data that has been imported
#'   into R using either [rgt_read()] or [rgt_read_file()].
#'
#' @param shape Aggregation format or shape to use. The default, "long" format,
#'   organizes the data with as one column per variable and represents repeated
#'   measurements for experimental subjects as rows. This "tidy" format is
#'   commonly required for analyses performed using R. The alternative "wide"
#'   format instead uses additional columns to represent repeated measurements
#'   for experimental subjects (with one row per subject). The wide format is
#'   commonly required for analyses performed using other programs like SPSS.
#'
#' @param output_file Comma-separated variable (csv) file path and name to
#'   export the combined data to, which should end in ".csv". e.g.
#'   "rgt_data.csv" to save the file in your working directory.
#'
#' @param recode_session_by_date If TRUE (default) re-codes the session variable
#'   based on the session dates for each rat in chronological order (from the
#'   perspective of the rat). Set this to FALSE if you want to retain the
#'   original session numbers from the raw MedPC file.
#'
#' @return A data frame containing analysis-ready RGT data with the following columns
#'
#' \itemize{
#'   \item **subject:** rat ID number
#'   \item **group:** experimental group (only valid if entered into MedPC)
#'   \item **session_date:** session date (yyyy-mm-dd) according to the MedPC file
#'   \item **session:** session number (subject's perspective)
#'   \item **msn:** the MedPC program that was run during the session
#'   \item **n_trials:** number of trials initiated in the session
#'   \item **premat_n:** number of premature responses in the session
#'   \item **premat_prop:** premat_tot/n_trials = premature response rate (for the session)
#'   \item **total_choices:** number of choices made in the session (successfully completed trials)
#'   \item **P1:** proportion of total_choices where option P1 was selected (P1 preference)
#'   \item **P2:** proportion of total_choices where option P2 was selected (P2 preference)
#'   \item **P3:** proportion of total_chocies where option P3 was selected (P3 preference)
#'   \item **P4:** proportion of total_chocies where option P4 was selected (P4 preference)
#'   \item **choice_score:** overall choice preference score = (P1 + P2) - (P3 + P4)
#'   \item **omissions:** number of omissions in the session (where the rat did not chose an option)
#'   \item **mean_choice_lat:** mean time to choose an option on trials in the session when any choice (P1, P2, P3, or P4) was made
#'   \item **choice_lat_P1:** mean choice latency for trials when P1 was chosen
#'   \item **choice_lat_P2:** mean choice latency for trials when P2 was chosen
#'   \item **choice_lat_P3:** mean choice latency for trials when P3 was chosen
#'   \item **choice_lat_P4:** mean choice latency for trials when P4 was chosen
#'   \item **mean_collect_lat:** mean time to nose poke for sugar pellets when the rat was rewarded
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
iti5_prep <- function(.df, shape = c("long", "wide"),
                     output_file = NULL, recode_session_by_date = TRUE) {
  shape <- match.arg(shape)

  .df <- dplyr::rename(.df, session_date = start_date)

  if(recode_session_by_date == TRUE) {
    .df <- .df |>
      dplyr::arrange(subject, session_date) |>
      dplyr::mutate(session = translate(session_date,
                                        sort(unique(session_date)),
                                        seq_along(sort(unique(session_date)))))
  }

  #premature responding & total trials
  premature_by_session <- .df |>
    dplyr::select(subject, session_date, session, trial, premature_resp) |>
    dplyr::summarise(session_date = as.Date(mcv(session_date, na.rm = TRUE)),
                     n_trials = sum(!is.na(trial)),
                     premat_n = sum(premature_resp, na.rm = TRUE),
                     premat_prop = round(premat_n/n_trials, 4),
                     premat_pct = premat_prop*100,
                     .by = c(subject, session)) |>
    dplyr::arrange(subject, session)

  premature_by_hole <- .df |>
    dplyr::filter(premature_hole %in% 1:5) |>
    dplyr::select(subject, session, premature_hole, premature_resp) |>
    dplyr::summarise(n_premat = sum(premature_resp, na.rm = TRUE),
                     .by = c(subject, session, premature_hole)) |>
    dplyr::arrange(subject, session, premature_hole) |>
    tidyr::pivot_wider(names_from = "premature_hole", values_from = "n_premat", names_prefix = "n_premat_h",
                       values_fill = 0)

  premat <-  suppressMessages(dplyr::left_join(premature_by_session, premature_by_hole))

  #omissions
  omit <- .df |>
    dplyr::filter(msn %in% c("rGT_classicA", "rGT_classicB", "rGT_A-cue", "rGT_B-cue", "RevRGT_A-cue", "RevRGT_B-cue")) |>
    dplyr::summarise(omissions = sum(omission, na.rm = TRUE), .by = c(subject, session))

  #choice latency
  choice_lat_by_hole <- .df |>
    dplyr::filter(chosen != 0, msn %in% c("rGT_classicA", "rGT_classicB", "rGT_A-cue", "rGT_B-cue", "RevRGT_A-cue", "RevRGT_B-cue")) |>
    dplyr::select(subject, msn, group, session, choice_lat, chosen) |>
    dplyr::mutate(chosen = dplyr::case_when(stringr::str_detect(msn, "A") & chosen == 1 ~ "P1",
                                            stringr::str_detect(msn, "A") & chosen == 2 ~ "P4",
                                            stringr::str_detect(msn, "A") & chosen == 4 ~ "P2",
                                            stringr::str_detect(msn, "A") & chosen == 5 ~ "P3",
                                            stringr::str_detect(msn, "B") & chosen == 1 ~ "P4",
                                            stringr::str_detect(msn, "B") & chosen == 2 ~ "P1",
                                            stringr::str_detect(msn, "B") & chosen == 4 ~ "P3",
                                            stringr::str_detect(msn, "B") & chosen == 5 ~ "P2"),
                  chosen = factor(chosen, levels = c("P1", "P2", "P3", "P4"))) |>
    dplyr::summarise(mean_choice_lat = mean(choice_lat, na.rm = TRUE), .by = c(subject, session, chosen)) |>
    dplyr::arrange(subject, session, chosen) |>
    tidyr::pivot_wider(names_from = "chosen", values_from = "mean_choice_lat",
                       names_prefix = "choice_lat_", names_expand = TRUE)

  choice_lat_session_avg <- .df |>
    dplyr::filter(chosen != 0, msn %in% c("rGT_classicA", "rGT_classicB", "rGT_A-cue", "rGT_B-cue", "RevRGT_A-cue", "RevRGT_B-cue")) |>
    dplyr::select(subject, msn, group, session, choice_lat) |>
    dplyr::summarise(mean_choice_lat = mean(choice_lat, na.rm = TRUE), .by = c(subject, session)) |>
    dplyr::arrange(subject, session)

  choice_lat <- suppressMessages(dplyr::left_join(choice_lat_session_avg, choice_lat_by_hole))

  #collection latency
  collection_lat <- .df |>
    dplyr::filter(chosen != 0, msn %in% c("rGT_classicA", "rGT_classicB", "rGT_A-cue", "rGT_B-cue", "RevRGT_A-cue", "RevRGT_B-cue")) |>
    dplyr::select(subject, msn, group, session, collect_lat, chosen) |>
    dplyr::summarise(mean_collect_lat = mean(collect_lat, na.rm = TRUE),
                     .by = c(subject, msn, group, session))

  #choice score
  choice  <- .df |>
    dplyr::filter(msn %in% c("rGT_classicA", "rGT_classicB", "rGT_A-cue", "rGT_B-cue", "RevRGT_A-cue", "RevRGT_B-cue")) |>
    dplyr::mutate(chosen = dplyr::case_when(stringr::str_detect(msn, "A") & chosen == 1 ~ "P1",
                                            stringr::str_detect(msn, "A") & chosen == 2 ~ "P4",
                                            stringr::str_detect(msn, "A") & chosen == 4 ~ "P2",
                                            stringr::str_detect(msn, "A") & chosen == 5 ~ "P3",
                                            stringr::str_detect(msn, "B") & chosen == 1 ~ "P4",
                                            stringr::str_detect(msn, "B") & chosen == 2 ~ "P1",
                                            stringr::str_detect(msn, "B") & chosen == 4 ~ "P3",
                                            stringr::str_detect(msn, "B") & chosen == 5 ~ "P2"),
                  chosen = factor(chosen, levels = c("P1", "P2", "P3", "P4"))) |>
    dplyr::filter(!is.na(chosen)) |>
    dplyr::mutate(version = dplyr::if_else(stringr::str_detect(msn, "A"), "A", "B")) |>
    dplyr::select(version, subject, session, trial, chosen) |>
    dplyr::distinct() |>
    dplyr::mutate(times_chosen = dplyr::n(), .by = c(subject, session, chosen)) |>
    dplyr::mutate(total_choices = dplyr::n(),
                  choice_prop = round(times_chosen/total_choices, 4),
                  .by = c(subject, session)) |>
    dplyr::select(subject, session, chosen, total_choices, choice_prop) |>
    dplyr::arrange(subject, session, chosen) |>
    dplyr::distinct() |>
    tidyr::pivot_wider(names_from = chosen,
                       values_from = c("choice_prop"),
                       values_fill = 0, names_expand = TRUE) |>
    dplyr::mutate(choice_score = ((P1 + P2) - (P3 + P4)))

  # combine datasets
  suppressMessages(
    df_all <- premat |>
      dplyr::full_join(choice) |>
      dplyr::full_join(omit) |>
      dplyr::full_join(choice_lat) |>
      dplyr::full_join(collection_lat) |>
      dplyr::select(subject, group, session_date, session, msn, n_trials,
                    premat_n, premat_prop, total_choices:P4, choice_score,
                    omissions, mean_choice_lat, choice_lat_P1:choice_lat_P4, mean_collect_lat)
  )

  if(shape == "wide") {
    df_all <- df_all |>
      dplyr::mutate(session = paste0("s", as.character(session))) |>
      tidyr::pivot_wider(names_from = session, values_from = c(session_date, msn:tidyselect::last_col()),
                         names_glue = "{.value}_{session}") |>
      dplyr::arrange(subject)
  } else {
    df_all <- dplyr::arrange(df_all, subject, session)
  }
  if(!missing(output_file)) {
    if(!grepl("\\.csv$", output_file)) {
      stop('output file name must end in ".csv"')
    }
    message("writing combined data to disk")

    df_all |> readr::write_csv(output_file)
  }
  return(df_all)
}

#' Convert a session-aggregated version of data from the cued (Barrus &
#' Winstanley, 2016) or uncued (Zeeb et al., 2009) rat gambling task (RGT) that
#' has been parsed by [rgt_read()] or [rgt_read_file()] and prepared by
#' [rgt_prep()] from wide format to long format or vice versa based on its
#' current shape. For more information on pivoting data frames between long and
#' wide formats, see the
#' [pivoting](https://tidyr.tidyverse.org/articles/pivot.html) article on the
#' tidyr website.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr relocate
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect last_col
#'
#' @param .df A data frame of RGT data that has been imported into R using
#'   either [rgt_read()] or [rgt_read_file()] and
#'   prepared by [rgt_prep()].
#'
#' @return A data frame containing session-aggregated RGT data with the
#'   following columns, for the long format/shape (one column per variable):
#'
#' \itemize{
#'   \item **subject:** rat ID number
#'   \item **group:** experimental group (only valid if entered into MedPC)
#'   \item **session_date:** session date (yyyy-mm-dd) according to the MedPC file
#'   \item **session:** session number (subject's perspective)
#'   \item **msn:** the MedPC program that was run during the session
#'   \item **n_trials:** number of trials initiated in the session
#'   \item **premat_n:** number of premature responses in the session
#'   \item **premat_prop:** premat_tot/n_trials = premature response rate (for the session)
#'   \item **total_choices:** number of choices made in the session (successfully completed trials)
#'   \item **P1:** proportion of total_choices where option P1 was selected (P1 preference)
#'   \item **P2:** proportion of total_choices where option P2 was selected (P2 preference)
#'   \item **P3:** proportion of total_chocies where option P3 was selected (P3 preference)
#'   \item **P4:** proportion of total_chocies where option P4 was selected (P4 preference)
#'   \item **choice_score:** overall choice preference score = (P1 + P2) - (P3 + P4)
#'   \item **omissions:** number of omissions in the session (where the rat did not chose an option)
#'   \item **mean_choice_lat:** mean time to choose an option on trials in the session when any choice (P1, P2, P3, or P4) was made
#'   \item **choice_lat_P1:** mean choice latency for trials when P1 was chosen
#'   \item **choice_lat_P2:** mean choice latency for trials when P2 was chosen
#'   \item **choice_lat_P3:** mean choice latency for trials when P3 was chosen
#'   \item **choice_lat_P4:** mean choice latency for trials when P4 was chosen
#'   \item **mean_collect_lat:** mean time to nose poke for sugar pellets when the rat was rewarded
#' }
#'
#' The wide format/shape contains the same session-aggregated variables that
#' have been pivoted by session so that all repeated measurements by subject
#' appear in additional columns with names ending with with "*_s#" where "#" =
#' the session number, e.g., session_date_s1 is the date of that subject's first
#' session. The wide form of the data has one row per subject.
#'
#' @seealso \link[tidyr]{pivot_wider}, \link[tidyr]{pivot_longer}
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
iti5_pivot <- function(.df) {
  if(nrow(.df) > ncol(.df)) {
    out <- .df |>
      dplyr::mutate(session = paste0("s", as.character(session))) |>
      tidyr::pivot_wider(names_from = session, values_from = c(session_date, msn:tidyselect::last_col()),
                         names_glue = "{.value}_{session}")
    out <- wash_df(out, clean_names = FALSE) |>
      dplyr::arrange(subject)
  } else {
    out <- .df |>
      tidyr::pivot_longer(cols = c(-subject, -group),
                          names_to = c(".value", "session"),
                          names_pattern = "(.*)_s([0123456789]{0,})") |>
      dplyr::relocate(session_date, .after = subject) |>
      dplyr::filter(!is.na(n_trials))
    out <- wash_df(out, clean_names = FALSE) |>
      dplyr::arrange(subject, session)
  }
  return(out)
}

#' Prepare a session-aggregated version of ITI9 RGT data for analysis.
#'
#' Prepare an analytical version of session-aggregated data from the cued
#' (Barrus & Winstanley, 2016) or uncued (Zeeb et al., 2009) version of the rat
#' gambling task (RGT) that has been parsed by [iti9_read()] or
#' [iti9_read_file()]. Either the long or wide forms of the data can be generated
#' and there is a convenience option to export the data to a csv file.
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr if_else
#' @importFrom dplyr case_when
#' @importFrom dplyr join_by
#' @importFrom dplyr relocate
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_wider
#' @importFrom readr write_csv
#' @importFrom tidyselect last_col
#' @importFrom stringr str_detect
#'
#' @param .df A data frame of 5CSRT data that has been imported into R using
#'   either [iti9_read()] or [iti9_read_file()].
#'
#' @param shape Aggregation format or shape to use. The default, "long" format,
#'   organizes the data with as one column per variable and represents repeated
#'   measurements for experimental subjects as rows. This "tidy" format is
#'   commonly required for analyses performed using R. The alternative "wide"
#'   format instead uses additional columns to represent repeated measurements
#'   for experimental subjects (with one row per subject). The wide format is
#'   commonly required for analyses performed using other programs like SPSS.
#'
#' @param output_file Comma-separated variable (csv) file path and name to
#'   export the combined data to, which should end in ".csv". e.g.
#'   "rgt_data.csv" to save the file in your working directory.
#'
#' @param recode_session_by_date If TRUE (default) re-codes the session variable
#'   based on the session dates for each rat in chronological order (from the
#'   perspective of the rat). Set this to FALSE if you want to retain the
#'   original session numbers from the raw MedPC file.
#'
#' @return A data frame containing analysis-ready RGT data with the following columns
#'
#' \itemize{
#'   \item **subject:** rat ID number
#'   \item **group:** experimental group (only valid if entered into MedPC)
#'   \item **session_date:** session date (yyyy-mm-dd) according to the MedPC file
#'   \item **session:** session number (subject's perspective)
#'   \item **msn:** the MedPC program that was run during the session
#'   \item **n_trials:** number of trials initiated in the session
#'   \item **premat_n:** number of premature responses in the session
#'   \item **premat_prop:** premat_tot/n_trials = premature response rate (for the session)
#'   \item **mean_premat_time:** mean time of premature responses during the session
#'   \item **total_choices:** number of choices made in the session (successfully completed trials)
#'   \item **P1:** proportion of total_choices where option P1 was selected (P1 preference)
#'   \item **P2:** proportion of total_choices where option P2 was selected (P2 preference)
#'   \item **P3:** proportion of total_chocies where option P3 was selected (P3 preference)
#'   \item **P4:** proportion of total_chocies where option P4 was selected (P4 preference)
#'   \item **choice_score:** overall choice preference score = (P1 + P2) - (P3 + P4)
#'   \item **omissions:** number of omissions in the session (where the rat did not chose an option)
#'   \item **mean_choice_lat:** mean time to choose an option on trials in the session when any choice (P1, P2, P3, or P4) was made
#'   \item **choice_lat_P1:** mean choice latency for trials when P1 was chosen
#'   \item **choice_lat_P2:** mean choice latency for trials when P2 was chosen
#'   \item **choice_lat_P3:** mean choice latency for trials when P3 was chosen
#'   \item **choice_lat_P4:** mean choice latency for trials when P4 was chosen
#'   \item **mean_collect_lat:** mean time to nose poke for sugar pellets when the rat was rewarded
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
iti9_prep <- function(.df, shape = c("long", "wide"),
                      output_file = NULL, recode_session_by_date = TRUE) {
  shape <- match.arg(shape)

  .df <- dplyr::rename(.df, session_date = start_date)

  if(recode_session_by_date == TRUE) {
    .df <- .df |>
      dplyr::arrange(subject, session_date) |>
      dplyr::mutate(session = translate(session_date,
                                        sort(unique(session_date)),
                                        seq_along(sort(unique(session_date)))))
  }

  #premature responding & total trials
  premature_by_session <- .df |>
    dplyr::select(subject, session_date, session, trial, premature_resp, premature_time) |>
    dplyr::summarise(session_date = as.Date(mcv(session_date, na.rm = TRUE)),
                     n_trials = sum(!is.na(trial)),
                     premat_n = sum(premature_resp, na.rm = TRUE),
                     premat_prop = round(premat_n/n_trials, 4),
                     premat_pct = premat_prop*100,
                     mean_premat_time = mean(premature_time, na.rm = TRUE),
                     .by = c(subject, session)) |>
    dplyr::arrange(subject, session)

  premature_by_hole <- .df |>
    dplyr::filter(premature_hole %in% 1:5) |>
    dplyr::select(subject, session, premature_hole, premature_resp) |>
    dplyr::summarise(n_premat = sum(premature_resp, na.rm = TRUE),
                     .by = c(subject, session, premature_hole)) |>
    dplyr::arrange(subject, session, premature_hole) |>
    tidyr::pivot_wider(names_from = "premature_hole", values_from = "n_premat", names_prefix = "n_premat_h",
                       values_fill = 0)

  premat <-  suppressMessages(dplyr::left_join(premature_by_session, premature_by_hole))

  #omissions
  omit <- .df |>
    dplyr::filter(stringr::str_detect(msn, "I9|ITI9")) |>
    dplyr::summarise(omissions = sum(omission, na.rm = TRUE), .by = c(subject, session))

  #choice latency
  choice_lat_by_hole <- .df |>
    dplyr::filter(chosen != 0, stringr::str_detect(msn, "I9|ITI9")) |>
    dplyr::select(subject, msn, group, session, choice_lat, chosen) |>
    dplyr::mutate(chosen = dplyr::case_when(stringr::str_detect(msn, "A") & chosen == 1 ~ "P1",
                                            stringr::str_detect(msn, "A") & chosen == 2 ~ "P4",
                                            stringr::str_detect(msn, "A") & chosen == 4 ~ "P2",
                                            stringr::str_detect(msn, "A") & chosen == 5 ~ "P3",
                                            stringr::str_detect(msn, "B") & chosen == 1 ~ "P4",
                                            stringr::str_detect(msn, "B") & chosen == 2 ~ "P1",
                                            stringr::str_detect(msn, "B") & chosen == 4 ~ "P3",
                                            stringr::str_detect(msn, "B") & chosen == 5 ~ "P2"),
                  chosen = factor(chosen, levels = c("P1", "P2", "P3", "P4"))) |>
    dplyr::summarise(mean_choice_lat = mean(choice_lat, na.rm = TRUE), .by = c(subject, session, chosen)) |>
    dplyr::arrange(subject, session, chosen) |>
    tidyr::pivot_wider(names_from = "chosen", values_from = "mean_choice_lat",
                       names_prefix = "choice_lat_", names_expand = TRUE)

  choice_lat_session_avg <- .df |>
    dplyr::filter(chosen != 0, stringr::str_detect(msn, "I9|ITI9")) |>
    dplyr::select(subject, msn, group, session, choice_lat) |>
    dplyr::summarise(mean_choice_lat = mean(choice_lat, na.rm = TRUE), .by = c(subject, session)) |>
    dplyr::arrange(subject, session)

  choice_lat <- suppressMessages(dplyr::left_join(choice_lat_session_avg, choice_lat_by_hole))

  #collection latency
  collection_lat <- .df |>
    dplyr::filter(chosen != 0) |>
    dplyr::select(subject, msn, group, session, collect_lat, chosen) |>
    dplyr::filter(stringr::str_detect(msn, "I9|ITI9")) |>
    dplyr::summarise(mean_collect_lat = mean(collect_lat, na.rm = TRUE),
                     .by = c(subject, msn, group, session))

  #choice score
  choice  <- .df |>
    dplyr::filter(stringr::str_detect(msn, "I9|ITI9")) |>
    dplyr::mutate(chosen = dplyr::case_when(stringr::str_detect(msn, "A") & chosen == 1 ~ "P1",
                                            stringr::str_detect(msn, "A") & chosen == 2 ~ "P4",
                                            stringr::str_detect(msn, "A") & chosen == 4 ~ "P2",
                                            stringr::str_detect(msn, "A") & chosen == 5 ~ "P3",
                                            stringr::str_detect(msn, "B") & chosen == 1 ~ "P4",
                                            stringr::str_detect(msn, "B") & chosen == 2 ~ "P1",
                                            stringr::str_detect(msn, "B") & chosen == 4 ~ "P3",
                                            stringr::str_detect(msn, "B") & chosen == 5 ~ "P2"),
                  chosen = factor(chosen, levels = c("P1", "P2", "P3", "P4"))) |>
    dplyr::filter(!is.na(chosen)) |>
    dplyr::mutate(version = dplyr::if_else(stringr::str_detect(msn, "A"), "A", "B")) |>
    dplyr::select(version, subject, session, trial, chosen) |>
    dplyr::distinct() |>
    dplyr::mutate(times_chosen = dplyr::n(), .by = c(subject, session, chosen)) |>
    dplyr::mutate(total_choices = dplyr::n(),
                  choice_prop = round(times_chosen/total_choices, 4),
                  .by = c(subject, session)) |>
    dplyr::select(subject, session, chosen, total_choices, choice_prop) |>
    dplyr::arrange(subject, session, chosen) |>
    dplyr::distinct() |>
    tidyr::pivot_wider(names_from = chosen,
                       values_from = c("choice_prop"),
                       values_fill = 0, names_expand = TRUE) |>
    dplyr::mutate(choice_score = ((P1 + P2) - (P3 + P4)))

  # combine datasets
  suppressMessages(
    df_all <- premat |>
      dplyr::full_join(choice) |>
      dplyr::full_join(omit) |>
      dplyr::full_join(choice_lat) |>
      dplyr::full_join(collection_lat) |>
      dplyr::select(subject, group, session_date, session, msn, n_trials,
                    premat_n, premat_prop, mean_premat_time, total_choices:P4, choice_score,
                    omissions, mean_choice_lat, choice_lat_P1:choice_lat_P4, mean_collect_lat)
  )

  if(shape == "wide") {
    df_all <- df_all |>
      dplyr::mutate(session = paste0("s", as.character(session))) |>
      tidyr::pivot_wider(names_from = session, values_from = c(session_date, msn:tidyselect::last_col()),
                         names_glue = "{.value}_{session}") |>
      dplyr::arrange(subject)
  } else {
    df_all <- dplyr::arrange(df_all, subject, session)
  }
  if(!missing(output_file)) {
    if(!grepl("\\.csv$", output_file)) {
      stop('output file name must end in ".csv"')
    }
    message("writing combined data to disk")

    df_all |> readr::write_csv(output_file)
  }
  return(df_all)
}

#' Convert a session-aggregated version of data from the cued (Barrus &
#' Winstanley, 2016) or uncued (Zeeb et al., 2009) rat gambling task (RGT) that
#' has been parsed by [iti9_read()] or [iti9_read_file()] and prepared by
#' [iti9_prep()] from wide format to long format or vice versa based on its
#' current shape. For more information on pivoting data frames between long and
#' wide formats, see the
#' [pivoting](https://tidyr.tidyverse.org/articles/pivot.html) article on the
#' tidyr website.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr relocate
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect last_col
#'
#' @param .df A data frame of RGT data that has been imported into R using
#'   either [iti9_read()] or [iti9_read_file()] and
#'   prepared by [iti9_prep()].
#'
#' @return A data frame containing session-aggregated RGT data with the
#'   following columns, for the long format/shape (one column per variable):
#'
#' \itemize{
#'   \item **subject:** rat ID number
#'   \item **group:** experimental group (only valid if entered into MedPC)
#'   \item **session_date:** session date (yyyy-mm-dd) according to the MedPC file
#'   \item **session:** session number (subject's perspective)
#'   \item **msn:** the MedPC program that was run during the session
#'   \item **n_trials:** number of trials initiated in the session
#'   \item **premat_n:** number of premature responses in the session
#'   \item **premat_prop:** premat_tot/n_trials = premature response rate (for the session)
#'   \item **mean_premat_time:** mean time of premature responses during the session
#'   \item **total_choices:** number of choices made in the session (successfully completed trials)
#'   \item **P1:** proportion of total_choices where option P1 was selected (P1 preference)
#'   \item **P2:** proportion of total_choices where option P2 was selected (P2 preference)
#'   \item **P3:** proportion of total_chocies where option P3 was selected (P3 preference)
#'   \item **P4:** proportion of total_chocies where option P4 was selected (P4 preference)
#'   \item **choice_score:** overall choice preference score = (P1 + P2) - (P3 + P4)
#'   \item **omissions:** number of omissions in the session (where the rat did not chose an option)
#'   \item **mean_choice_lat:** mean time to choose an option on trials in the session when any choice (P1, P2, P3, or P4) was made
#'   \item **choice_lat_P1:** mean choice latency for trials when P1 was chosen
#'   \item **choice_lat_P2:** mean choice latency for trials when P2 was chosen
#'   \item **choice_lat_P3:** mean choice latency for trials when P3 was chosen
#'   \item **choice_lat_P4:** mean choice latency for trials when P4 was chosen
#'   \item **mean_collect_lat:** mean time to nose poke for sugar pellets when the rat was rewarded
#' }
#'
#' The wide format/shape contains the same session-aggregated variables that
#' have been pivoted by session so that all repeated measurements by subject
#' appear in additional columns with names ending with with "*_s#" where "#" =
#' the session number, e.g., session_date_s1 is the date of that subject's first
#' session. The wide form of the data has one row per subject.
#'
#' @seealso \link[tidyr]{pivot_wider}, \link[tidyr]{pivot_longer}
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
iti9_pivot <- function(.df) {
  if(nrow(.df) > ncol(.df)) {
    out <- .df |>
      dplyr::mutate(session = paste0("s", as.character(session))) |>
      tidyr::pivot_wider(names_from = session, values_from = c(session_date, msn:tidyselect::last_col()),
                         names_glue = "{.value}_{session}")
    out <- wash_df(out, clean_names = FALSE) |>
      dplyr::arrange(subject)
  } else {
    out <- .df |>
      tidyr::pivot_longer(cols = c(-subject, -group),
                          names_to = c(".value", "session"),
                          names_pattern = "(.*)_s([0123456789]{0,})") |>
      dplyr::relocate(session_date, .after = subject) |>
      dplyr::filter(!is.na(n_trials))
    out <- wash_df(out, clean_names = FALSE) |>
      dplyr::arrange(subject, session)
  }
  return(out)
}

#' Prepare a session-aggregated version of ITI9 RGT data for analysis.
#'
#' Prepare an analytical version of session-aggregated data from the cued
#' (Barrus & Winstanley, 2016) or uncued (Zeeb et al., 2009) version of the rat
#' gambling task (RGT) that has been parsed by [ddrgt_read()] or
#' [ddrgt_read_file()]. Either the long or wide forms of the data can be generated
#' and there is a convenience option to export the data to a csv file.
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr if_else
#' @importFrom dplyr case_when
#' @importFrom dplyr join_by
#' @importFrom dplyr relocate
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_wider
#' @importFrom readr write_csv
#' @importFrom tidyselect last_col
#'
#' @param .df A data frame of delay discounting RGT data that has been imported
#'   into R using either [ddrgt_read()] or [ddrgt_read_file()].
#'
#' @param shape Aggregation format or shape to use. The default, "long" format,
#'   organizes the data with as one column per variable and represents repeated
#'   measurements for experimental subjects as rows. This "tidy" format is
#'   commonly required for analyses performed using R. The alternative "wide"
#'   format instead uses additional columns to represent repeated measurements
#'   for experimental subjects (with one row per subject). The wide format is
#'   commonly required for analyses performed using other programs like SPSS.
#'
#' @param output_file Comma-separated variable (csv) file path and name to
#'   export the combined data to, which should end in ".csv". e.g.
#'   "rgt_data.csv" to save the file in your working directory.
#'
#' @param recode_session_by_date If TRUE (default) re-codes the session variable
#'   based on the session dates for each rat in chronological order (from the
#'   perspective of the rat). Set this to FALSE if you want to retain the
#'   original session numbers from the raw MedPC file.
#'
#' @return A data frame containing analysis-ready RGT data with the following columns
#'
#' \itemize{
#'   \item **subject:** rat ID number
#'   \item **group:** experimental group (only valid if entered into MedPC)
#'   \item **session_date:** session date (yyyy-mm-dd) according to the MedPC file
#'   \item **session:** session number (subject's perspective)
#'   \item **msn:** the MedPC program that was run during the session
#'   \item **n_trials:** number of trials initiated in the session
#'   \item **premat_n:** number of premature responses in the session
#'   \item **premat_prop:** premat_tot/n_trials = premature response rate (for the session)
#'   \item **mean_premat_time:** mean premature response time for the session
#'   \item **total_choices:** number of choices made in the session (successfully completed trials)
#'   \item **P1:** proportion of total_choices where option P1 was selected (P1 preference)
#'   \item **P2:** proportion of total_choices where option P2 was selected (P2 preference)
#'   \item **P3:** proportion of total_chocies where option P3 was selected (P3 preference)
#'   \item **P4:** proportion of total_chocies where option P4 was selected (P4 preference)
#'   \item **choice_score:** overall choice preference score = (P1 + P2) - (P3 + P4)
#'   \item **omissions:** number of omissions in the session (where the rat did not chose an option)
#'   \item **mean_choice_lat:** mean time to choose an option on trials in the session when any choice (P1, P2, P3, or P4) was made
#'   \item **choice_lat_P1:** mean choice latency for trials when P1 was chosen
#'   \item **choice_lat_P2:** mean choice latency for trials when P2 was chosen
#'   \item **choice_lat_P3:** mean choice latency for trials when P3 was chosen
#'   \item **choice_lat_P4:** mean choice latency for trials when P4 was chosen
#'   \item **mean_collect_lat:** mean time to nose poke for sugar pellets when the rat was rewarded
#'   \item **dd_phenotype:** the targeted delay discounting phenotype (ideal or worsening)
#'   \item **iti_dur_P1:** ITI duration used for P1 during the session
#'   \item **iti_dur_P2:** ITI duration used for P2 during the session
#'   \item **iti_dur_P3:** ITI duration used for P3 during the session
#'   \item **iti_dur_P4:** ITI duration used for P4 during the session
#'   \item **mean_choice_iti:** mean ITI of options chosen during the session
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
ddrgt_prep <- function(.df, shape = c("long", "wide"),
                       output_file = NULL, recode_session_by_date = TRUE) {
  shape <- match.arg(shape)

  .df <- dplyr::rename(.df, session_date = start_date)

  if(recode_session_by_date == TRUE) {
    .df <- .df |>
      dplyr::arrange(subject, session_date) |>
      dplyr::mutate(session = translate(session_date,
                                        sort(unique(session_date)),
                                        seq_along(sort(unique(session_date)))))
  }

  #premature responding & total trials
  premature_by_session <- .df |>
    dplyr::select(subject, session_date, session, trial, premature_resp, premature_time) |>
    dplyr::summarise(session_date = as.Date(mcv(session_date, na.rm = TRUE)),
                     n_trials = sum(!is.na(trial)),
                     premat_n = sum(premature_resp, na.rm = TRUE),
                     premat_prop = round(premat_n/n_trials, 4),
                     premat_pct = premat_prop*100,
                     mean_premat_time = mean(premature_time, na.rm = TRUE),
                     .by = c(subject, session)) |>
    dplyr::arrange(subject, session)

  premature_by_hole <- .df |>
    dplyr::filter(premature_hole %in% 1:5) |>
    dplyr::select(subject, session, premature_hole, premature_resp) |>
    dplyr::summarise(n_premat = sum(premature_resp, na.rm = TRUE),
                     .by = c(subject, session, premature_hole)) |>
    dplyr::arrange(subject, session, premature_hole) |>
    tidyr::pivot_wider(names_from = "premature_hole", values_from = "n_premat", names_prefix = "n_premat_h",
                       values_fill = 0)

  premat <-  suppressMessages(dplyr::left_join(premature_by_session, premature_by_hole))

  #omissions
  omit <- .df |>
    dplyr::filter(stringr::str_detect(msn, "DDrGT_A|DDrGT_B")) |>
    dplyr::summarise(omissions = sum(omission, na.rm = TRUE), .by = c(subject, session))

  #choice latency
  choice_lat_by_hole <- .df |>
    dplyr::filter(chosen != 0, stringr::str_detect(msn, "DDrGT_A|DDrGT_B")) |>
    dplyr::select(subject, msn, group, session, choice_lat, chosen) |>
    dplyr::mutate(chosen = dplyr::case_when(stringr::str_detect(msn, "A") & chosen == 1 ~ "P1",
                                            stringr::str_detect(msn, "A") & chosen == 2 ~ "P4",
                                            stringr::str_detect(msn, "A") & chosen == 4 ~ "P2",
                                            stringr::str_detect(msn, "A") & chosen == 5 ~ "P3",
                                            stringr::str_detect(msn, "B") & chosen == 1 ~ "P4",
                                            stringr::str_detect(msn, "B") & chosen == 2 ~ "P1",
                                            stringr::str_detect(msn, "B") & chosen == 4 ~ "P3",
                                            stringr::str_detect(msn, "B") & chosen == 5 ~ "P2"),
                  chosen = factor(chosen, levels = c("P1", "P2", "P3", "P4"))) |>
    dplyr::summarise(mean_choice_lat = mean(choice_lat, na.rm = TRUE), .by = c(subject, session, chosen)) |>
    dplyr::arrange(subject, session, chosen) |>
    tidyr::pivot_wider(names_from = "chosen", values_from = "mean_choice_lat",
                       names_prefix = "choice_lat_", names_expand = TRUE)

  choice_lat_session_avg <- .df |>
    dplyr::filter(chosen != 0, stringr::str_detect(msn, "DDrGT_A|DDrGT_B")) |>
    dplyr::select(subject, msn, group, session, choice_lat) |>
    dplyr::summarise(mean_choice_lat = mean(choice_lat, na.rm = TRUE), .by = c(subject, session)) |>
    dplyr::arrange(subject, session)

  choice_lat <- suppressMessages(dplyr::left_join(choice_lat_session_avg, choice_lat_by_hole))

  #collection latency
  collection_lat <- .df |>
    dplyr::filter(chosen != 0, stringr::str_detect(msn, "DDrGT_A|DDrGT_B")) |>
    dplyr::select(subject, group, session, collect_lat, chosen) |>
    dplyr::summarise(mean_collect_lat = mean(collect_lat, na.rm = TRUE),
                     .by = c(subject, group, session))

  #choice score
  choice  <- .df |>
    dplyr::filter(stringr::str_detect(msn, "DDrGT_A|DDrGT_B")) |>
    dplyr::mutate(chosen = dplyr::case_when(stringr::str_detect(msn, "A") & chosen == 1 ~ "P1",
                                            stringr::str_detect(msn, "A") & chosen == 2 ~ "P4",
                                            stringr::str_detect(msn, "A") & chosen == 4 ~ "P2",
                                            stringr::str_detect(msn, "A") & chosen == 5 ~ "P3",
                                            stringr::str_detect(msn, "B") & chosen == 1 ~ "P4",
                                            stringr::str_detect(msn, "B") & chosen == 2 ~ "P1",
                                            stringr::str_detect(msn, "B") & chosen == 4 ~ "P3",
                                            stringr::str_detect(msn, "B") & chosen == 5 ~ "P2"),
                  chosen = factor(chosen, levels = c("P1", "P2", "P3", "P4"))) |>
    dplyr::filter(!is.na(chosen)) |>
    dplyr::mutate(version = dplyr::if_else(stringr::str_detect(msn, "DDrGT_A"), "A", "B")) |>
    dplyr::select(version, subject, session, trial, chosen) |>
    dplyr::distinct() |>
    dplyr::mutate(times_chosen = dplyr::n(), .by = c(subject, session, chosen)) |>
    dplyr::mutate(total_choices = dplyr::n(),
                  choice_prop = round(times_chosen/total_choices, 4),
                  .by = c(subject, session)) |>
    dplyr::select(subject, session, chosen, total_choices, choice_prop) |>
    dplyr::arrange(subject, session, chosen) |>
    dplyr::distinct() |>
    tidyr::pivot_wider(names_from = chosen,
                       values_from = c("choice_prop"),
                       values_fill = 0, names_expand = TRUE) |>
    dplyr::mutate(choice_score = ((P1 + P2) - (P3 + P4)))

  dd <- .df |>
    dplyr::filter(chosen != 0, stringr::str_detect(msn, "DDrGT_A|DDrGT_B")) |>
    dplyr::mutate(mean_choice_iti = mean(chosen_iti_dur, na.rm = TRUE),
                  .by = c(subject, group, session)) |>
    dplyr::select(subject, msn, session, iti_dur_p1:iti_dur_p4, mean_choice_iti) |>
    dplyr::distinct() |>
    dplyr::mutate(dd_phenotype = stringr::str_extract(msn, "ideal|worsening"), .after = session) |>
    dplyr::select(-msn) |>
    dplyr::arrange(subject, session)

  # combine datasets
  suppressMessages(
    df_all <- premat |>
      dplyr::full_join(choice) |>
      dplyr::full_join(omit) |>
      dplyr::full_join(choice_lat) |>
      dplyr::full_join(collection_lat) |>
      dplyr::full_join(dd) |>
      dplyr::select(subject, group, session_date, session, msn, n_trials,
                    premat_n, premat_prop, mean_premat_time,
                    total_choices:P4, choice_score,
                    omissions, mean_choice_lat, choice_lat_P1:choice_lat_P4,
                    mean_collect_lat, dd_phenotype, iti_dur_p1:iti_dur_p4, mean_choice_iti)
  )

  if(shape == "wide") {
    df_all <- df_all |>
      dplyr::mutate(session = paste0("s", as.character(session))) |>
      tidyr::pivot_wider(names_from = session, values_from = c(session_date, msn:tidyselect::last_col()),
                         names_glue = "{.value}_{session}") |>
      dplyr::arrange(subject)
  } else {
    df_all <- dplyr::arrange(df_all, subject, session)
  }
  if(!missing(output_file)) {
    if(!grepl("\\.csv$", output_file)) {
      stop('output file name must end in ".csv"')
    }
    message("writing combined data to disk")

    df_all |> readr::write_csv(output_file)
  }
  return(df_all)
}

#' Convert a session-aggregated version of data from the cued (Barrus &
#' Winstanley, 2016) or uncued (Zeeb et al., 2009) rat gambling task (RGT) that
#' has been parsed by [ddrgt_read()] or [ddrgt_read_file()] and prepared by
#' [ddrgt_prep()] from wide format to long format or vice versa based on its
#' current shape. For more information on pivoting data frames between long and
#' wide formats, see the
#' [pivoting](https://tidyr.tidyverse.org/articles/pivot.html) article on the
#' tidyr website.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr relocate
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect last_col
#'
#' @param .df A data frame of delay discounting RGT data that has been imported
#'   into R using either [ddrgt_read()] or [ddrgt_read_file()] and prepared by
#'   [ddrgt_prep()].
#'
#' @return A data frame containing session-aggregated RGT data with the
#'   following columns, for the long format/shape (one column per variable):
#'
#' \itemize{
#'   \item **subject:** rat ID number
#'   \item **group:** experimental group (only valid if entered into MedPC)
#'   \item **session_date:** session date (yyyy-mm-dd) according to the MedPC file
#'   \item **session:** session number (subject's perspective)
#'   \item **msn:** the MedPC program that was run during the session
#'   \item **n_trials:** number of trials initiated in the session
#'   \item **premat_n:** number of premature responses in the session
#'   \item **premat_prop:** premat_tot/n_trials = premature response rate (for the session)
#'   \item **mean_premat_time:** mean premature response time for the session
#'   \item **total_choices:** number of choices made in the session (successfully completed trials)
#'   \item **P1:** proportion of total_choices where option P1 was selected (P1 preference)
#'   \item **P2:** proportion of total_choices where option P2 was selected (P2 preference)
#'   \item **P3:** proportion of total_chocies where option P3 was selected (P3 preference)
#'   \item **P4:** proportion of total_chocies where option P4 was selected (P4 preference)
#'   \item **choice_score:** overall choice preference score = (P1 + P2) - (P3 + P4)
#'   \item **omissions:** number of omissions in the session (where the rat did not chose an option)
#'   \item **mean_choice_lat:** mean time to choose an option on trials in the session when any choice (P1, P2, P3, or P4) was made
#'   \item **choice_lat_P1:** mean choice latency for trials when P1 was chosen
#'   \item **choice_lat_P2:** mean choice latency for trials when P2 was chosen
#'   \item **choice_lat_P3:** mean choice latency for trials when P3 was chosen
#'   \item **choice_lat_P4:** mean choice latency for trials when P4 was chosen
#'   \item **mean_collect_lat:** mean time to nose poke for sugar pellets when the rat was rewarded
#'   \item **dd_phenotype:** the targeted delay discounting phenotype (ideal or worsening)
#'   \item **iti_dur_P1:** ITI duration used for P1 during the session
#'   \item **iti_dur_P2:** ITI duration used for P2 during the session
#'   \item **iti_dur_P3:** ITI duration used for P3 during the session
#'   \item **iti_dur_P4:** ITI duration used for P4 during the session
#'   \item **mean_choice_iti:** mean ITI of options chosen during the session
#' }
#'
#' The wide format/shape contains the same session-aggregated variables that
#' have been pivoted by session so that all repeated measurements by subject
#' appear in additional columns with names ending with with "*_s#" where "#" =
#' the session number, e.g., session_date_s1 is the date of that subject's first
#' session. The wide form of the data has one row per subject.
#'
#' @seealso \link[tidyr]{pivot_wider}, \link[tidyr]{pivot_longer}
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
ddrgt_pivot <- function(.df) {
  if(nrow(.df) > ncol(.df)) {
    out <- .df |>
      dplyr::mutate(session = paste0("s", as.character(session))) |>
      tidyr::pivot_wider(names_from = session, values_from = c(session_date, msn:tidyselect::last_col()),
                         names_glue = "{.value}_{session}")
    out <- wash_df(out, clean_names = FALSE) |>
      dplyr::arrange(subject)
  } else {
    out <- .df |>
      tidyr::pivot_longer(cols = c(-subject, -group),
                          names_to = c(".value", "session"),
                          names_pattern = "(.*)_s([0123456789]{0,})") |>
      dplyr::relocate(session_date, .after = subject) |>
      dplyr::filter(!is.na(n_trials))
    out <- wash_df(out, clean_names = FALSE) |>
      dplyr::arrange(subject, session)
  }
  return(out)
}

#' Prepare a session-aggregated version of cued or uncued RGT data for analysis.
#'
#' Prepare an analytical version of session-aggregated data from the cued
#' (Barrus & Winstanley, 2016) or uncued (Zeeb et al., 2009) version of the rat
#' gambling task (RGT) that has been parsed by [rgt_read()] or
#' [rgt_read_file()]. Either the long or wide forms of the data can be generated
#' and there is a convenience option to export the data to a csv file.
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr if_else
#' @importFrom dplyr case_when
#' @importFrom dplyr join_by
#' @importFrom dplyr relocate
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_wider
#' @importFrom readr write_csv
#' @importFrom tidyselect last_col
#' @importFrom stringr str_detect
#' @importFrom data.table as.data.table
#' @importFrom data.table rbindlist
#' @importFrom tibble as_tibble
#'
#' @param .df A data frame of cued or uncued rGT data that has been imported
#'   into R using either [rgt_read()] or [rgt_read_file()].
#'
#' @param shape Aggregation format or shape to use. The default, "long" format,
#'   organizes the data with as one column per variable and represents repeated
#'   measurements for experimental subjects as rows. This "tidy" format is
#'   commonly required for analyses performed using R. The alternative "wide"
#'   format instead uses additional columns to represent repeated measurements
#'   for experimental subjects (with one row per subject). The wide format is
#'   commonly required for analyses performed using other programs like SPSS.
#'
#' @param output_file Comma-separated variable (csv) file path and name to
#'   export the combined data to, which should end in ".csv". e.g.
#'   "rgt_data.csv" to save the file in your working directory.
#'
#' @param recode_session_by_date If TRUE (default) re-codes the session variable
#'   based on the session dates for each rat in chronological order (from the
#'   perspective of the rat). Set this to FALSE if you want to retain the
#'   original session numbers from the raw MedPC file.
#'
#' @return A data frame containing analysis-ready rGT data with columns that depend on the
#'   source data MSNs.
#'   For standard cued/uncued ITI5 data, the output will include:
#'
#' \itemize{
#'   \item **subject:** rat ID number
#'   \item **group:** experimental group (only valid if entered into MedPC)
#'   \item **session_date:** session date (yyyy-mm-dd) according to the MedPC file
#'   \item **session:** session number (subject's perspective)
#'   \item **msn:** the MedPC program that was run during the session
#'   \item **n_trials:** number of trials initiated in the session
#'   \item **premat_n:** number of premature responses in the session
#'   \item **premat_prop:** premat_tot/n_trials = premature response rate (for the session)
#'   \item **total_choices:** number of choices made in the session (successfully completed trials)
#'   \item **P1:** proportion of total_choices where option P1 was selected (P1 preference)
#'   \item **P2:** proportion of total_choices where option P2 was selected (P2 preference)
#'   \item **P3:** proportion of total_chocies where option P3 was selected (P3 preference)
#'   \item **P4:** proportion of total_chocies where option P4 was selected (P4 preference)
#'   \item **choice_score:** overall choice preference score = (P1 + P2) - (P3 + P4)
#'   \item **omissions:** number of omissions in the session (where the rat did not chose an option)
#'   \item **mean_choice_lat:** mean time to choose an option on trials in the session when any choice (P1, P2, P3, or P4) was made
#'   \item **choice_lat_P1:** mean choice latency for trials when P1 was chosen
#'   \item **choice_lat_P2:** mean choice latency for trials when P2 was chosen
#'   \item **choice_lat_P3:** mean choice latency for trials when P3 was chosen
#'   \item **choice_lat_P4:** mean choice latency for trials when P4 was chosen
#'   \item **mean_collect_lat:** mean time to nose poke for sugar pellets when the rat was rewarded
#' }
#'
#'  ITI-9 data will also include the following columns:
#'
#'  \itemize{
#'   \item **mean_premat_time:** mean time of premature responses during the session
#'  }
#'
#'  DD-rGT data will also include the following columns:
#'
#'  \itemize{
#'   \item **mean_premat_time:** mean time of premature responses during the session
#'   \item **dd_phenotype:** the targeted delay discounting phenotype (ideal or worsening)
#'   \item **iti_dur_P1:** ITI duration used for P1 during the session
#'   \item **iti_dur_P2:** ITI duration used for P2 during the session
#'   \item **iti_dur_P3:** ITI duration used for P3 during the session
#'   \item **iti_dur_P4:** ITI duration used for P4 during the session
#'   \item **mean_choice_iti:** mean ITI of options chosen during the session
#' }

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
rgt_prep <- function(.df, shape = c("long", "wide"),
                     output_file = NULL, recode_session_by_date = TRUE) {
  shape <- match.arg(shape)
  msns <- unique(.df[["msn"]])
  supported_msns <- c("rGT_classicA", "rGT_classicB", #uncued ITI-5
                      "rGT_A-cue", "rGT_B-cue", "RevRGT_A-cue", "RevRGT_B-cue", #cued ITI-5
                      "rGT_ClassicA_I9", "rGT_ClassicB_I9", #uncued ITI-9
                      "rGT_A-cue-I9", "rGT_A-cue_I9", "rGT_B-cue_I9", "rGT_B-cue-I9", #cued ITI-9
                      "rGT_A-cue-ITI9-v3", "rGT_B-cue-ITI9-v3",# cued ITI-9
                      "DDrGT_A-cue-v10_ideal_phenotype", "DDrGT_A-cue-v10_worsening_phenotype", #ddrGT vA
                      "DDrGT_B-cue-v10_ideal_phenotype", "DDrGT_B-cue-v10_worsening_phenotype") #ddrGT vB
  if(any(!(msns %in% supported_msns))) {
    stop(
      stringr::str_glue("the MedPC program used for data collection is not supported according to the listed MSN code.\nCurrently supported programs include:\n {paste(supported_msns, collapse = '\n ')}")
         )
  }

  iti5_present <- any(stringr::str_detect(msns, "I9|DDrGT", negate = TRUE))
  iti9_present <- any(stringr::str_detect(msns, "I9"))
  ddrgt_present <- any(stringr::str_detect(msns, "DDrGT"))

  if(
    (iti5_present & iti9_present) || (iti5_present & ddrgt_present) ||(iti9_present & ddrgt_present)) {
    warning(paste(
      "A combination of ITI-5, ITI-9, and/or ddrGT data sources detected.",
      "  Will attempt to aggregate data from each task type and then join them.",
      "  If output is flawed consider filtering the data to parse one rGT data type at a time (ITI-5 or ITI-9 or ddRGT) and try again.",
      sep = "\n"))
    out <- list()
      if(iti5_present) {
        message("ITI5 data detected")
        iti5_data <- .df |> dplyr::filter(stringr::str_detect(msn, "I9|DDrGT", negate = TRUE))
        if(missing(output_file)){
          out[["iti5"]] <- iti5_prep(iti5_data, shape = shape, recode_session_by_date = recode_session_by_date)
        } else {
          out[["iti5"]] <- iti5_prep(iti5_data, shape = shape, output_file = output_file, recode_session_by_date = recode_session_by_date)
        }
        out[["iti5"]] <- data.table::as.data.table(out[["iti5"]])
      }
    if(iti9_present) {
      message("ITI9 data detected")
      iti9_data <- .df |> dplyr::filter(stringr::str_detect(msn, "I9"))
      if(missing(output_file)){
        out[["iti9"]] <- iti9_prep(iti9_data, shape = shape, recode_session_by_date = recode_session_by_date)
      } else {
        out[["iti9"]] <- iti9_prep(iti9_data, shape = shape, output_file = output_file, recode_session_by_date = recode_session_by_date)
      }
      out[["iti9"]] <- data.table::as.data.table(out[["iti9"]])
    }
    if(ddrgt_present) {
      message("DDrGT data detected")
      ddrgt_data <- .df |> dplyr::filter(stringr::str_detect(msn, "DDrGT"))
      if(missing(output_file)){
        out[["ddrgt"]] <- ddrgt_prep(ddrgt_data, shape = shape, recode_session_by_date = recode_session_by_date)
      } else {
        out[["ddrgt"]] <- ddrgt_prep(ddrgt_data, shape = shape, output_file = output_file, recode_session_by_date = recode_session_by_date)
      }
      out[["ddrgt"]] <- data.table::as.data.table(out[["ddrgt"]])
    }
    out <- data.table::rbindlist(out, fill = TRUE) |> tibble::as_tibble()
  } else {
    if(iti5_present) {
      message("ITI5 data detected")
      if(missing(output_file)){
        out <- iti5_prep(.df, shape = shape, recode_session_by_date = recode_session_by_date) |> tibble::as_tibble()
      } else {
        out <- iti5_prep(.df, shape = shape, output_file = output_file, recode_session_by_date = recode_session_by_date) |> tibble::as_tibble()
      }
      return(out)
    } else if(iti9_present) {
      message("ITI9 data detected")
      if(missing(output_file)){
        out <- iti9_prep(.df, shape = shape, recode_session_by_date = recode_session_by_date) |> tibble::as_tibble()
      } else {
        out <- iti9_prep(.df, shape = shape, output_file = output_file, recode_session_by_date = recode_session_by_date) |> tibble::as_tibble()
      }
      return(out)
    } else {
      if(missing(output_file)){
        message("DDrGT data detected")
        out <- ddrgt_prep(.df, shape = shape, recode_session_by_date = recode_session_by_date) |> tibble::as_tibble()
      } else {
        out <- ddrgt_prep(.df, shape = shape, output_file = output_file, recode_session_by_date = recode_session_by_date) |> tibble::as_tibble()
      }
      return(out)
    }
  }
}

#' Convert a session-aggregated version of data from the cued (Barrus &
#' Winstanley, 2016) or uncued (Zeeb et al., 2009) rat gambling task (RGT) that
#' has been parsed by [rgt_read()] or [rgt_read_file()] and prepared by
#' [rgt_prep()] from wide format to long format or vice versa based on its
#' current shape. For more information on pivoting data frames between long and
#' wide formats, see the
#' [pivoting](https://tidyr.tidyverse.org/articles/pivot.html) article on the
#' tidyr website.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr relocate
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect last_col
#' @importFrom data.table as.data.table
#' @importFrom data.table rbindlist
#' @importFrom tibble as_tibble
#'
#' @param .df A data frame of RGT data that has been imported into R using
#'   either [rgt_read()] or [rgt_read_file()] and
#'   prepared by [rgt_prep()].
#'
#' @return A data frame containing session-aggregated RGT data with the
#'   following columns, for the long format/shape (one column per variable):
#'
#' @return A data frame containing analysis-ready rGT data with columns that
#'   depend on the source data MSNs. The long format/shape (one column per
#'   variable) includes the following columns.
#'
#'   For standard cued/uncued ITI5 data:
#'
#' \itemize{
#'   \item **subject:** rat ID number
#'   \item **group:** experimental group (only valid if entered into MedPC)
#'   \item **session_date:** session date (yyyy-mm-dd) according to the MedPC file
#'   \item **session:** session number (subject's perspective)
#'   \item **msn:** the MedPC program that was run during the session
#'   \item **n_trials:** number of trials initiated in the session
#'   \item **premat_n:** number of premature responses in the session
#'   \item **premat_prop:** premat_tot/n_trials = premature response rate (for the session)
#'   \item **total_choices:** number of choices made in the session (successfully completed trials)
#'   \item **P1:** proportion of total_choices where option P1 was selected (P1 preference)
#'   \item **P2:** proportion of total_choices where option P2 was selected (P2 preference)
#'   \item **P3:** proportion of total_chocies where option P3 was selected (P3 preference)
#'   \item **P4:** proportion of total_chocies where option P4 was selected (P4 preference)
#'   \item **choice_score:** overall choice preference score = (P1 + P2) - (P3 + P4)
#'   \item **omissions:** number of omissions in the session (where the rat did not chose an option)
#'   \item **mean_choice_lat:** mean time to choose an option on trials in the session when any choice (P1, P2, P3, or P4) was made
#'   \item **choice_lat_P1:** mean choice latency for trials when P1 was chosen
#'   \item **choice_lat_P2:** mean choice latency for trials when P2 was chosen
#'   \item **choice_lat_P3:** mean choice latency for trials when P3 was chosen
#'   \item **choice_lat_P4:** mean choice latency for trials when P4 was chosen
#'   \item **mean_collect_lat:** mean time to nose poke for sugar pellets when the rat was rewarded
#' }
#'
#'  ITI-9 data also includes:
#'
#'  \itemize{
#'   \item **mean_premat_time:** mean time of premature responses during the session
#'  }
#'
#'  DD-rGT data also includes:
#'
#'  \itemize{
#'   \item **mean_premat_time:** mean time of premature responses during the session
#'   \item **dd_phenotype:** the targeted delay discounting phenotype (ideal or worsening)
#'   \item **iti_dur_P1:** ITI duration used for P1 during the session
#'   \item **iti_dur_P2:** ITI duration used for P2 during the session
#'   \item **iti_dur_P3:** ITI duration used for P3 during the session
#'   \item **iti_dur_P4:** ITI duration used for P4 during the session
#'   \item **mean_choice_iti:** mean ITI of options chosen during the session
#' }
#'
#' The wide format/shape contains the same session-aggregated variables that
#' have been pivoted by session so that all repeated measurements by subject
#' appear in additional columns with names ending with with "*_s#" where "#" =
#' the session number, e.g., session_date_s1 is the date of that subject's first
#' session. The wide form of the data has one row per subject.
#'
#' @seealso \link[tidyr]{pivot_wider}, \link[tidyr]{pivot_longer}
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
rgt_pivot <- function(.df) {
  msns <- unique(.df[["msn"]])
  iti5_present <- any(stringr::str_detect(msns, "I9|DDrGT", negate = TRUE))
  iti9_present <- any(stringr::str_detect(msns, "I9"))
  ddrgt_present <- any(stringr::str_detect(msns, "DDrGT"))

  if(
    (iti5_present & iti9_present) || (iti5_present & ddrgt_present) ||(iti9_present & ddrgt_present)) {
    #   stop("A combination of ITI-5, ITI-9, and/or ddrGT data sources detected.\nSubset/filter the data to parse one rGT data type at a time (ITI-5 or ITI-9 or ddRGT) and try again.")
    warning(paste(
      "A combination of ITI-5, ITI-9, and/or ddrGT data sources detected.",
      "  Will attempt to pivot data from each task type and then re-join them.",
      "  If output is flawed consider filtering the data to parse one rGT data type at a time (ITI-5 or ITI-9 or ddRGT) and try again.",
      sep = "\n"))

    out <- list()
    if(iti5_present) {
      message("ITI5 data detected")
      iti5_data <- .df |> dplyr::filter(stringr::str_detect(msn, "I9|DDrGT", negate = TRUE))
      out[["iti5"]] <- iti5_pivot(iti5_data)
      out[["iti5"]] <- data.table::as.data.table(out[["iti5"]])
    }
    if(iti9_present) {
      message("ITI9 data detected")
      iti9_data <- .df |> dplyr::filter(stringr::str_detect(msn, "I9"))
      out[["iti9"]] <- iti9_pivot(iti9_data)
      out[["iti9"]] <- data.table::as.data.table(out[["iti9"]])
    }
    if(ddrgt_present) {
      message("DDrGT data detected")
      ddrgt_data <- .df |> dplyr::filter(stringr::str_detect(msn, "DDrGT"))
      out[["ddrgt"]] <- ddrgt_pivot(ddrgt_data)
      out[["ddrgt"]] <- data.table::as.data.table(out[["ddrgt"]])
    }
    out <- data.table::rbindlist(out, fill = TRUE) |> tibble::as_tibble()
  } else {
    if(iti5_present) {
      message("ITI5 data detected")
      out <- iti5_pivot(.df)
    } else if(iti9_present) {
      message("ITI9 data detected")
      out <- iti9_pivot(.df)
    } else {
      message("DDrGT data detected")
      out <- ddrgt_pivot(.df)
    }
  }
  return(out)
}
