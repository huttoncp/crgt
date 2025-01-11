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

# importing functions -----------------------------------------------------

#' Read a raw MedPC file from the five choice serial reaction time task.
#'
#' Read a raw MedPC (Tatham & Zurn, 1989) data file from the 5 choice serial
#' reaction time task (5CSRT; Robbins, 2002) and convert it into an R data.table.
#'
#' @importFrom rstudioapi selectFile
#' @importFrom data.table data.table
#' @importFrom data.table fifelse
#' @importFrom data.table :=
#' @importFrom lubridate mdy
#'
#' @param path Character string for the raw MedPC file path, e.g.
#'   "!2021-11-13_7h01m.Subject 1" if the file is in your working directory. A
#'   file explorer window will pop-up to allow you to choose the file
#'   interactively if the path is unspecified.
#'
#' @return A data frame containing 5CSRT data with the following columns
#'
#' \itemize{
#'   \item **msn:** task version
#'   \item **start_date:** the session start date
#'   \item **start_time:** the session start time
#'   \item **subject:** subject identifier
#'   \item **box:** the operant box used to collect the data
#'   \item **comment:** any comments that were noted in MedPC
#'   \item **session:** session number
#'   \item **stim_dur:** stimulus duration
#'   \item **iti_dur:** inter-trial-interval duration
#'   \item **trial:** trial number
#'   \item **offered:** the choice offered in the trial
#'   \item **chosen:** the option chosen in the trial
#'   \item **correct_latency:** response time when correct
#'   \item **incorrect_latency:** response time when incorrect
#'   \item **collection_latency:** time to collect a reward
#'   \item **omission:** indicator of failure to respond
#'   \item **prematures:** premature response indicator
#'   \item **persev_r1:** perseveration
#'   \item **persev_r2:** perseveration
#'   \item **persev_r3:** perseveration
#'   \item **persev_r4:** perseveration
#'   \item **persev_r5:** perseveration
#'   \item **traypokes:** tray poke count
#' }
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @references
#' Robbins, T. (2002). The 5-choice serial reaction time task: behavioural
#' pharmacology and functional neurochemistry. Psychopharmacology, 163(3),
#' 362-380.
#'
#' Tatham, T. A., & Zurn, K. R. (1989). The MED-PC experimental apparatus
#' programming system. Behavior Research Methods, Instruments, & Computers,
#' 21(2), 294-302.
#'
#' @export
fcsrt_read_file <- function(path = "") {
  if(!file.exists(path)){
    path <- rstudioapi::selectFile()
  }
  med_pc_raw <- suppressWarnings(import_medpc(path))
  increment <- 20
  len <- length(med_pc_raw$K)/increment
  ivec <- increment*0:(len-1)

  out <- data.table::data.table(msn = med_pc_raw$MSN,
                                start_date = med_pc_raw$`Start Date`,
                                start_time = med_pc_raw$`Start Time`,
                                subject = med_pc_raw$Subject,
                                box = med_pc_raw$Box,
                                comment = med_pc_raw$Comment,
                                session = med_pc_raw$K[13 + ivec],
                                stim_dur = med_pc_raw$K[19 + ivec],
                                iti_dur = med_pc_raw$K[20 + ivec],
                                trial = med_pc_raw$K[1 + ivec],
                                offered = med_pc_raw$K[2 + ivec],
                                chosen = med_pc_raw$K[3 + ivec],
                                correct_latency = med_pc_raw$K[4 + ivec],
                                incorrect_latency = med_pc_raw$K[5 + ivec],
                                collection_latency = med_pc_raw$K[6 + ivec],
                                omission = med_pc_raw$K[7 + ivec],
                                premature_resp = med_pc_raw$K[14 + ivec],
                                persev_r1 = med_pc_raw$K[8 + ivec],
                                persev_r2 = med_pc_raw$K[9 + ivec],
                                persev_r3 = med_pc_raw$K[10 + ivec],
                                persev_r4 = med_pc_raw$K[11 + ivec],
                                persev_r5 = med_pc_raw$K[12 + ivec],
                                traypokes = med_pc_raw$K[18 + ivec])

  out[, start_date := lubridate::mdy(start_date)]

  #correct premature response trial numbers
  out <- out[trial >= 1]
  out[, trial := data.table::fifelse(premature_resp != 0, trial - 1, trial)]

  return(out)
}

#' Read and combine a folder of raw MedPC files from the five choice serial reaction time task.
#'
#' Read a folder of raw MedPC (Tatham & Zurn, 1989) data files from the 5 choice
#' serial reaction time task (5CSRT; Robbins, 2002) and combine them into a
#' single R tibble with an option to export the data to a comma-separated
#' variable (.csv) file.
#'
#' @importFrom rstudioapi selectDirectory
#' @importFrom data.table rbindlist
#' @importFrom dplyr mutate
#' @importFrom readr write_csv
#' @importFrom purrr map
#' @importFrom lubridate mdy
#'
#' @param path Character string for path to the folder containing the raw
#'   MedPC files, e.g. "~/raw_5csrt_files/". A file explorer window will pop-up
#'   to allow you to choose the folder interactively if the path is unspecified.
#'
#' @param output_file Comma-separated variable (csv) file path and name to
#'   export the combined data to, which should end in ".csv". e.g.
#'   "5csrt_data.csv" to save the file in your working directory.
#'
#' @param pattern If the specified folder contains non-MedPC files or other
#'   folders, this argument allows you to use a regular expression or fixed
#'   string to tell the function which files to read. For example, raw MedPC
#'   files often start with "!", so you might use pattern = "!" to only read
#'   files containing a "!" in their name. See this [this blog
#'   post](https://craig.rbind.io/post/2020-06-28-asgr-2-3-string-manipulation/)
#'   to learn more about regular expressions in R.
#'
#' @param fixed Set this to `TRUE` if you want the pattern argument to be
#'   interpreted as a fixed string pattern (i.e. exactly as written) instead of
#'   a regular expression for selecting the MedPC files in the chosen folder.
#'   Ignored if "pattern" argument is not used.
#'
#' @param negate Set this to `TRUE` to read files with names which do *not*
#'   match the specified pattern. Ignored if "pattern" argument is not used.
#'
#' @param progress Should a progress bar be displayed when reading multiple
#'   MedPC data files (default = TRUE)? See [purrr::progress_bars]
#'   for details and options.
#'
#' @return A data frame containing 5CSRT data with the following columns
#' \itemize{
#'   \item **msn:** task version
#'   \item **start_date:** the session start date
#'   \item **start_time:** the session start time
#'   \item **subject:** subject identifier
#'   \item **box:** the operant box used to collect the data
#'   \item **comment:** any comments that were noted in MedPC
#'   \item **session:** session number
#'   \item **stim_dur:** stimulus duration
#'   \item **iti_dur:** inter-trial-interval duration
#'   \item **trial:** trial number
#'   \item **offered:** the choice offered in the trial
#'   \item **chosen:** the option chosen in the trial
#'   \item **correct_latency:** response time when correct
#'   \item **incorrect_latency:** response time when incorrect
#'   \item **collection_latency:** time to collect a reward
#'   \item **omission:** indicator of failure to respond
#'   \item **prematures:** premature response indicator
#'   \item **persev_r1:** perseveration on hole 1
#'   \item **persev_r2:** perseveration on hole 2
#'   \item **persev_r3:** perseveration on hole 3
#'   \item **persev_r4:** perseveration on hole 4
#'   \item **persev_r5:** perseveration on hole 5
#'   \item **traypokes:** tray nosepoke count
#' }
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @references
#' Robbins, T. (2002). The 5-choice serial reaction time task: behavioural
#' pharmacology and functional neurochemistry. Psychopharmacology, 163(3),
#' 362-380.
#'
#' Tatham, T. A., & Zurn, K. R. (1989). The MED-PC experimental apparatus
#' programming system. Behavior Research Methods, Instruments, & Computers,
#' 21(2), 294-302.
#'
#' @export
fcsrt_read <- function(path = "", output_file = NULL,
                       pattern = NULL, fixed = FALSE, negate = FALSE, progress = TRUE) {
  if(!dir.exists(path)){
    path <- rstudioapi::selectDirectory()
  }
  folder <- path
  file_list <- list.files(folder, recursive = TRUE)
  if(!missing(pattern)) {
    file_list <- grep(pattern, file_list, value = TRUE, fixed = fixed, invert = negate)
  }
  file_paths <- paste(folder, file_list, sep = "/")
  out <- purrr::map(file_paths, fcsrt_read_file, .progress = progress) |> #read each file and store in a list of data.tables
    data.table::rbindlist() |> #combine data frames
    wash_df() #parse column classes, remove empty

  if(!missing(output_file)) {
    if(!grepl("\\.csv$", output_file)) {
      message('output file name must end in ".csv"
              \nappending ".csv" to output file name')
    }
    readr::write_csv(out, output_file)
    message(paste0("data exported to path: ", output_file))
  }

  return(out)
}

#' Read a raw MedPC file from the cued or uncued rat gambling task.
#'
#' Read a raw MedPC (Tatham & Zurn, 1989) data file from the cued (Barrus &
#' Winstanley, 2016) or uncued (Zeeb et al., 2009) versions of the rat gambling
#' task (RGT) and convert it into an R data.table.
#'
#' @importFrom rstudioapi selectFile
#' @importFrom data.table data.table
#' @importFrom data.table fifelse
#' @importFrom data.table :=
#' @importFrom lubridate mdy
#'
#' @param path Character string for the raw MedPC file path, e.g.
#'   "!2021-11-13_7h01m.Subject 1" if the file is in your working directory. A
#'   file explorer window will pop-up to allow you to choose the file
#'   interactively if the path is unspecified.
#'
#' @return A data frame containing 5CSRT data with the following columns
#' \itemize{
#'   \item **msn:** task reward contingency coding version (A or B)
#'   \item **start_date:** the session start date
#'   \item **start_time:** the session start time
#'   \item **subject:** subject identifier
#'   \item **group:** experimental group
#'   \item **box:** the operant box used to collect the data
#'   \item **experiment:** experiment identifier
#'   \item **session:** session number
#'   \item **trial:** trial number
#'   \item **rewarded:** 1 if rewarded, 0 if punished
#'   \item **pellets:** number of pellets dispensed
#'   \item **omission:** indicator of failure to respond
#'   \item **chosen:** the option chosen in the trial
#'   \item **choice_lat:** time taken to make a choice
#'   \item **collect_lat:** time to collect a reward
#'   \item **pun_persev_h1:** perseverant response on hole 1 during punishment
#'   \item **pun_persev_h2:** perseverant response on hole 2 during punishment
#'   \item **pun_persev_h3:** perseverant response on hole 3 during punishment
#'   \item **pun_persev_h4:** perseverant response on hole 4 during punishment
#'   \item **pun_persev_h5:** perseverant response on hole 5 during punishment
#'   \item **pun_head_entry:** indicator of the rat responding during a time-out
#'   \item **pun_dur:** timeout punishment duration
#'   \item **premature_resp:** premature response indicator
#'   \item **premature_hole:** hole chosen when prematurely responding
#'   \item **rew_persev_h1:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h2:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h3:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h4:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h5:** perseverant response on hole 1 during reward
#' }
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @references
#' Barrus, M.M., & Winstanley, C.A. (2016). Dopamine D3 receptors modulate the
#' ability of win-paired cues to increase risky choice in a rat gambling task.
#' Journal of Neuroscience, 36(3), 785-794.
#'
#' Tatham, T.A., & Zurn, K.R. (1989). The MED-PC experimental apparatus
#' programming system. Behavior Research Methods, Instruments, & Computers,
#' 21(2), 294-302.
#'
#' Zeeb, F.D., Robbins, T.W., & Winstanley, C.A. (2009). Serotonergic and
#' dopaminergic modulation of gambling behavior as assessed using a novel rat
#' gambling task. Neuropsychopharmacology, 34(10), 2329-2343.
#'
#' @export
iti5_read_file <- function(path = "")  {
  if(!file.exists(path)){
    path <- rstudioapi::selectFile()
  }
  med_pc_raw <- suppressWarnings(import_medpc(path))

  increment <- 30
  len <- length(med_pc_raw$K)/increment
  ivec <- increment*0:(len-1)

  out <- data.table::data.table(msn = med_pc_raw$MSN,
                                start_date = med_pc_raw$`Start Date`,
                                start_time = med_pc_raw$`Start Time`,
                                subject = med_pc_raw$Subject,
                                group = med_pc_raw$Group,
                                box = med_pc_raw$Box,
                                experiment = med_pc_raw$Experiment,
                                session = med_pc_raw$K[5 + ivec],
                                trial = med_pc_raw$K[1 + ivec],
                                rewarded = med_pc_raw$K[13 + ivec],
                                pellets =  med_pc_raw$K[14 + ivec],
                                omission = med_pc_raw$K[7 + ivec],
                                chosen = med_pc_raw$K[3 + ivec],
                                choice_lat = med_pc_raw$K[4 + ivec],
                                collect_lat = med_pc_raw$K[6 + ivec],
                                pun_persev_h1 = med_pc_raw$K[8 + ivec],
                                pun_persev_h2 = med_pc_raw$K[9 + ivec],
                                pun_persev_h3 = med_pc_raw$K[10 + ivec],
                                pun_persev_h4 = med_pc_raw$K[11 + ivec],
                                pun_persev_h5 = med_pc_raw$K[12 + ivec],
                                pun_head_entry = med_pc_raw$K[18 + ivec],
                                pun_dur = med_pc_raw$K[15 + ivec],
                                premature_resp = med_pc_raw$K[17 + ivec],
                                premature_hole = med_pc_raw$K[16 + ivec],
                                rew_persev_h1 = med_pc_raw$K[21 + ivec],
                                rew_persev_h2 = med_pc_raw$K[22 + ivec],
                                rew_persev_h3 = med_pc_raw$K[23 + ivec],
                                rew_persev_h4 = med_pc_raw$K[24 + ivec],
                                rew_persev_h5 = med_pc_raw$K[25 + ivec])

  out[, start_date := lubridate::mdy(start_date)]

  #correct premature response trial numbers
  out <- out[trial >= 1]
  out[, trial := data.table::fifelse(premature_resp != 0, trial - 1, trial)]

  return(out)
}

#' Read and combine a folder of raw MedPC files from the cued or uncued rat gambling task.
#'
#' Read a folder of raw MedPC (Tatham & Zurn, 1989) data files from the cued
#' (Barrus & Winstanley, 2016) or uncued (Zeeb et al., 2009) version of the rat
#' gambling task (RGT) and convert them into a single tibble with an option to
#' export the data to a comma-separated variable (.csv) file.
#'
#' @importFrom rstudioapi selectDirectory
#' @importFrom data.table rbindlist
#' @importFrom dplyr mutate
#' @importFrom readr write_csv
#' @importFrom purrr map
#'
#' @param path Character string for path to the folder containing the raw
#'   MedPC files, e.g. "~/raw_rgt_files/". A file explorer window will pop-up
#'   to allow you to choose the folder interactively if the path is unspecified.
#'
#' @param output_file Comma-separated variable (csv) file path and name to
#'   export the combined data to, which should end in ".csv". e.g.
#'   "rgt_data.csv" to save the file in your working directory.
#'
#' @param pattern If the specified folder contains non-MedPC files or other
#'   folders, this argument allows you to use a regular expression or fixed
#'   string to tell the function which files to read. For example, raw MedPC
#'   files often start with "!", so you might use pattern = "!" to only read
#'   files containing a "!" in their name. See this [this blog
#'   post](https://craig.rbind.io/post/2020-06-28-asgr-2-3-string-manipulation/)
#'   to learn more about regular expressions in R.
#'
#' @param fixed Set this to `TRUE` if you want the pattern argument to be
#'   interpreted as a fixed string pattern (i.e. exactly as written) instead of
#'   a regular expression for selecting the MedPC files in the chosen folder.
#'   Ignored if "pattern" argument is not used.
#'
#' @param negate Set this to `TRUE` to read files with names which do *not*
#'   match the specified pattern. Ignored if "pattern" argument is not used.
#'
#' @param progress Should a progress bar be displayed when reading multiple
#'   MedPC data files (default = TRUE)? See [purrr::progress_bars]
#'   for details and options.
#'
#' @return A data frame containing 5CSRT data with the following columns
#' \itemize{
#'   \item **msn:** task reward contingency coding version (A or B)
#'   \item **start_date:** the session start date
#'   \item **start_time:** the session start time
#'   \item **subject:** subject identifier
#'   \item **group:** experimental group
#'   \item **box:** the operant box used to collect the data
#'   \item **experiment:** experiment identifier
#'   \item **session:** session number
#'   \item **trial:** trial number
#'   \item **rewarded:** 1 if rewarded, 0 if punished
#'   \item **pellets:** number of pellets dispensed
#'   \item **omission:** indicator of failure to respond
#'   \item **chosen:** the option chosen in the trial
#'   \item **choice_lat:** time taken to make a choice
#'   \item **collect_lat:** time to collect a reward
#'   \item **pun_persev_h1:** perseverant response on hole 1 during punishment
#'   \item **pun_persev_h2:** perseverant response on hole 2 during punishment
#'   \item **pun_persev_h3:** perseverant response on hole 3 during punishment
#'   \item **pun_persev_h4:** perseverant response on hole 4 during punishment
#'   \item **pun_persev_h5:** perseverant response on hole 5 during punishment
#'   \item **pun_head_entry:** indicator of the rat responding during a time-out
#'   \item **pun_dur:** timeout punishment duration
#'   \item **premature_resp:** premature response indicator
#'   \item **premature_hole:** hole chosen when prematurely responding
#'   \item **rew_persev_h1:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h2:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h3:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h4:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h5:** perseverant response on hole 1 during reward
#' }
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @references
#' Barrus, M.M., & Winstanley, C.A. (2016). Dopamine D3 receptors modulate the
#' ability of win-paired cues to increase risky choice in a rat gambling task.
#' Journal of Neuroscience, 36(3), 785-794.
#'
#' Tatham, T.A., & Zurn, K.R. (1989). The MED-PC experimental apparatus
#' programming system. Behavior Research Methods, Instruments, & Computers,
#' 21(2), 294-302.
#'
#' Zeeb, F.D., Robbins, T.W., & Winstanley, C.A. (2009). Serotonergic and
#' dopaminergic modulation of gambling behavior as assessed using a novel rat
#' gambling task. Neuropsychopharmacology, 34(10), 2329-2343.
#'
#' @export
iti5_read <- function(path = "", output_file = NULL,
                     pattern = NULL, fixed = FALSE, negate = FALSE, progress = TRUE) {
  if(!dir.exists(path)){
    path <- rstudioapi::selectDirectory()
  }
  folder <- path
  file_list <- list.files(folder, recursive = TRUE)
  if(!missing(pattern)) {
    file_list <- grep(pattern, file_list, value = TRUE, fixed = fixed, invert = negate)
  }
  file_paths <- paste(folder, file_list, sep = "/")
  out <- purrr::map(file_paths, iti5_read_file, .progress = progress) |> #read each file and store in a list of data.tables
    data.table::rbindlist() |> #combine data frames
    wash_df() #parse column classes, remove empty

  if(!missing(output_file)) {
    if(!grepl("\\.csv$", output_file)) {
      message('output file name must end in ".csv"
              \nappending ".csv" to output file name')
    }
    readr::write_csv(out, output_file)
    message(paste0("data exported to path: ", output_file))
  }

  return(out)
}

#' Read a raw MedPC file from the ITI-9 version of the rat gambling task.
#'
#' Read a raw MedPC (Tatham & Zurn, 1989) data file from the cued (Barrus &
#' Winstanley, 2016) or uncued (Zeeb et al., 2009) versions of the rat gambling
#' task (RGT) with a 9-second intertrial interval (ITI) and convert it into an R
#' data.table.
#'
#' @importFrom rstudioapi selectFile
#' @importFrom data.table data.table
#' @importFrom data.table fifelse
#' @importFrom data.table :=
#' @importFrom lubridate mdy
#'
#' @param path Character string for the raw MedPC file path, e.g.
#'   "!2021-11-13_7h01m.Subject 1" if the file is in your working directory. A
#'   file explorer window will pop-up to allow you to choose the file
#'   interactively if the path is unspecified.
#'
#' @return A data frame containing ITI-9 RGT data with the following columns
#' \itemize{
#'   \item **msn:** task reward contingency coding version (A or B)
#'   \item **start_date:** the session start date
#'   \item **start_time:** the session start time
#'   \item **subject:** subject identifier
#'   \item **group:** experimental group
#'   \item **box:** the operant box used to collect the data
#'   \item **experiment:** experiment identifier
#'   \item **session:** session number
#'   \item **trial:** trial number
#'   \item **rewarded:** 1 if rewarded, 0 if punished
#'   \item **pellets:** number of pellets dispensed
#'   \item **omission:** indicator of failure to respond
#'   \item **chosen:** the option chosen in the trial
#'   \item **choice_lat:** time taken to make a choice
#'   \item **collect_lat:** time to collect a reward
#'   \item **pun_dur:** timeout punishment duration
#'   \item **iti_dur:** intertrial interval duration used during data collection
#'   \item **premature_resp:** premature response indicator
#'   \item **premature_time:** premature response time
#'   \item **premature_hole:** hole chosen when prematurely responding
#'   \item **rew_persev_h1:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h2:** perseverant response on hole 2 during reward
#'   \item **rew_persev_h3:** perseverant response on hole 3 during reward
#'   \item **rew_persev_h4:** perseverant response on hole 4 during reward
#'   \item **rew_persev_h5:** perseverant response on hole 5 during reward
#'   \item **pun_persev_h1:** perseverant response on hole 1 during punishment
#'   \item **pun_persev_h2:** perseverant response on hole 2 during punishment
#'   \item **pun_persev_h3:** perseverant response on hole 3 during punishment
#'   \item **pun_persev_h4:** perseverant response on hole 4 during punishment
#'   \item **pun_persev_h5:** perseverant response on hole 5 during punishment
#'   \item **pun_head_entry:** indicator of the rat responding during a time-out
#'   \item **btw_persev_h1:** Perseverant Responses to hole 1 between punishment and beginning next trial
#'   \item **btw_persev_h2:** Perseverant Responses to hole 2 between punishment and beginning next trial
#'   \item **btw_persev_h3:** Perseverant Responses to hole 3 between punishment and beginning next trial
#'   \item **btw_persev_h4:** Perseverant Responses to hole 4 between punishment and beginning next trial
#'   \item **btw_persev_h5:** Perseverant Responses to hole 5 between punishment and beginning next trial
#' }
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @references
#' Barrus, M.M., & Winstanley, C.A. (2016). Dopamine D3 receptors modulate the
#' ability of win-paired cues to increase risky choice in a rat gambling task.
#' Journal of Neuroscience, 36(3), 785-794.
#'
#' Tatham, T.A., & Zurn, K.R. (1989). The MED-PC experimental apparatus
#' programming system. Behavior Research Methods, Instruments, & Computers,
#' 21(2), 294-302.
#'
#' Zeeb, F.D., Robbins, T.W., & Winstanley, C.A. (2009). Serotonergic and
#' dopaminergic modulation of gambling behavior as assessed using a novel rat
#' gambling task. Neuropsychopharmacology, 34(10), 2329-2343.
#'
#' @export
iti9_read_file <- function(path = "") {
  if(!file.exists(path)){
    path <- rstudioapi::selectFile()
  }
  med_pc_raw <- suppressWarnings(import_medpc(path))

  increment <- 40 #number of elements displayed from line ~196 in MedPC code, under K(I) = Trial by trial Data (x elements)
  len <- length(med_pc_raw$K)/increment
  ivec <- increment*0:(len-1)

  out <- data.table::data.table(msn = med_pc_raw$MSN,
                                start_date = med_pc_raw$`Start Date`,
                                start_time = med_pc_raw$`Start Time`,
                                subject = med_pc_raw$Subject,
                                group = med_pc_raw$Group,
                                box = med_pc_raw$Box,
                                experiment = med_pc_raw$Experiment,
                                session = med_pc_raw$K[ivec + 5], #add 1 to the k(I + value) in the code file
                                trial = med_pc_raw$K[ivec + 1],
                                rewarded = med_pc_raw$K[ivec + 13],
                                pellets =  med_pc_raw$K[ivec + 14],
                                omission = med_pc_raw$K[ivec + 7],
                                chosen = med_pc_raw$K[ivec + 3],
                                choice_lat = med_pc_raw$K[ivec + 4],
                                collect_lat = med_pc_raw$K[ivec + 6],
                                pun_dur = med_pc_raw$K[ivec + 15],
                                iti_dur = med_pc_raw$K[ivec + 20],
                                premature_resp = med_pc_raw$K[ivec + 17],
                                premature_time = med_pc_raw$K[ivec + 31],
                                premature_hole = med_pc_raw$K[ivec + 16],
                                rew_persev_h1 = med_pc_raw$K[ivec + 21],
                                rew_persev_h2 = med_pc_raw$K[ivec + 22],
                                rew_persev_h3 = med_pc_raw$K[ivec + 23],
                                rew_persev_h4 = med_pc_raw$K[ivec + 24],
                                rew_persev_h5 = med_pc_raw$K[ivec + 25],
                                pun_persev_h1 = med_pc_raw$K[ivec + 8],
                                pun_persev_h2 = med_pc_raw$K[ivec + 9],
                                pun_persev_h3 = med_pc_raw$K[ivec + 10],
                                pun_persev_h4 = med_pc_raw$K[ivec + 11],
                                pun_persev_h5 = med_pc_raw$K[ivec + 12],
                                pun_head_entry = med_pc_raw$K[ivec + 18],
                                btw_persev_h1 = med_pc_raw$K[ivec + 26],
                                btw_persev_h2 = med_pc_raw$K[ivec + 27],
                                btw_persev_h3 = med_pc_raw$K[ivec + 28],
                                btw_persev_h4 = med_pc_raw$K[ivec + 29],
                                btw_persev_h5 = med_pc_raw$K[ivec + 30])

  out[, start_date := lubridate::mdy(start_date)]

  #correct premature response trial numbers
  out <- out[trial >= 1]
  out[, trial := data.table::fifelse(premature_resp != 0, trial - 1, trial)]

  return(out)
}

#' Read and combine a folder of raw MedPC files from the ITI-9 version of the cued or uncued rat gambling task.
#'
#' Read a folder of raw MedPC (Tatham & Zurn, 1989) data files from the cued
#' (Barrus & Winstanley, 2016) or uncued (Zeeb et al., 2009) versions of the rat
#' gambling task (RGT) with a 9-second intertrial interval (ITI) and convert
#' them into a single tibble with an option to export the data to a
#' comma-separated variable (.csv) file.
#'
#' @importFrom rstudioapi selectDirectory
#' @importFrom data.table rbindlist
#' @importFrom dplyr mutate
#' @importFrom readr write_csv
#' @importFrom purrr map
#'
#' @param path Character string for path to the folder containing the raw
#'   MedPC files, e.g. "~/raw_iti9_rgt_files/". A file explorer window will pop-up
#'   to allow you to choose the folder interactively if the path is unspecified.
#'
#' @param output_file Comma-separated variable (csv) file path and name to
#'   export the combined data to, which should end in ".csv". e.g.
#'   "rgt_data.csv" to save the file in your working directory.
#'
#' @param pattern If the specified folder contains non-MedPC files or other
#'   folders, this argument allows you to use a regular expression or fixed
#'   string to tell the function which files to read. For example, raw MedPC
#'   files often start with "!", so you might use pattern = "!" to only read
#'   files containing a "!" in their name. See this [this blog
#'   post](https://craig.rbind.io/post/2020-06-28-asgr-2-3-string-manipulation/)
#'   to learn more about regular expressions in R.
#'
#' @param fixed Set this to `TRUE` if you want the pattern argument to be
#'   interpreted as a fixed string pattern (i.e. exactly as written) instead of
#'   a regular expression for selecting the MedPC files in the chosen folder.
#'   Ignored if "pattern" argument is not used.
#'
#' @param negate Set this to `TRUE` to read files with names which do *not*
#'   match the specified pattern. Ignored if "pattern" argument is not used.
#'
#' @param progress Should a progress bar be displayed when reading multiple
#'   MedPC data files (default = TRUE)? See [purrr::progress_bars]
#'   for details and options.
#'
#' @return A data frame containing ITI-9 RGT data with the following columns
#' \itemize{
#'   \item **msn:** task reward contingency coding version (A or B)
#'   \item **start_date:** the session start date
#'   \item **start_time:** the session start time
#'   \item **subject:** subject identifier
#'   \item **group:** experimental group
#'   \item **box:** the operant box used to collect the data
#'   \item **experiment:** experiment identifier
#'   \item **session:** session number
#'   \item **trial:** trial number
#'   \item **rewarded:** 1 if rewarded, 0 if punished
#'   \item **pellets:** number of pellets dispensed
#'   \item **omission:** indicator of failure to respond
#'   \item **chosen:** the option chosen in the trial
#'   \item **choice_lat:** time taken to make a choice
#'   \item **collect_lat:** time to collect a reward
#'   \item **pun_dur:** timeout punishment duration
#'   \item **iti_dur:** intertrial interval duration used during data collection
#'   \item **premature_resp:** premature response indicator
#'   \item **premature_time:** premature response time
#'   \item **premature_hole:** hole chosen when prematurely responding
#'   \item **rew_persev_h1:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h2:** perseverant response on hole 2 during reward
#'   \item **rew_persev_h3:** perseverant response on hole 3 during reward
#'   \item **rew_persev_h4:** perseverant response on hole 4 during reward
#'   \item **rew_persev_h5:** perseverant response on hole 5 during reward
#'   \item **pun_persev_h1:** perseverant response on hole 1 during punishment
#'   \item **pun_persev_h2:** perseverant response on hole 2 during punishment
#'   \item **pun_persev_h3:** perseverant response on hole 3 during punishment
#'   \item **pun_persev_h4:** perseverant response on hole 4 during punishment
#'   \item **pun_persev_h5:** perseverant response on hole 5 during punishment
#'   \item **pun_head_entry:** indicator of the rat responding during a time-out
#'   \item **btw_persev_h1:** Perseverant Responses to hole 1 between punishment and beginning next trial
#'   \item **btw_persev_h2:** Perseverant Responses to hole 2 between punishment and beginning next trial
#'   \item **btw_persev_h3:** Perseverant Responses to hole 3 between punishment and beginning next trial
#'   \item **btw_persev_h4:** Perseverant Responses to hole 4 between punishment and beginning next trial
#'   \item **btw_persev_h5:** Perseverant Responses to hole 5 between punishment and beginning next trial
#' }
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @references
#' Barrus, M.M., & Winstanley, C.A. (2016). Dopamine D3 receptors modulate the
#' ability of win-paired cues to increase risky choice in a rat gambling task.
#' Journal of Neuroscience, 36(3), 785-794.
#'
#' Tatham, T.A., & Zurn, K.R. (1989). The MED-PC experimental apparatus
#' programming system. Behavior Research Methods, Instruments, & Computers,
#' 21(2), 294-302.
#'
#' Zeeb, F.D., Robbins, T.W., & Winstanley, C.A. (2009). Serotonergic and
#' dopaminergic modulation of gambling behavior as assessed using a novel rat
#' gambling task. Neuropsychopharmacology, 34(10), 2329-2343.
#'
#' @export
iti9_read <- function(path = "", output_file = NULL,
                      pattern = NULL, fixed = FALSE, negate = FALSE, progress = TRUE) {
  if(!dir.exists(path)){
    path <- rstudioapi::selectDirectory()
  }
  folder <- path
  file_list <- list.files(folder, recursive = TRUE)
  if(!missing(pattern)) {
    file_list <- grep(pattern, file_list, value = TRUE, fixed = fixed, invert = negate)
  }
  file_paths <- paste(folder, file_list, sep = "/")
  out <- purrr::map(file_paths, iti9_read_file, .progress = progress) |> #read each file and store in a list of data.tables
    data.table::rbindlist() |> #combine data frames
    wash_df() #parse column classes, remove empty

  if(!missing(output_file)) {
    if(!grepl("\\.csv$", output_file)) {
      message('output file name must end in ".csv"
              \nappending ".csv" to output file name')
    }
    readr::write_csv(out, output_file)
    message(paste0("data exported to path: ", output_file))
  }

  return(out)
}

#' Read a raw MedPC file from the delay-discounting version of the rat gambling task.
#'
#' Read a raw MedPC (Tatham & Zurn, 1989) data file from the delay-discounting
#' version of the cued (Barrus & Winstanley, 2016) or uncued (Zeeb et al., 2009)
#' rat gambling task (RGT) and convert it into an R data.table.
#'
#' @importFrom rstudioapi selectFile
#' @importFrom data.table data.table
#' @importFrom data.table fifelse
#' @importFrom data.table :=
#' @importFrom lubridate mdy
#'
#' @param path Character string for the raw MedPC file path, e.g.
#'   "!2021-11-13_7h01m.Subject 1" if the file is in your working directory. A
#'   file explorer window will pop-up to allow you to choose the file
#'   interactively if the path is unspecified.
#'
#' @return A data frame containing ITI-9 RGT data with the following columns
#' \itemize{
#'   \item **msn:** task reward contingency coding version (A or B)
#'   \item **start_date:** the session start date
#'   \item **start_time:** the session start time
#'   \item **subject:** subject identifier
#'   \item **group:** experimental group
#'   \item **box:** the operant box used to collect the data
#'   \item **experiment:** experiment identifier
#'   \item **session:** session number
#'   \item **trial:** trial number
#'   \item **rewarded:** 1 if rewarded, 0 if punished
#'   \item **pellets:** number of pellets dispensed
#'   \item **omission:** indicator of failure to respond
#'   \item **chosen:** the option chosen in the trial
#'   \item **choice_lat:** time taken to make a choice
#'   \item **collect_lat:** time to collect a reward
#'   \item **pun_persev_h1:** perseverant response on hole 1 during punishment
#'   \item **pun_persev_h2:** perseverant response on hole 2 during punishment
#'   \item **pun_persev_h3:** perseverant response on hole 3 during punishment
#'   \item **pun_persev_h4:** perseverant response on hole 4 during punishment
#'   \item **pun_persev_h5:** perseverant response on hole 5 during punishment
#'   \item **pun_head_entry:** indicator of the rat responding during a time-out
#'   \item **pun_dur:** timeout punishment duration
#'   \item **premature_resp:** premature response indicator
#'   \item **premature_hole:** hole chosen when prematurely responding
#'   \item **premature_time:** premature response time
#'   \item **rew_persev_h1:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h2:** perseverant response on hole 2 during reward
#'   \item **rew_persev_h3:** perseverant response on hole 3 during reward
#'   \item **rew_persev_h4:** perseverant response on hole 4 during reward
#'   \item **rew_persev_h5:** perseverant response on hole 5 during reward
#'   \item **chosen_iti_dur:** intertrial interval of the chosen hole
#'   \item **iti_dur_p1:** intertrial interval set for P1
#'   \item **iti_dur_p2:** intertrial interval set for P2
#'   \item **iti_dur_p3:** intertrial interval set for P3
#'   \item **iti_dur_p4:** intertrial interval set for P4
#'   \item **h1_on:** indicator of whether or not hole 1 was active when the rat responded
#'   \item **h2_on:** indicator of whether or not hole 2 was active when the rat responded
#'   \item **h4_on:** indicator of whether or not hole 4 was active when the rat responded
#'   \item **h5_on:** indicator of whether or not hole 5 was active when the rat responded
#' }
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @references
#' Barrus, M.M., & Winstanley, C.A. (2016). Dopamine D3 receptors modulate the
#' ability of win-paired cues to increase risky choice in a rat gambling task.
#' Journal of Neuroscience, 36(3), 785-794.
#'
#' Tatham, T.A., & Zurn, K.R. (1989). The MED-PC experimental apparatus
#' programming system. Behavior Research Methods, Instruments, & Computers,
#' 21(2), 294-302.
#'
#' Zeeb, F.D., Robbins, T.W., & Winstanley, C.A. (2009). Serotonergic and
#' dopaminergic modulation of gambling behavior as assessed using a novel rat
#' gambling task. Neuropsychopharmacology, 34(10), 2329-2343.
#'
#' @export
ddrgt_read_file <- function(path = "") {
  if(!file.exists(path)){
    path <- rstudioapi::selectFile()
  }
  med_pc_raw <- suppressWarnings(import_medpc(path))

  increment <- 40 #number of elements displayed from line ~196 in MedPC code, under K(I) = Trial by trial Data (x elements)
  len <- length(med_pc_raw$K)/increment
  ivec <- increment*0:(len-1)

  out <- data.table::data.table(msn = med_pc_raw$MSN,
                                start_date = med_pc_raw$`Start Date`,
                                start_time = med_pc_raw$`Start Time`,
                                subject = med_pc_raw$Subject,
                                group = med_pc_raw$Group,
                                box = med_pc_raw$Box,
                                experiment = med_pc_raw$Experiment,
                                session = med_pc_raw$K[ivec + 5], #add 1 to the k(I + value) in the code file
                                trial = med_pc_raw$K[ivec + 1],
                                rewarded = med_pc_raw$K[ivec + 13],
                                pellets =  med_pc_raw$K[ivec + 14],
                                omission = med_pc_raw$K[ivec + 7],
                                chosen = med_pc_raw$K[ivec + 3],
                                choice_lat = med_pc_raw$K[ivec + 4],
                                collect_lat = med_pc_raw$K[ivec + 6],
                                pun_persev_h1 = med_pc_raw$K[ivec + 8],
                                pun_persev_h2 = med_pc_raw$K[ivec + 9],
                                pun_persev_h3 = med_pc_raw$K[ivec + 10],
                                pun_persev_h4 = med_pc_raw$K[ivec + 11],
                                pun_persev_h5 = med_pc_raw$K[ivec + 12],
                                pun_head_entry = med_pc_raw$K[ivec + 18],
                                pun_dur = med_pc_raw$K[ivec + 15],
                                premature_resp = med_pc_raw$K[ivec + 17],
                                premature_hole = med_pc_raw$K[ivec + 16],
                                premature_time = med_pc_raw$K[ivec + 39],
                                rew_persev_h1 = med_pc_raw$K[ivec + 21],
                                rew_persev_h2 = med_pc_raw$K[ivec + 22],
                                rew_persev_h3 = med_pc_raw$K[ivec + 23],
                                rew_persev_h4 = med_pc_raw$K[ivec + 24],
                                rew_persev_h5 = med_pc_raw$K[ivec + 25],
                                chosen_iti_dur = med_pc_raw$K[ivec + 20],
                                iti_dur_p1 = med_pc_raw$K[ivec + 31],
                                iti_dur_p2 = med_pc_raw$K[ivec + 32],
                                iti_dur_p3 = med_pc_raw$K[ivec + 33],
                                iti_dur_p4 = med_pc_raw$K[ivec + 34],
                                h1_on = med_pc_raw$K[ivec + 35],
                                h2_on = med_pc_raw$K[ivec + 36],
                                h4_on = med_pc_raw$K[ivec + 37],
                                h5_on = med_pc_raw$K[ivec + 38])

  out[, start_date := lubridate::mdy(start_date)]

  #correct premature response trial numbers
  out <- out[trial >= 1]
  out[, trial := data.table::fifelse(premature_resp != 0, trial - 1, trial)]

  return(out)
}

#' Read a folder of raw MedPC files from the delay-discounting version of the rat gambling task.
#'
#' Read a folder of raw MedPC (Tatham & Zurn, 1989) data files from the delay-discounting
#' version of the cued (Barrus & Winstanley, 2016) or uncued (Zeeb et al., 2009)
#' rat gambling task (RGT) and convert them into a single tibble with an option to
#' export the data to a comma-separated variable (.csv) file.
#'
#' @importFrom rstudioapi selectFile
#' @importFrom data.table data.table
#' @importFrom data.table fifelse
#' @importFrom data.table :=
#' @importFrom lubridate mdy
#'
#' @param path Character string for path to the folder containing the raw
#'   MedPC files, e.g. "~/raw_dd_crgt_files/". A file explorer window will pop-up
#'   to allow you to choose the folder interactively if the path is unspecified.
#'
#' @param output_file Comma-separated variable (csv) file path and name to
#'   export the combined data to, which should end in ".csv". e.g.
#'   "rgt_data.csv" to save the file in your working directory.
#'
#' @param pattern If the specified folder contains non-MedPC files or other
#'   folders, this argument allows you to use a regular expression or fixed
#'   string to tell the function which files to read. For example, raw MedPC
#'   files often start with "!", so you might use pattern = "!" to only read
#'   files containing a "!" in their name. See this [this blog
#'   post](https://craig.rbind.io/post/2020-06-28-asgr-2-3-string-manipulation/)
#'   to learn more about regular expressions in R.
#'
#' @param fixed Set this to `TRUE` if you want the pattern argument to be
#'   interpreted as a fixed string pattern (i.e. exactly as written) instead of
#'   a regular expression for selecting the MedPC files in the chosen folder.
#'   Ignored if "pattern" argument is not used.
#'
#' @param negate Set this to `TRUE` to read files with names which do *not*
#'   match the specified pattern. Ignored if "pattern" argument is not used.
#'
#' @param progress Should a progress bar be displayed when reading multiple
#'   MedPC data files (default = TRUE)? See [purrr::progress_bars]
#'   for details and options.
#'
#' @return A data frame containing ITI-9 RGT data with the following columns
#' \itemize{
#'   \item **msn:** task reward contingency coding version (A or B)
#'   \item **start_date:** the session start date
#'   \item **start_time:** the session start time
#'   \item **subject:** subject identifier
#'   \item **group:** experimental group
#'   \item **box:** the operant box used to collect the data
#'   \item **experiment:** experiment identifier
#'   \item **session:** session number
#'   \item **trial:** trial number
#'   \item **rewarded:** 1 if rewarded, 0 if punished
#'   \item **pellets:** number of pellets dispensed
#'   \item **omission:** indicator of failure to respond
#'   \item **chosen:** the option chosen in the trial
#'   \item **choice_lat:** time taken to make a choice
#'   \item **collect_lat:** time to collect a reward
#'   \item **pun_persev_h1:** perseverant response on hole 1 during punishment
#'   \item **pun_persev_h2:** perseverant response on hole 2 during punishment
#'   \item **pun_persev_h3:** perseverant response on hole 3 during punishment
#'   \item **pun_persev_h4:** perseverant response on hole 4 during punishment
#'   \item **pun_persev_h5:** perseverant response on hole 5 during punishment
#'   \item **pun_head_entry:** indicator of the rat responding during a time-out
#'   \item **pun_dur:** timeout punishment duration
#'   \item **premature_resp:** premature response indicator
#'   \item **premature_hole:** hole chosen when prematurely responding
#'   \item **premature_time:** premature response time
#'   \item **rew_persev_h1:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h2:** perseverant response on hole 2 during reward
#'   \item **rew_persev_h3:** perseverant response on hole 3 during reward
#'   \item **rew_persev_h4:** perseverant response on hole 4 during reward
#'   \item **rew_persev_h5:** perseverant response on hole 5 during reward
#'   \item **chosen_iti_dur:** intertrial interval of the chosen hole
#'   \item **iti_dur_p1:** intertrial interval set for P1
#'   \item **iti_dur_p2:** intertrial interval set for P2
#'   \item **iti_dur_p3:** intertrial interval set for P3
#'   \item **iti_dur_p4:** intertrial interval set for P4
#'   \item **h1_on:** indicator of whether or not hole 1 was active when the rat responded
#'   \item **h2_on:** indicator of whether or not hole 2 was active when the rat responded
#'   \item **h4_on:** indicator of whether or not hole 4 was active when the rat responded
#'   \item **h5_on:** indicator of whether or not hole 5 was active when the rat responded
#' }
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @references
#' Barrus, M.M., & Winstanley, C.A. (2016). Dopamine D3 receptors modulate the
#' ability of win-paired cues to increase risky choice in a rat gambling task.
#' Journal of Neuroscience, 36(3), 785-794.
#'
#' Tatham, T.A., & Zurn, K.R. (1989). The MED-PC experimental apparatus
#' programming system. Behavior Research Methods, Instruments, & Computers,
#' 21(2), 294-302.
#'
#' Zeeb, F.D., Robbins, T.W., & Winstanley, C.A. (2009). Serotonergic and
#' dopaminergic modulation of gambling behavior as assessed using a novel rat
#' gambling task. Neuropsychopharmacology, 34(10), 2329-2343.
#'
#' @export
ddrgt_read <- function(path = "", output_file = NULL,
                       pattern = NULL, fixed = FALSE, negate = FALSE, progress = TRUE) {
  if(!dir.exists(path)){
    path <- rstudioapi::selectDirectory()
  }
  folder <- path
  file_list <- list.files(folder, recursive = TRUE)
  if(!missing(pattern)) {
    file_list <- grep(pattern, file_list, value = TRUE, fixed = fixed, invert = negate)
  }
  file_paths <- paste(folder, file_list, sep = "/")
  out <- purrr::map(file_paths, ddrgt_read_file, .progress = progress) |> #read each file and store in a list of data.tables
    data.table::rbindlist() |> #combine data frames
    wash_df() #parse column classes, remove empty

  if(!missing(output_file)) {
    if(!grepl("\\.csv$", output_file)) {
      message('output file name must end in ".csv"
              \nappending ".csv" to output file name')
    }
    readr::write_csv(out, output_file)
    message(paste0("data exported to path: ", output_file))
  }

  return(out)
}

#' Read a raw MedPC file from the cued or uncued rat gambling task.
#'
#' Read a raw MedPC (Tatham & Zurn, 1989) data file from the cued (Barrus &
#' Winstanley, 2016) or uncued (Zeeb et al., 2009) versions of the rat gambling
#' task (RGT) and convert it into an R data.table.
#'
#' @importFrom rstudioapi selectFile
#' @importFrom data.table data.table
#' @importFrom data.table fifelse
#' @importFrom data.table :=
#' @importFrom lubridate mdy
#' @importFrom stringr str_glue
#'
#' @param path Character string for the raw MedPC file path, e.g.
#'   "!2021-11-13_7h01m.Subject 1" if the file is in your working directory. A
#'   file explorer window will pop-up to allow you to choose the file
#'   interactively if the path is unspecified.
#'
#' @return A data frame containing rGT data with columns that depend on the
#'   source data MSN. For standard cued/uncued ITI5 data, the output will include
#' \itemize{
#'   \item **msn:** task reward contingency coding version (A or B)
#'   \item **start_date:** the session start date
#'   \item **start_time:** the session start time
#'   \item **subject:** subject identifier
#'   \item **group:** experimental group
#'   \item **box:** the operant box used to collect the data
#'   \item **experiment:** experiment identifier
#'   \item **session:** session number
#'   \item **trial:** trial number
#'   \item **rewarded:** 1 if rewarded, 0 if punished
#'   \item **pellets:** number of pellets dispensed
#'   \item **omission:** indicator of failure to respond
#'   \item **chosen:** the option chosen in the trial
#'   \item **choice_lat:** time taken to make a choice
#'   \item **collect_lat:** time to collect a reward
#'   \item **pun_persev_h1:** perseverant response on hole 1 during punishment
#'   \item **pun_persev_h2:** perseverant response on hole 2 during punishment
#'   \item **pun_persev_h3:** perseverant response on hole 3 during punishment
#'   \item **pun_persev_h4:** perseverant response on hole 4 during punishment
#'   \item **pun_persev_h5:** perseverant response on hole 5 during punishment
#'   \item **pun_head_entry:** indicator of the rat responding during a time-out
#'   \item **premature_resp:** premature response indicator
#'   \item **premature_hole:** hole chosen when prematurely responding
#'   \item **rew_persev_h1:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h2:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h3:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h4:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h5:** perseverant response on hole 1 during reward
#' }
#'
#'  ITI-9 data will also include the following columns:
#'  \itemize{
#'   \item **iti_dur:** intertrial interval duration used during data collection
#'   \item **premature_time:** premature response time
#'  }
#'
#'  DD-rGT data will also also include the following columns:
#'
#'  \itemize{
#'   \item **premature_time:** premature response time
#'   \item **chosen_iti_dur:** intertrial interval of the chosen hole
#'   \item **iti_dur_p1:** intertrial interval set for P1
#'   \item **iti_dur_p2:** intertrial interval set for P2
#'   \item **iti_dur_p3:** intertrial interval set for P3
#'   \item **iti_dur_p4:** intertrial interval set for P4
#'   \item **h1_on:** indicator of whether or not hole 1 was active when the rat responded
#'   \item **h2_on:** indicator of whether or not hole 2 was active when the rat responded
#'   \item **h4_on:** indicator of whether or not hole 4 was active when the rat responded
#'   \item **h5_on:** indicator of whether or not hole 5 was active when the rat responded
#' }
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @references
#' Barrus, M.M., & Winstanley, C.A. (2016). Dopamine D3 receptors modulate the
#' ability of win-paired cues to increase risky choice in a rat gambling task.
#' Journal of Neuroscience, 36(3), 785-794.
#'
#' Tatham, T.A., & Zurn, K.R. (1989). The MED-PC experimental apparatus
#' programming system. Behavior Research Methods, Instruments, & Computers,
#' 21(2), 294-302.
#'
#' Zeeb, F.D., Robbins, T.W., & Winstanley, C.A. (2009). Serotonergic and
#' dopaminergic modulation of gambling behavior as assessed using a novel rat
#' gambling task. Neuropsychopharmacology, 34(10), 2329-2343.
#'
#' @export
rgt_read_file <- function(path = "")  {
  if(!file.exists(path)){
    path <- rstudioapi::selectFile()
  }
  med_pc_raw <- suppressWarnings(import_medpc(path))
  msn <- med_pc_raw$MSN
  if(grepl(pattern = "rGT_", x = msn) && !grepl(pattern = "I9|DDrGT", x = msn)) {
    out <- iti5_read_file(path = path)
  } else if(grepl(pattern = "I9", x = msn)) {
    out <- iti9_read_file(path = path)
  } else if(grepl(pattern = "DDrGT", x = msn)){
    out <- ddrgt_read_file(path = path)
  } else {
    supported_msns <- c("rGT_classicA", "rGT_classicB", #uncued ITI-5
                        "rGT_A-cue", "rGT_B-cue", "RevRGT_A-cue", "RevRGT_B-cue", #cued ITI-5
                        "rGT_ClassicA_I9", "rGT_ClassicB_I9", #uncued ITI-9
                        "rGT_A-cue-I9", "rGT_A-cue_I9", "rGT_B-cue_I9", "rGT_B-cue-I9", #cued ITI-9
                        "rGT_A-cue-ITI9-v3", "rGT_B-cue-ITI9-v3",# cued ITI-9
                        "DDrGT_A-cue-v10_ideal_phenotype", "DDrGT_A-cue-v10_worsening_phenotype", #ddrGT vA
                        "DDrGT_B-cue-v10_ideal_phenotype", "DDrGT_B-cue-v10_worsening_phenotype") #ddrGT vB
    stop(
      stringr::str_glue("the MedPC program file is not supported according to the listed MSN code.\nCurrently supported programs include:\n {paste(supported_msns, collapse = '\n ')}")
    )
  }
  return(out)
}


#' Read and combine a folder of raw MedPC files from the cued or uncued rat gambling task.
#'
#' Read a folder of raw MedPC (Tatham & Zurn, 1989) data files from the cued
#' (Barrus & Winstanley, 2016) or uncued (Zeeb et al., 2009) version of the rat
#' gambling task (RGT) and convert them into a single tibble with an option to
#' export the data to a comma-separated variable (.csv) file.
#'
#' @importFrom rstudioapi selectDirectory
#' @importFrom data.table rbindlist
#' @importFrom dplyr mutate
#' @importFrom readr write_csv
#' @importFrom purrr map
#' @importFrom purrr map_chr
#'
#' @param path Character string for path to the folder containing the raw
#'   MedPC files, e.g. "~/raw_rgt_files/". A file explorer window will pop-up
#'   to allow you to choose the folder interactively if the path is unspecified.
#'
#' @param output_file Comma-separated variable (csv) file path and name to
#'   export the combined data to, which should end in ".csv". e.g.
#'   "rgt_data.csv" to save the file in your working directory.
#'
#' @param pattern If the specified folder contains non-MedPC files or other
#'   folders, this argument allows you to use a regular expression or fixed
#'   string to tell the function which files to read. For example, raw MedPC
#'   files often start with "!", so you might use pattern = "!" to only read
#'   files containing a "!" in their name. See this [this blog
#'   post](https://craig.rbind.io/post/2020-06-28-asgr-2-3-string-manipulation/)
#'   to learn more about regular expressions in R.
#'
#' @param fixed Set this to `TRUE` if you want the pattern argument to be
#'   interpreted as a fixed string pattern (i.e. exactly as written) instead of
#'   a regular expression for selecting the MedPC files in the chosen folder.
#'   Ignored if "pattern" argument is not used.
#'
#' @param negate Set this to `TRUE` to read files with names which do *not*
#'   match the specified pattern. Ignored if "pattern" argument is not used.
#'
#' @param progress Should a progress bar be displayed when reading multiple
#'   MedPC data files (default = TRUE)? See [purrr::progress_bars]
#'   for details and options.
#'
#' @return A data frame containing rGT data with columns that depend on the
#'   source data MSN. For standard cued/uncued ITI5 data, the output will include
#' \itemize{
#'   \item **msn:** task reward contingency coding version (A or B)
#'   \item **start_date:** the session start date
#'   \item **start_time:** the session start time
#'   \item **subject:** subject identifier
#'   \item **group:** experimental group
#'   \item **box:** the operant box used to collect the data
#'   \item **experiment:** experiment identifier
#'   \item **session:** session number
#'   \item **trial:** trial number
#'   \item **rewarded:** 1 if rewarded, 0 if punished
#'   \item **pellets:** number of pellets dispensed
#'   \item **omission:** indicator of failure to respond
#'   \item **chosen:** the option chosen in the trial
#'   \item **choice_lat:** time taken to make a choice
#'   \item **collect_lat:** time to collect a reward
#'   \item **pun_persev_h1:** perseverant response on hole 1 during punishment
#'   \item **pun_persev_h2:** perseverant response on hole 2 during punishment
#'   \item **pun_persev_h3:** perseverant response on hole 3 during punishment
#'   \item **pun_persev_h4:** perseverant response on hole 4 during punishment
#'   \item **pun_persev_h5:** perseverant response on hole 5 during punishment
#'   \item **pun_head_entry:** indicator of the rat responding during a time-out
#'   \item **premature_resp:** premature response indicator
#'   \item **premature_hole:** hole chosen when prematurely responding
#'   \item **rew_persev_h1:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h2:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h3:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h4:** perseverant response on hole 1 during reward
#'   \item **rew_persev_h5:** perseverant response on hole 1 during reward
#' }
#'
#'  ITI-9 data will also include the following columns:
#'  \itemize{
#'   \item **iti_dur:** intertrial interval duration used during data collection
#'   \item **premature_time:** premature response time
#'  }
#'
#'  DD-rGT data will also include the following columns:
#'
#'  \itemize{
#'   \item **premature_time:** premature response time
#'   \item **chosen_iti_dur:** intertrial interval of the chosen hole
#'   \item **iti_dur_p1:** intertrial interval set for P1
#'   \item **iti_dur_p2:** intertrial interval set for P2
#'   \item **iti_dur_p3:** intertrial interval set for P3
#'   \item **iti_dur_p4:** intertrial interval set for P4
#'   \item **h1_on:** indicator of whether or not hole 1 was active when the rat responded
#'   \item **h2_on:** indicator of whether or not hole 2 was active when the rat responded
#'   \item **h4_on:** indicator of whether or not hole 4 was active when the rat responded
#'   \item **h5_on:** indicator of whether or not hole 5 was active when the rat responded
#' }
#'
#' @author Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @references
#' Barrus, M.M., & Winstanley, C.A. (2016). Dopamine D3 receptors modulate the
#' ability of win-paired cues to increase risky choice in a rat gambling task.
#' Journal of Neuroscience, 36(3), 785-794.
#'
#' Tatham, T.A., & Zurn, K.R. (1989). The MED-PC experimental apparatus
#' programming system. Behavior Research Methods, Instruments, & Computers,
#' 21(2), 294-302.
#'
#' Zeeb, F.D., Robbins, T.W., & Winstanley, C.A. (2009). Serotonergic and
#' dopaminergic modulation of gambling behavior as assessed using a novel rat
#' gambling task. Neuropsychopharmacology, 34(10), 2329-2343.
#'
#' @export
rgt_read <- function(path = "", output_file = NULL,
                      pattern = NULL, fixed = FALSE, negate = FALSE, progress = TRUE) {
  if(!dir.exists(path)){
    path <- rstudioapi::selectDirectory()
  }
  folder <- path

  file_list <- list.files(folder, recursive = TRUE)

  if(!missing(pattern)) {
    file_list <- grep(pattern, file_list, value = TRUE, fixed = fixed, invert = negate)
  }
  file_paths <- paste(folder, file_list, sep = "/")

  msns <- purrr::map_chr(file_paths, ~import_medpc(.x)$MSN) |> unique()
  iti5_present <- any(!grepl(pattern = "I9|DDrGT", x = msns) & grepl(pattern = "rGT_", x = msns))
  iti9_present <- any(grepl(pattern = "I9", x = msns))
  ddrgt_present <- any(grepl(pattern = "DDrGT", x = msns))

  if(
    (iti5_present & iti9_present) || (iti5_present & ddrgt_present) ||(iti9_present & ddrgt_present)) {
    warning(paste(
      "A combination of ITI-5, ITI-9, and/or ddrGT data sources detected.",
      "  Will attempt to parse each file using the correct table profile and then join them.",
      "  If output is flawed consider separating files and attemping to parse one rGT data type at a time (ITI-5 or ITI-9 or ddRGT).",
      sep = "\n")
      )
    out <- purrr::map(file_paths, rgt_read_file, .progress = progress) |> #read each file and store in a list of data.tables
      # purrr::reduce(~dplyr::full_join(.x)) |> #combine data frames
      # wash_df() #parse column classes, remove empty
      data.table::rbindlist(fill = TRUE) |> #combine data frames
      wash_df() #parse column classes, remove empty
  } else if(iti5_present) {
    out <- purrr::map(file_paths, iti5_read_file, .progress = progress) |> #read each file and store in a list of data.tables
      data.table::rbindlist() |> #combine data frames
      wash_df() #parse column classes, remove empty
    return(out)
  } else if(iti9_present) {
    out <- purrr::map(file_paths, iti9_read_file, .progress = progress) |> #read each file and store in a list of data.tables
      data.table::rbindlist() |> #combine data frames
      wash_df() #parse column classes, remove empty
    return(out)
  } else {
    out <- purrr::map(file_paths, ddrgt_read_file, .progress = progress) |> #read each file and store in a list of data.tables
      data.table::rbindlist() |> #combine data frames
      wash_df() #parse column classes, remove empty
    return(out)
  }

  if(!missing(output_file)) {
    if(!grepl("\\.csv$", output_file)) {
      message('output file name must end in ".csv"
              \nappending ".csv" to output file name')
    }
    readr::write_csv(out, output_file)
    message(paste0("data exported to path: ", output_file))
  }

  return(out)
}
