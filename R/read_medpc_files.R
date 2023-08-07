
# importing functions -----------------------------------------------------

#' Read a raw MedPC file from the five choice serial reaction time task.
#'
#' Read a raw MedPC (Tatham & Zurn, 1989) data file from the 5 choice serial
#' reaction time task (5CSRT; Robbins, 2002) and convert it into an R data.table
#' with an option to export the data to a Microsoft Excel file.
#'
#' @importFrom rstudioapi selectFile
#' @importFrom data.table data.table
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
                                prematures = med_pc_raw$K[14 + ivec],
                                persev_r1 = med_pc_raw$K[8 + ivec],
                                persev_r2 = med_pc_raw$K[9 + ivec],
                                persev_r3 = med_pc_raw$K[10 + ivec],
                                persev_r4 = med_pc_raw$K[11 + ivec],
                                persev_r5 = med_pc_raw$K[12 + ivec],
                                traypokes = med_pc_raw$K[18 + ivec]) |>
    dplyr::mutate(start_date = lubridate::mdy(start_date))

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
#' @importFrom elucidate wash_df
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
    elucidate::wash_df() #parse column classes, remove empty

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
#' task (RGT) and convert it into an R data frame with an option to export the
#' data to a comma-separated variable (.csv) file.
#'
#' @importFrom rstudioapi selectFile
#' @importFrom data.table  data.table
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
#'   \item **pun_persev_h1:** perseverative response on hole 1 if punished
#'   \item **pun_persev_h2:** perseverative response on hole 2 if punished
#'   \item **pun_persev_h3:** perseverative response on hole 3 if punished
#'   \item **pun_persev_h4:** perseverative response on hole 4 if punished
#'   \item **pun_persev_h5:** perseverative response on hole 5 if punished
#'   \item **pun_head_entry:** indicator of the rat responding during a time-out
#'   \item **premature_resp:** premature response indicator
#'   \item **premature_hole:** hole chosen when prematurely responding
#'   \item **rew_persev_h1:** perseverative response on hole 1 if rewarded
#'   \item **rew_persev_h2:** perseverative response on hole 1 if rewarded
#'   \item **rew_persev_h3:** perseverative response on hole 1 if rewarded
#'   \item **rew_persev_h4:** perseverative response on hole 1 if rewarded
#'   \item **rew_persev_h5:** perseverative response on hole 1 if rewarded
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
                                rew_persev_h5 = med_pc_raw$K[25 + ivec]) |>
    dplyr::mutate(start_date = lubridate::mdy(start_date))
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
#' @importFrom elucidate wash_df
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
#'   \item **pun_persev_h1:** perseverative response on hole 1 if punished
#'   \item **pun_persev_h2:** perseverative response on hole 2 if punished
#'   \item **pun_persev_h3:** perseverative response on hole 3 if punished
#'   \item **pun_persev_h4:** perseverative response on hole 4 if punished
#'   \item **pun_persev_h5:** perseverative response on hole 5 if punished
#'   \item **pun_head_entry:** indicator of the rat responding during a time-out
#'   \item **premature_resp:** premature response indicator
#'   \item **premature_hole:** hole chosen when prematurely responding
#'   \item **rew_persev_h1:** perseverative response on hole 1 if rewarded
#'   \item **rew_persev_h2:** perseverative response on hole 1 if rewarded
#'   \item **rew_persev_h3:** perseverative response on hole 1 if rewarded
#'   \item **rew_persev_h4:** perseverative response on hole 1 if rewarded
#'   \item **rew_persev_h5:** perseverative response on hole 1 if rewarded
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
  out <- purrr::map(file_paths, rgt_read_file, .progress = progress) |> #read each file and store in a list of data.tables
    data.table::rbindlist() |> #combine data frames
    elucidate::wash_df() #parse column classes, remove empty

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
