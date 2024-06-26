% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_medpc_files.R
\name{fcsrt_read}
\alias{fcsrt_read}
\title{Read and combine a folder of raw MedPC files from the five choice serial reaction time task.}
\usage{
fcsrt_read(
  path = "",
  output_file = NULL,
  pattern = NULL,
  fixed = FALSE,
  negate = FALSE,
  progress = TRUE
)
}
\arguments{
\item{path}{Character string for path to the folder containing the raw
MedPC files, e.g. "~/raw_5csrt_files/". A file explorer window will pop-up
to allow you to choose the folder interactively if the path is unspecified.}

\item{output_file}{Comma-separated variable (csv) file path and name to
export the combined data to, which should end in ".csv". e.g.
"5csrt_data.csv" to save the file in your working directory.}

\item{pattern}{If the specified folder contains non-MedPC files or other
folders, this argument allows you to use a regular expression or fixed
string to tell the function which files to read. For example, raw MedPC
files often start with "!", so you might use pattern = "!" to only read
files containing a "!" in their name. See this \href{https://craig.rbind.io/post/2020-06-28-asgr-2-3-string-manipulation/}{this blog post}
to learn more about regular expressions in R.}

\item{fixed}{Set this to \code{TRUE} if you want the pattern argument to be
interpreted as a fixed string pattern (i.e. exactly as written) instead of
a regular expression for selecting the MedPC files in the chosen folder.
Ignored if "pattern" argument is not used.}

\item{negate}{Set this to \code{TRUE} to read files with names which do \emph{not}
match the specified pattern. Ignored if "pattern" argument is not used.}

\item{progress}{Should a progress bar be displayed when reading multiple
MedPC data files (default = TRUE)? See \link[purrr:progress_bars]{purrr::progress_bars}
for details and options.}
}
\value{
A data frame containing 5CSRT data with the following columns
\itemize{
\item \strong{msn:} task version
\item \strong{start_date:} the session start date
\item \strong{start_time:} the session start time
\item \strong{subject:} subject identifier
\item \strong{box:} the operant box used to collect the data
\item \strong{comment:} any comments that were noted in MedPC
\item \strong{session:} session number
\item \strong{stim_dur:} stimulus duration
\item \strong{iti_dur:} inter-trial-interval duration
\item \strong{trial:} trial number
\item \strong{offered:} the choice offered in the trial
\item \strong{chosen:} the option chosen in the trial
\item \strong{correct_latency:} response time when correct
\item \strong{incorrect_latency:} response time when incorrect
\item \strong{collection_latency:} time to collect a reward
\item \strong{omission:} indicator of failure to respond
\item \strong{prematures:} premature response indicator
\item \strong{persev_r1:} perseveration on hole 1
\item \strong{persev_r2:} perseveration on hole 2
\item \strong{persev_r3:} perseveration on hole 3
\item \strong{persev_r4:} perseveration on hole 4
\item \strong{persev_r5:} perseveration on hole 5
\item \strong{traypokes:} tray nosepoke count
}
}
\description{
Read a folder of raw MedPC (Tatham & Zurn, 1989) data files from the 5 choice
serial reaction time task (5CSRT; Robbins, 2002) and combine them into a
single R tibble with an option to export the data to a comma-separated
variable (.csv) file.
}
\references{
Robbins, T. (2002). The 5-choice serial reaction time task: behavioural
pharmacology and functional neurochemistry. Psychopharmacology, 163(3),
362-380.

Tatham, T. A., & Zurn, K. R. (1989). The MED-PC experimental apparatus
programming system. Behavior Research Methods, Instruments, & Computers,
21(2), 294-302.
}
\author{
Craig P. Hutton, \email{craig.hutton@gmail.com}
}
