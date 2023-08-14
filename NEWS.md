# crgt 0.0.1.9002 - August 14th, 2023

* Added a new function, `rgt_stability()`, which uses a logistic mixed effects model to check if subjects' choice preferences are stable across the last *n* sessions of the rat gambling task. The default is to check for stability across the past 5 sessions, but this can be adjusted by the user via the "n_sessions" argument. The inclusion of both fixed and random slope effects for the change in choice preference over time (session variable), enables an evaluation of stability for the sample as a whole (fixed effect term for the interaction between choice and session) vs. stability for each individual subject (random slopes for the effect of session clustered by subject). 

* Updated documentation and license information.

* Created a first draft of the readme and package hex sticker.

# crgt 0.0.1.9001 - August 6th, 2023

*	Package created with initial definitions for functions `import_medpc()`, `fcsrt_read_file()`, `fcsrt_read()`, `fcsrt_prep()`, `fcsrt_pivot()`, `rgt_read_file()`, `rgt_read()`, `rgt_prep()`, and `rgt_pivot()`.

* The package also provides a [ggplot2 theme](https://ggplot2.tidyverse.org/reference/index.html#themes), `theme_crgt()`, which is a modification of `ggplot2::theme_bw()`.

* `fcsrt_read_file()` reads a single raw MedPC data file from the five choice serial reaction time task (5CSRT) into R. If the file path is not specified, the user will be prompted to select the data file via `rstudioapi::selectFile()`.

* `fcsrt_read()` reads an entire folder of raw MedPC data files from the 5CSRT into R. If the data folder path is not specified, the user will be prompted to select the folder containing the data files via `rstudioapi::selectDirectory()`. A subset of files can be selected using a string pattern or regular expression using the "pattern", "fixed", and "negate" arguments. By default, a progress bar is also shown which advances as data files are processed.

* `fcsrt_prep()` takes an R data frame of 5CSRT data that has been imported via `fcsrt_read*` and aggregates it by subject (i.e., rat) and session using either a long (default) or wide format (controlled via the "shape" argument). This function yields the form of 5CSRT data that is most commonly analysed (session-aggregated analysis) for research purposes.

* `fcsrt_pivot()` uses [tidyr](https://tidyr.tidyverse.org/articles/pivot.html) to pivot session-aggregated 5CSRT data between long and wide formats, after it has been imported via `fcsrt_read*` and aggregated via `fcsrt_prep()`.

* `rgt_read_file()` reads a single raw MedPC data file from the cued or uncued versions of the rat gambling task (RGT) into R. If the file path is not specified, the user will be prompted to select the data file via `rstudioapi::selectFile()`.

* `rgt_read()` reads an entire folder of raw MedPC data files from the RGT into R. If the data folder path is not specified, the user will be prompted to select the folder containing the data files via `rstudioapi::selectDirectory()`. A subset of files can be selected using a string pattern or regular expression using the "pattern", "fixed", and "negate" arguments. By default, a progress bar is also shown which advances as data files are processed.

* `rgt_prep()` takes an R data frame of RGT data that has been imported via `rgt_read*` and aggregates it by subject (i.e., rat) and session using either a long (default) or wide format (controlled via the "shape" argument). This function yields the form of RGT data that is most commonly analysed (session-aggregated analysis) for research purposes.

* `rgt_pivot()` uses [tidyr](https://tidyr.tidyverse.org/articles/pivot.html) to pivot session-aggregated RGT data between long and wide formats, after it has been imported via `rgt_read*` and aggregated via `rgt_prep()`.

* ***N.B.*** The code for the `import_medpc()` function was derived, with minimal modifications (e.g., `scan(quiet = TRUE)`), from Gary Kane's tremendously helpful [rmedpc](https://github.com/gkane26/rmedpc) package, where it was originally defined. It has been included as an internal function with this package for convenience and stability reasons. `crgt` package users typically wouldn't call `import_medpc()` directly. Rather, it is intended to be used internally by the `*_read*` functions.
