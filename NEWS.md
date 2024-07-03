# crgt 0.0.1.9006 - May 11th, 2024

* Fixed a bug that prevented the shape = "wide" output option for `rgt_prep()` from pivoting some columns in the raw data when aggregating it by session that led to problems if a user subsequently attempted to use `rgt_pivot()` to pivot it to the long format.

# crgt 0.0.1.9005 - May 11th, 2024

* Updated the method that employed by the glmm approach to using `rgt_stability()` to estimate random slope standard errors (individual-level stability) to more closely align with precisely estimated values returned by a full Bayesian inference approach, which can provide the random slope coefficient standard errors that incorporate uncertainty in the fixed effect slope for the session term (i.e., validated against the output of an equivalent [brms](https://paul-buerkner.github.io/brms/) model). Tests comparing random slope estimates and confidence intervals for them derived from the standard errors obtained using the [glmmTMB](https://github.com/glmmTMB/glmmTMB) (frequentist) models implemented in {crgt} against [brms](https://paul-buerkner.github.io/brms/) (bayesian) models of stability on the same data show a high degree of overlap in the 95% confidence intervals obtained using the [glmmTMB](https://github.com/glmmTMB/glmmTMB) model and the 95% credible intervals from the [brms](https://paul-buerkner.github.io/brms/) model, indicating that the assumption that the random slope and fixed effect slope variances are independent of one another is reasonable for modelling stability in RGT data, supporting the use of the faster and more computationally efficient frequentist modelling approach currently implemented for `rgt_stability(method = "glmm")`.

# crgt 0.0.1.9004 - May 11th, 2024

* Updated `rgt_stability()` to provide users with the option to evaluate group-level stability using the classical approach of a repeated-measures ANOVA on arcsine-transformed data. The analysis approach ("glmm" or "rm_aov") can be specified via a new "method" argument. 

* `rgt_stability()` now also has a "between" argument that allows users to specify a between-subjects factor to include as a fixed effect in the analysis. This could be useful if you have data for both male and female rats, for example.

* Internalized the [elucidate](https://bcgov.github.io/elucidate/) functions added in the last update and changed the name of `mode()` to `mcv()` (for most common value) to prevent the naming conflict message from appearing in the console when the package is loaded.

# crgt 0.0.1.9003 - May 9th, 2024

* Updated `rgt_stability()` to use the correct subject- and choice-specific slope coefficients to derive estimates of the individual-level odds ratios for change across sessions, rather than the un-adjusted random slope coefficients from the model (obtained via `ranef()`) which represent the deviations from the marginal fixed effect session slopes for each choice option (i.e. the raw values represent deviations from the fixed effect slopes for the change in choice preferences across sessions). Note that this approach should yield much more meaningful point estimates for the change in each rat's choice preferences across sessions to evaluate individual-level stability. However, the standard errors and confidence intervals obtained this way (to construct the error bars) assume that the random slope and fixed effect slope variances are independent of one another, which may not be the case, as discussed [here](https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#confidence-intervals-on-conditional-meansblupsrandom-effects:~:text=Getting%20the%20uncertainty%20of,intercept%20for%20each%20group). When I have time, I will attempt to use either a Bayesian or bootstrapping approach to estimate the random effect coefficient standard errors to see how reasonable this assumption is, as recommended [here]( https://stackoverflow.com/questions/26198958/extracting-coefficients-and-their-standard-error-for-each-unit-in-an-lme-model-f#:~:text=Two%20alternatives%20would,the%20bootstrap%20distributions).

* Integrated the `translate()`, `mode()`, and `wash_df()` functions from the [elucidate](https://bcgov.github.io/elucidate/) package so that it could be removed as a dependency.

* Added new functions: `iti9_read_file()`, `iti9_read()`, `iti9_prep()`, and `iti9_pivot()` to support processing of MedPC data files from the variation of the cued (or uncued) rat gambling task that uses a 9-second intertrial interval and tracks a few additional measures.

* Added new functions: `ddrgt_read_file()`, `ddrgt_read()`, `ddrgt_prep()`, and `ddrgt_pivot()` to support processing of MedPC data files from the delay-discounting variation of the cued (or uncued) rat gambling task.


# crgt 0.0.1.9002 - August 14th, 2023

* Added a new function, `rgt_stability()`, which uses a logistic mixed effects model to check if subjects' choice preferences are stable across the last *n* sessions of the rat gambling task. The default is to check for stability across the past 5 sessions, but this can be adjusted by the user via the "n_sessions" argument. The inclusion of both fixed and random slope effects for the change in choice preference over time (session variable), enables an evaluation of stability for the sample as a whole (fixed effect term for the interaction between choice and session) vs. stability for each individual subject (random slopes for the effect of session clustered by subject and choice option). 

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
