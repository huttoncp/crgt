
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crgt

<img src="man/figures/crgt-hex.png" align="center" width="250px"/>

- source: [DALL-E-2](https://openai.com/dall-e-2) and
  [hexSticker](https://github.com/GuangchuangYu/hexSticker).

## Project Status

<!-- badges: start -->

![](https://img.shields.io/badge/Lifecycle-Experimental-339999)

<!-- badges: end -->

This package is currently maintained by [Craig P.
Hutton](https://craig.rbind.io/).

***N.B.*** Several new functions are actively being developed to expand
the scope of the package to support a wider range of RGT and 5CSRT data
analysis tasks. They will be added to the package as soon as they are
ready and adequately documented.

## Why `crgt`?

`crgt` provides a collection of functions to facilitate the processing
and analysis of operant rodent behavioural data from the cued and uncued
versions of the rat gambling task (RGT; [Zeeb et al.,
2009](https://www.nature.com/articles/npp200962); [Barrus & Winstanley,
2016](https://www.jneurosci.org/content/36/3/785) and the five choice
serial reaction time task (5CSRT; [Robbins,
2002](https://link.springer.com/article/10.1007/s00213-002-1154-7),
which rats are typically trained on prior to the RGT.

- A set of data-reading functions, `rgt_read_file()`, `rgt_read()`,
  `fcsrt_read_file()`, and `fcsrt_read()`, for reading raw MedPC data
  files (`*_read_file()`), or folders of data files (`*_read()`), from
  the RGT (`rgt_*`) or 5CSRT (`fcsrt_*`) and converting them into R data
  frames. These functions provide a free, open source, easy-to-use, and
  efficient alternative to programs like
  [MedPC2XL](https://med-associates.com/product/med-pc-to-excel-data-transfer-mpc2xl-utility)
  that is designed specifically to work with RGT and 5CSRT behavioural
  data. These functions are basically specialized extensions of the
  [rmedpc::import_medpc()](https://github.com/gkane26/rmedpc) function.

- An `rgt_prep()` and `fcsrt_prep()` pair of functions for preparing
  session-aggregated versions of imported RGT and 5CSRT data in either
  [long (default) or wide
  formats](https://tidyr.tidyverse.org/articles/pivot.html) to provide
  users with the measures that are most-often used in statistical
  analyses of these data by the [Winstanley laboratory at
  UBC](https://winstanleylab.psych.ubc.ca/), which created the RGT. Both
  functions also provide a convenience shortcut to export the aggregated
  data to a csv file via
  [readr::write_csv()](https://readr.tidyverse.org/reference/write_delim.html),
  either as a backup, or to enable users to analyse it with other
  software programs.

- An `rgt_pivot()` and `fcsrt_pivot()` pair of functions that make it
  easy to [pivot](https://tidyr.tidyverse.org/articles/pivot.html) the
  session-aggregated forms of RGT and 5CSRT data prepared by the
  `rgt_prep()` and `fcsrt_prep()` functions between long[^1] and
  wide[^2] formats. This should make RGT and 5CSRT data processing easy
  for both R users (who usually need the long form) and users of other
  programs like SPSS (who usually need the wide form).

- An `rgt_stability()` function which checks the last several (default
  is 5) sessions of RGT data, previously aggregated using `rgt_prep()`,
  to see if rats’ choice preferences are stable.

- A [ggplot2
  theme](https://ggplot2.tidyverse.org/reference/index.html#themes) to
  support visualization of cRGT/5CSRT data, `theme_crgt()`, that is
  similar to
  [ggplot2::theme_bw()](https://ggplot2.tidyverse.org/reference/ggtheme.html)
  but with larger default text font and line sizes.

## Installation

You can install the development version of crgt from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("huttoncp/crgt")
```

The authors of `crgt` acknowledge and express their gratitude to Gary
Kane, the author of the
[rmedpc::import_medpc()](https://github.com/gkane26/rmedpc) function,
and the authors of the other dependency packages which were used to
build `crgt`, without whose prior work this package would not exist.

## Reporting an Issue

To report bugs/issues or request feature changes, open an
[issue](https://github.com/huttoncp/crgt/issues) for the package GitHub
repo. If raising an issue, *please provide a reproducible example*
([reprex](https://www.tidyverse.org/help/)) of the problem you’re
encountering.

## Requesting Features and/or Changes

To suggest changes or code improvements, please submit a [pull
request](https://github.com/huttoncp/crgt/pulls).

## License

Copyright 2023 Craig P. Hutton

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see <https://www.gnu.org/licenses/>.

[^1]: In the *long* data format, repeated observations of the same
    individuals are represented in additional rows with session and/or
    trial indicator columns noting the observation time (i.e., a single
    column per measure and one row per observation, potentially with
    multiple observation rows per individual).

[^2]: In the *wide* data format, repeated observations of the same
    individual are represented in additional columns (i.e., a single row
    per individual, one column per observation, potentially with
    multiple columns for each measure).
