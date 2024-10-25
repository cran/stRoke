# stRoke 24.10.1

This version marks a significant change in the contents and focus of this package. Going forward I will include functions with a focus on handling clinical trial data from my own stroke research.
Other functions for general data management an project management has been migrated to the [`project.aid`](https://agdamsbo.github.io/project.aid/) package, which is moving towards CRAN submission. Install dev-version with `pak::pak("agdamsbo/project.aid")`.

### Functions:

* UPDATE: `pase_calc()` updated for uniform column naming in output as well as streamlining the function a bit.

* Moving: The following functions are moved to `agdamsbo/project.aid` to focus on (stroke) trial related functions: `str_extract()`, `add_padding()`, `age_calc()`, `chunks_of_n()`, `contrast_text()`, `files_filter()`, `quantile_cut()`, `write_ical()`.

* NEW: `mfi_calc()` calculates domain scores from the MFI questionnaire. Takes data frame of 20 ordered as the questionnaire. Default is to reverse questions with reverse scoring.

Checks set up with `rhub` v2

# stRoke 23.9.1

### Functions:

* NEW: `chunks_of_n()` uses `split()` to separate supplied vector or data frame into chunks of n. Flags to set if all but the last chunks should be exactly size n, or if they should be evenly sized of max n. Labels can be provided including regex pattern for subject naming to include in chunk names.

* NEW: `n_chunks()` is the opposite of `chunks_of_n()` and is simply a wrapper for this function to create list of n chunks based of provided vector or data frame.

* NEW: `str_extract()` will extract the substring of a character string given by a regex pattern. Came to be as a helper function for labelling chunks in `chunks_of_n()`, but will be useful on its own. Other functions doing the same exists, but this is my take only using base _R_. Draws on `REDCapCAST::strsplitx()`, where splits can be performed around a pattern.

* NEW: `add_padding()` was created out of frustration. I wanted to add padding using `sprintf("%0s",string)`, in examples for the above, but it would fail when rendering on Windows. Say hello to another function. Just very small. Defaults to adding leading zeros, to get all string to equal length with the longer string supplied.

* Deprecation: `ds2dd()` moved to `REDCapCAST::ds2dd()` as this is where it belongs.

# stRoke 23.6.3

### Bug

* Fixed `ds2dd()` bug after first practical implementation.

# stRoke 23.6.2

### Functions:

* NEW: `pase_calc()` function calculates PASE scores from raw questionnaire data. Gives sub scores as well and returns basic data quality and completeness checks. Acknowledges the difference between the scoring manual and the article by Washburn PA. et al. (1999) on including sitting work in the score calculations.

### Data:

* NEW: `pase` sample questionnaire data. Non-identifiable and for use with the `pase_calc()` function.

# stRoke 23.6.1

### Functions:

* NEW: 'color_plot()' function implements the 'contrast_text()' and is very much inspired from  'scales::show_col()'. Passes arguments to the internal 'contrast_text()'. Tests and all. Took way longer than intended.

# stRoke 23.4.1

### Functions:

* NEW: ds2dd() creates a REDCap data dictionary based on a data set for easy upload. A new vignette will be provided for example use. A separate vignette has been added.

### Notes:

* With newer additions to the package, these functions clearly has their potential use also outside stroke research.
* A new vector with REDCap metadata headers has been added. Can be called with data(metadata_names).


# stRoke 23.1.8

### Functions:

* write_ical() is an easy to use implementation of the package `library(calendar)` for easy conversion of spreadsheets to ical object. Export an .ics file using `calendar::ic_write()`. 
* contrast_text() calculates the best contrast text color for a given background color. For use in graphics.

### Notes:

* This is the first update on CRAN.

### Documentation

* Badges, lots of badges


# stRoke 23.1.7

### Notes:

* This is the version first published on CRAN as of 24.jan.2023.
* This is also the version first published to zenodo.org, and with corresponding [doi: 10.5281/zenodo.7572023](https://doi.org/10.5281/zenodo.7572023).

### Functions:

* redcap_read_tables() has been removed from the package for now. Looking to add it back later as a minimal data acquisition tool.

# stRoke 23.1.6 - failed due to dependencies

### New attempt at CRAN submission

### Functions:

* win_prop() added to implement the suggested methods in [DOI: 10.1161/STROKEAHA.121.037744](https://doi.org/10.1161/STROKEAHA.121.037744), as an implementation of "Tournament Methods" also found in `library(genodds)`. The function is based on the spreadsheet provided by the authors. A print.win_Prop is also added for printing.

### Notes:

* 23.1.5 failed on CRAN due to gt_plot(). This function has been dropped. Find it as as_ggplot() elsewhere.
* agdamsbo/REDCapRITS added as "Additional_repositories".
* Included references listed as authors.

# stRoke 23.1.5

### RELEASE ON CRAN - failed

# stRoke 0.23.1.4

### Functions:

* plot_olr() has been deprecated and removed. 
* ci_plot() functionality extended to include logistic model plotting.
* age_calc() use vapply() instead of sapply()
* gt_plot() function added to plot gt elements as ggplots. From [bstfun](https://github.com/MSKCC-Epi-Bio/bstfun). Not all done and satisfied with the layout with patchwork.

### Documentation

* Trying to complete all flags from goodpractice and inteRgrate

# stRoke 0.23.1.3

### Functions:

* files_filter() added. Simple function to get file names in path with specified filter.
* updated cpr_dob to give warnings if format is not recognised and return NAs.
* ci_plot() updated to actually handle binary factors. Uses glm(), not lm().

### Documentation:

* test, test and tests to satisfy codecov and, of course, ensure higher quality
* changes to comply with goodpractices::gp()

...and probably some more.

# stRoke 0.23.1.2

### Functions:

* cpr_dob() now includes `format=`.
* Other minor updates.

### Documentation:

* Two new vignettes
* Taking last steps in documenting before releasing on CRAN

### Other:

* New hex logo

# stRoke 0.23.1.1

### Functions:

* age_calc() now also outputs a numeric vector for units="days", and not a character vector as difftime() would.

### Documentation:

* Added a `NEWS.md` file to track changes to the package.
* Added codecov
* Added tests with the help of gpttools
