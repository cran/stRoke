# stRoke 23.1.7

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
