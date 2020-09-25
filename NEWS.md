# geoChronR 1.0.3

* Added interoperability with "tidyTs" objects, that is, TS objects expressed as tidy (or nested) tibbles.
* Fixed binding warning in `plotTimeAvailabililyTs()`
* Fixed bug in `mapTs()` when the regions were really small

# geoChronR 1.0.2

* Fixed a bug in `plotTimeAvailabililyTs()` where if `group.var` had a variable with a single instance it would fail. 

# geoChronR 1.0.1

* Added a `NEWS.md` file to track changes to the package.
* Added pkgdown documentation, website available at nickmckay.github.io/GeoChronR
* Updated vignettes for consistency with Geochronology manuscript and version 1.0.0
* Fixed typo in `plotCorEns()` legend when using `use.fdr = FALSE`

# geoChronR 1.0.0

* This release marks the submission of the manuscript describing GeoChronR to "Geochronology", the manuscript, which is under discussion, [is available here](https://doi.org/10.5194/gchron-2020-25). 
* Major changes to parameter names (terms within functions) were made for version 1.0.0, standardizing them all to lower case, period separated terminology. If you've been using geoChronR for awhile, this will almost certainly break some of your code, sorry.
