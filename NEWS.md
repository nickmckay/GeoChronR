# geoChronR 1.1.14

# geoChronR 1.1.13

* Reference to `spatstat.explore::CDF.density` replaced with `spatstat.univar::CDF.density` following update to spatstat v3.0.0

# geoChronR 1.1.12

* added `oxcal.code.export.path` option to `runOxcal()` to allow users to optionally save the oxcal model code before it runs

# geoChronR 1.1.11

* Attempted to fix the bug that resulted from the rEDM v1.15.0 name change of `make_surrogate_data` to `SurrogateData`

# geoChronR 1.1.10

* Fixed the many `if(is.na(parameter))...` statements that broke with R version 4.2
* Modified the use of the spatstat package (from spatstat.core to spatstat.summary) to accommodate the changes in that package.

# geoChronR 1.1.9

* Fixed a bug in `writeBacon()` where `remove.rejected` didn't work properly if there was exactly one rejected date. Thanks @JanPetrik1 for reporting - see [issue 71](https://github.com/nickmckay/GeoChronR/issues/71).

# geoChronR 1.1.8

* Added an `ask.reservoir` option to `runBacon()` that connects to the same parameter in `writeBacon()`. Setting `ask.reservoir = FALSE` will allow for non-interactive code that uses `runBacon()` and includes reservoir corrections. Thanks @JanPetrik1 for reporting - see [issue 70](https://github.com/nickmckay/GeoChronR/issues/70) for details.

# geoChronR 1.1.7

* Fixed shape mapping bug with unknown archiveType (thanks Sarah Ivory for reporting)

# geoChronR 1.1.6

* Moved *nuspectral* to suggested package, as it's rarely used and requires compilation [See issue 62](https://github.com/nickmckay/GeoChronR/issues/62)
* Removed *devtools* from required packages - it shouldn't be necessary. 

# geoChronR 1.1.5

* Add an option to erase the bacon tempdir (if using) before each run, and set to default.
* Fixed bug in `writeBacon()` that was making the model overwriting option not work properly. Thanks Sofia Kjellman for reporting the bug.

# geoChronR 1.1.4

* Better error messages in `binTs()`
* Better error handling in `pcaEns()`

# geoChronR 1.1.3

* Fixed bug in `createOxCalModel()` that was causing errors with IntCal20
* Suppressed non important warning ("Density was normalised within the computed range of x values [0, 1]"") in `pvalMonteCarlo()` that occurred during ecdf estimation

# geoChronR 1.1.2

* add time.var option to `runBam()`
* better error handling in `ar1Surrogates()`
* better logic and consistency checking in `mapAgeEnsembleToPaleoData()`
* Create median variable option now included in `mapAgeEnsembleToPaleoData()`

# geoChronR 1.1.1

* Fix (at least temporarily) rare bugs in `pcaEns()` and `fdr()`

# geoChronR 1.1.0

* A bunch of quality of life improvements
* Better integration with new lipdR classes
* A new function to create multi-model ensembles! `createMultiModelEnsemble()`

# geoChronR 1.0.14

* removed unused arima() in `createSyntheticTimeseries()`

# geoChronR 1.0.13

* Added explicit references to astrochron function `linterp()` in `computeSpectraEns()`. [See issue 55](https://github.com/nickmckay/GeoChronR/issues/55)

# geoChronR 1.0.12

* Improved documentation
* `runBchron()`: `which.table` parameter changed to `meas.table.num` for consistency. 

# geoChronR 1.0.11

* Removed jitterPositions option following `Bchron` update. [See issue 53](https://github.com/nickmckay/GeoChronR/issues/53)

# geoChronR 1.0.10

* Added handling for shapes in `mapTs()` for tibbles.

# geoChronR 1.0.9

* Fixed bug writing Bchron output into LiPD structure. Thanks @alexkjames for recognizing the bug.

# geoChronR 1.0.8

* Modified the use of the spatstat package (to spatstat.core) to accommodate the changes in that package.
* Moved astrochron back to a "required" package, now that it's back.

# geoChronR 1.0.7

* Moved astrochron to a "suggested" package, after it was removed from CRAN.

# geoChronR 1.0.6

* Fixed sampleBaconAges() for handling different versions of R and rbacon.

# geoChronR 1.0.5

* Improve and document the `export.quantiles` option in `plotTimeseriesEnsRibbons`
* Fix bug in `plotTimeAvailabilityTs()` when only one instance of the `group.var` [see issue 49](https://github.com/nickmckay/GeoChronR/issues/49)
* Added support for flipping timeseries vertically  in `plotTimeseriesStack` to match interpertations, based on TS metadata, specified in `invert.var`. Details in `?plotTimeseriesStack`. [See issue 30](https://github.com/nickmckay/GeoChronR/issues/30)

# geoChronR 1.0.4

* Better automatic detection for mapping ranges in `baseMap()`

# geoChronR 1.0.3

* Added interoperability with "tidyTs" objects, that is, TS objects expressed as tidy (or nested) tibbles.
* Fixed binding warning in `plotTimeAvailabilityTs()`
* Fixed bug in `mapTs()` when the regions were really small

# geoChronR 1.0.2

* Fixed a bug in `plotTimeAvailabililyTs()` where if `group.var` had a variable with a single instance it would fail. 

# geoChronR 1.0.1

* Added a `NEWS.md` file to track changes to the package.
* Added pkgdown documentation, website available at nickmckay.github.io/GeoChronR
* Updated vignettes for consistency with Geochronology manuscript and version 1.0.0
* Fixed typo in `plotCorEns()` legend when using `use.fdr = FALSE`

# geoChronR 1.0.0

* This release marks the submission of the manuscript describing geoChronR to "Geochronology", the manuscript, which is under discussion, [is available here](https://doi.org/10.5194/gchron-2020-25). 
* Major changes to parameter names (terms within functions) were made for version 1.0.0, standardizing them all to lower case, period separated terminology. If you've been using geoChronR for awhile, this will almost certainly break some of your code, sorry.
