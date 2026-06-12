# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## IMPORTANT: this repo has been split into a package family (June 2026)

This repository (`nickmckay/GeoChronR`) is the **original monolithic geoChronR**, preserved
on `master` and the `refactor` branch. The codebase it documents below still lives here, but
active development moved to a family of smaller, interoperable packages in **separate local
repos** under `~/GitHub/`. When continuing this work, edit those repos, not this one.

Dependency DAG (no cycles): **ens ← lipdViz ← geoChronR**, with actR and compositeR on top.

| Repo (local dir = `~/GitHub/...`) | GitHub | Branch | Role |
|---|---|---|---|
| `ens` | nickmckay/ens | main | Ensemble methods: uncertainty propagation + null-hypothesis engine (`propagateUncertainty`/`testNullHypothesis`/`kdePval`), correlation, regression, PCA, spectra, binning, BAM core (`simulateBam`), LiPD ensemble plumbing (`selectData`, `mapAgeEnsembleToPaleoData`). Foundation of the family. |
| `lipdViz` | nickmckay/lipdViz | main | All plotting + mapping; imports ens; provides `plot.*` methods for ens result classes |
| `geoChronR-chronOnly` | nickmckay/geoChronR-chronOnly | main | **geoChronR 2.0** — age modeling only (Bacon/Bchron/OxCal/BAM) + LiPD model infra; imports and re-exports the full ens+lipdViz API so `library(geoChronR)` keeps the 1.x interface |
| `actR` | **LinkedEarth/actR** | refactor | Abrupt-change detection; each detector is a `changeFun(time,vals,...)` on the ens engine |
| `compositeR` | nickmckay/compositeR | refactor | Record compositing on ens/lipdViz; `binFun`/`stanFun` plugin pattern |

Status: all 5 repos pass R CMD check + CI on Windows/Linux/macOS with full vignettes.
See memory `[[three-package-split]]` and `[[actr-compositer-stack]]`, and plan file
`~/.claude/plans/velvety-sprouting-finch.md`. Deferred TODOs: actR vctrs/dplyr class-restore;
eval-string param-storage refactor in the UQ engine.

Everything below describes the **monolithic** package as it was before the split (still
accurate for this repo's code, and for understanding what each piece does).

## Overview

geoChronR is an R package for analyzing and visualizing time-uncertain paleoscience data. It provides tools for:
- Generating age models using Bacon, Bchron, OxCal, and BAM
- Creating and analyzing age-ensemble data
- Time-uncertain correlation, regression, spectral analysis, and PCA
- Visualizing paleoclimate data and uncertainty

The package is designed to work with LiPD (Linked Paleo Data) formatted files.

## Development Commands

### Building and Testing
```r
# Install from source (for development)
remotes::install_github("nickmckay/geoChronR")

# Load the package in development mode
devtools::load_all()

# Build documentation
devtools::document()

# Run R CMD check
devtools::check()

# Run tests
devtools::test()
# Or using testthat directly:
testthat::test_check("geoChronR")
```

### Package Building
```r
# Build vignettes
devtools::build_vignettes()

# Build package
devtools::build()
```

### Git Workflow
**When making changes to the codebase, always create a git commit after completing work.** This makes it easy to track changes and revert if needed.

```bash
# After making changes, stage and commit
git add .
git commit -m "Descriptive message about what changed"

# For code changes, use descriptive commit messages like:
# - "Add function to calculate sediment accumulation rates"
# - "Fix bug in age uncertainty calculation for radiocarbon dates"
# - "Update plotChronEns() to support custom color palettes"
# - "Refactor ensemble interpolation for better performance"
```

## Architecture

### Core Components

**Age Model Generation (R/bacon.lipd.R, R/run.bchron.LiPD.R, R/oxcal.R, R/BAM.lipd.R)**
- `runBacon()` - Bayesian accumulation model using rbacon
- `runBchron()` - Bayesian chronology using Bchron
- `runOxcal()` - OxCal integration for calibration
- `runBam()` - Bacon-derived age models
- These functions create ensemble tables stored in LiPD chronData/model structure

**LiPD Data Manipulation (R/lipd.manipulation.R)**
- Functions for working with LiPD objects (lists with nested structure)
- `selectData()` - Extract variables from LiPD objects
- `mapAgeEnsembleToPaleoData()` - Map chronology ensembles to paleo measurements
- `pullTsVariable()` - Extract variables from TS (timeseries) objects
- LiPD structure: L$chronData[[i]]$model[[j]]$ensembleTable[[k]] and L$paleoData[[i]]$measurementTable[[j]]

**Chronology Building Utilities (R/chronBuilding.R)**
- `createChronMeasInputDf()` - Standardize chron measurement data into dataframes for age models
- Interactive functions that prompt users for variable selection
- Uses a global environment (`geoChronREnv`) to store session variables like `chron_varUsedStr`

**Ensemble Analysis**
- R/correlation.regression.functions.R - `corEns()`, `regressEns()`
- R/power.spectra.ens.R - `computeSpectraEns()`, spectral analysis
- R/PCA.geoChronR.R - `pcaEns()` for ensemble PCA
- All ensemble functions operate on matrices where columns are ensemble members

**Visualization (R/plotting.geoChronR.R)**
- ggplot2-based plotting functions
- `plotChronEns()` - Visualize age model ensembles
- `plotTimeseriesEnsRibbons()`, `plotTimeseriesEnsLines()` - Timeseries with uncertainty
- `plotCorEns()`, `plotRegressEns()`, `plotSpectraEns()`, `plotPcaEns()` - Analysis plots
- Many plotting functions use magrittr pipes (%>%) for composability

**Statistical Utilities (R/helper.R, R/fdr.R)**
- `gaussianize()` - Transform data to normal distribution
- `convertBP2AD()`, `convertAD2BP()` - Year conversion utilities
- `simulateAutoCorrelatedUncertainty()` - Generate AR1 noise
- `fdr()` - False discovery rate calculations

**Model Creation (R/createModel.R)**
- `createModel()` - Generic model creation framework
- `createMultiModelEnsemble()` - Combine multiple age models

### Data Flow Pattern

1. **Load LiPD data** - Using lipdR package: `L <- readLipd()`
2. **Create age model** - Using runBacon/runBchron/runOxcal, stores ensemble in chronData
3. **Map to measurements** - `mapAgeEnsembleToPaleoData()` interpolates age ensemble to paleo depths
4. **Extract variables** - `selectData()` pulls age and paleo data ensembles
5. **Analyze ensembles** - Use corEns(), regressEns(), computeSpectraEns(), etc.
6. **Visualize results** - Plot functions that handle ensemble uncertainty

### Key Design Patterns

**Interactive Mode**: Many functions (runBacon, runBchron, createChronMeasInputDf) run interactively if parameters are NA, prompting user with `askUser()` function using crayon-colored output.

**Ensemble Representation**: Ensembles stored as matrices with rows=observations, columns=ensemble members. Individual ensemble members selected with `pullInstance()`.

**LiPD Variable Structure**: Variables are lists with `$values`, `$variableName`, `$units`, `$description` components.

**Parameter Reuse**: Functions like `createChronMeasInputDf()` store used parameters in `geoChronREnv` for reuse via `getLastVarString()`.

## Important Notes

- **roxygen2** for documentation: All exported functions use roxygen2 comments (#')
- **Dependencies**: Heavy reliance on Bacon, Bchron (now from GitHub), oxcAAR, lipdR (from GitHub)
- **Testing**: tests/testthat/test-binning.R locks bin/binEns/corMatrix/corEns behavior against golden reference outputs (regenerate with tools/capture_reference.R); vignettes validate the rest
- **Vignettes**: Comprehensive tutorials in vignettes/ demonstrate full workflows
- **CI/CD**: GitHub Actions R-CMD-check runs on Windows and macOS with full vignette builds. Caution: the macOS runners sporadically segfault inside dplyr's C++ internals, so avoid introducing new dplyr calls in package code paths that vignettes exercise (use base R there); existing dplyr usage that passes CI is fine

## Common Gotchas

1. **Bacon directory handling**: Uses temp directories by default; manage with `getBaconDir()`/`setBaconDir()`
2. **Year 0 issue**: AD/BP conversion accounts for lack of year 0 (see convertBP2AD/convertAD2BP)
3. **Interactive functions**: Set parameters explicitly for non-interactive/reproducible workflows
4. **Unit conversions**: Functions check for ka vs yr and convert automatically in age model creation
5. **LiPD nested structure**: Navigate carefully - chronData/paleoData are lists of lists of lists
6. **Global environment**: Package uses `geoChronREnv` to maintain state between function calls
