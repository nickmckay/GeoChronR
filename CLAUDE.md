# CLAUDE.md

geoChronR 2.0 is a **thin wrapper**. There is almost no code here: `R/reexports.R` re-exports agemodelR, ens and lipdViz so `library(geoChronR)` keeps the 1.x interface. Work on actual functionality belongs in those repos.

Dependency order: **ens <- lipdViz <- agemodelR <- geoChronR**, with actR and compositeR built on ens + lipdViz.

| Repo (`~/GitHub/...`) | GitHub | Branch | Role |
|---|---|---|---|
| lipdR | nickmckay/lipdR | main | Read and write LiPD; foundation for everything |
| ens | nickmckay/ens | main | Ensemble methods |
| lipdViz | nickmckay/lipdViz | main | Plotting and mapping |
| agemodelR | nickmckay/agemodelR | main | Age modeling (Bacon, Bchron, OxCal, BAM) |
| GeoChronR (this repo) | nickmckay/GeoChronR | master | Wrapper |
| actR | LinkedEarth/actR | main | Abrupt-change detection |
| compositeR | nickmckay/compositeR | master | Record compositing |

- `R/reexports.R` is generated. After any export changes in a sub-package, run `Rscript tools/gen_reexports.R` and `devtools::document()`.
- `tests/testthat/exports-1x.txt` is the frozen 1.x export list; every name must stay available.
- The tutorials in `vignettes/` (all but `overview.Rmd`) are website-only: `.Rbuildignore` keeps them out of the package and `pkgdown` builds them, which keeps their public URLs. They use `library(geoChronR)`, so a pkgdown build tests the whole family.
- The monolith is at tag `v1.1.19` and branch `monolith-1.x`. Do not develop there.
