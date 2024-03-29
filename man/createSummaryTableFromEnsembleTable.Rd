% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createModel.R
\name{createSummaryTableFromEnsembleTable}
\alias{createSummaryTableFromEnsembleTable}
\title{Create a summary table from an ensemble table}
\usage{
createSummaryTableFromEnsembleTable(
  L,
  paleo.or.chron = "chronData",
  paleo.or.chron.num = NA,
  model.num = NA,
  ens.table.num = NA,
  ens.var = "ageEnsemble",
  probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
  prob.vars = c("unc2.5", "unc25", "ageMedian", "unc75", "unc97.5"),
  depth.var = "depth"
)
}
\arguments{
\item{L}{A LiPD object - an R serialization of a single LiPD file. It's a list, and is typically created by `readLipd()`}

\item{paleo.or.chron}{"paleoData" or "chronData"}

\item{paleo.or.chron.num}{an integer that corresponds to paleo.num or chron Data object (L$<paleo.or.chron>[[?]]) has the variable you want?}

\item{model.num}{an integer that corresponds to model.num that has the variable you want}

\item{ens.table.num}{an integer that corresponds to ensembleTable you want to get the variable from?}

\item{ens.var}{The name of the ensemble variable (default = "ageEnsemble")}

\item{probs}{A vector of probabilites (quantiles) should be computed for the summary? (default = c(0.025,0.25,.5,.75,.975))}

\item{prob.vars}{A vector of variableNames for the probabilites in probs (default =  c("unc2.5","unc25","ageMedian","unc75","unc97.5"))}

\item{depth.var}{The name of the depth (or position) variable in the ensemble table (default = "depth")}
}
\value{
A LiPD object with an added ensemble table
}
\description{
Create a summary table from an ensemble table
}
\seealso{
Other LiPD manipulation: 
\code{\link{createConcatenatedEnsembleMeasurementTable}()},
\code{\link{createModel}()},
\code{\link{createMultiModelEnsemble}()},
\code{\link{createTSid}()},
\code{\link{estimateUncertaintyFromRange}()},
\code{\link{getVariableIndex}()},
\code{\link{mapAgeEnsembleToPaleoData}()},
\code{\link{pullTsVariable}()},
\code{\link{selectData}()}
}
\concept{LiPD manipulation}
