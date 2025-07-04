% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ensembleComposite.R
\name{concatEnsembleTimeseries}
\alias{concatEnsembleTimeseries}
\title{Concatenate two independent ensemble timeseries}
\usage{
concatEnsembleTimeseries(age.ens.1, paleo.data.1, age.ens.2, paleo.data.2)
}
\arguments{
\item{age.ens.1}{Age ensemble list for the first dataset}

\item{paleo.data.1}{Paleo data list for the first dataset (ensembles optional)}

\item{age.ens.2}{Age ensemble list for the second dataset}

\item{paleo.data.2}{Paleo data  list for the second dataset (ensembles optional)}
}
\value{
a matrix with one instance of concatenate ensemble data
}
\description{
This "shuffles" together two *independent* ensemble datasets, making no assumptions about stratigraphic order, but assuming that each dataset is representing the same phenomenon.
}
\seealso{
Other utility: 
\code{\link{askUser}()},
\code{\link{convertAD2BP}()},
\code{\link{convertBP2AD}()},
\code{\link{createChronMeasInputDf}()},
\code{\link{gaussianize}()},
\code{\link{generateEnsembleFromUncertainty}()},
\code{\link{getLastVarString}()},
\code{\link{getOs}()},
\code{\link{heuristicUnits}()},
\code{\link{loadRemote}()},
\code{\link{pullInstance}()},
\code{\link{simulateAutoCorrelatedUncertainty}()},
\code{\link{stringifyVariables}()},
\code{\link{surrogateDataFun}()}
}
\concept{utility}
