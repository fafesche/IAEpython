% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/localPythonUsingReticulate.R
\name{IAE.fn.env}
\alias{IAE.fn.env}
\title{Fix env from ~ expansion}
\usage{
IAE.fn.env(choice = "global")
}
\arguments{
\item{choice}{either "local" to set up in AppData (windows) of user, or "global" to set up at C:}
}
\value{
Either a string corresponding to a directory or NULL when ~ cannot be extended for a user.
}
\description{
Fix env from ~ expansion
}
