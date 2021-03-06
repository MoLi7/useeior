% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WriteModel.R
\name{writeModeltoXLSX}
\alias{writeModeltoXLSX}
\title{Write selected model matrices and metadata as XLSX file to output folder}
\usage{
writeModeltoXLSX(model, outputfolder)
}
\arguments{
\item{model, }{any model object}

\item{outputfolder}{A directory to write model matrices and metadata as XLSX file out to}
}
\value{
None
}
\description{
Writes model matrices as XLSX file, including A, A_d, B, C, D, L, L_d, M, M_d, N, N_d, Rho (CPI ratio), Phi (Margin ratio),
demand vectors, and model metadata.
}
