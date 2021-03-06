% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/applyCutoffs.R
\docType{methods}
\name{applyCutoffs}
\alias{applyCutoffs}
\alias{applyCutoffs,dbFrame-method}
\title{Single-cell debarcoding (2)}
\usage{
applyCutoffs(x, ...)

\S4method{applyCutoffs}{dbFrame}(x, mhl_cutoff = 30,
  sep_cutoffs = NULL)
}
\arguments{
\item{x}{a \code{\link{dbFrame}}.}

\item{...}{optional arguments.}

\item{mhl_cutoff}{mahalanobis distance threshold above which events should be unassigned. 
This argument will be ignored if the \code{mhl_cutoff} slot of the input 
\code{dbFrame} is specified.}

\item{sep_cutoffs}{non-negative numeric of length one or of same length as the number of rows 
in the \code{bc_key(x)}. Specifies the distance separation cutoffs between 
positive and negative barcode populations below which events should be 
unassigned. If \code{NULL} (default), \code{applyCutoffs} will try to 
access the \code{sep_cutoffs} slot of the input \code{dbFrame}.}
}
\value{
Will update the \code{bc_ids} and, if not already specified, 
\code{sep_cutoffs} & \code{mhl_cutoff} slots of \code{x}.
}
\description{
Applies separation and mahalanobies distance cutoffs.
}
\examples{
data(sample_ff, sample_key)
re <- assignPrelim(x = sample_ff, y = sample_key)

# use global separation cutoff
applyCutoffs(x = re, sep_cutoffs = 0.4)

# estimate population-specific cutoffs
re <- estCutoffs(x = re)
applyCutoffs(x = re)

}
\references{
Zunder, E.R. et al. (2015).
Palladium-based mass tag cell barcoding with a doublet-filtering scheme 
and single-cell deconvolution algorithm.
\emph{Nature Protocols} \bold{10}, 316-333.
}
\author{
Helena Lucia Crowell \email{helena.crowell@uzh.ch}
}
