% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotNetwork.R
\name{plotNetwork}
\alias{plotNetwork}
\title{Plot variance-covariance networks}
\usage{
plotNetwork(
  correlations,
  phylogenetic.signal = NULL,
  phyloSignal.name = NULL,
  correlation.type,
  gr_vars = NULL,
  order_vars = NULL,
  edgeLab = F,
  layout = "spring",
  onlySignificant = T,
  notShowCors = NULL,
  threshold = 0,
  labelSize = 0.8,
  nodeLab = NULL
)
}
\arguments{
\item{correlations}{(data frame) correlations output of correlationsTraits$correlations.rslts}

\item{phylogenetic.signal}{(data frame) phylogenetic signal as reported by phylogeneticSignalTraits$phylogenetic.signal.rslts}

\item{phyloSignal.name}{(character) variable name of the phylogenetic signal as reported in phylogenetic.signal}

\item{correlation.type}{(character) variable name of the correlation as reported iun correlations}

\item{gr_vars}{(data frame) character vector as long as the number of variables indicating the variable name in the first column and the group of the variable in the second.}

\item{order_vars}{(character) order of the variables to plot}

\item{edgeLab}{(logical) if TRUE, edge labels show correlation coefficients}

\item{layout}{"spring" or "circular"}

\item{onlySignificant}{(logical) if TRUE, only significant correlations are plotted}

\item{notShowCors}{(character) vector of variables present in correlations data but exclued from the plot}

\item{threshold}{(numeric) correlations to be shown (e.g., those with a absolute value higher than 0.1)}

\item{labelSize}{(numeric) size of the node labels}

\item{nodeLab}{(data frame) character vector as long as the number of variables indicating the variable name in the first column and the new name, which will be shown in the plot.}
}
\value{

}
\description{
Plot variance-covariance networks
}
