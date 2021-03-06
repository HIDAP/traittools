% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trait_stats.R
\name{trait_summary}
\alias{trait_summary}
\title{Function to calculate summary statistics for fieldbook data from excel files}
\usage{
trait_summary(fieldbook, trait, genotype = NA, factor = NA, trait_dict,
  na.rm = TRUE)
}
\arguments{
\item{fieldbook}{A data frame of the fieldbook}

\item{trait}{The name or position of the measured variable that will be summarized.}

\item{genotype}{The column name of the genotype}

\item{factor}{The column name of the factor}

\item{trait_dict}{The data frame of the data dictionary for potato and sweetpotato}

\item{na.rm}{A boolean that indicates whether to ignore NA}
}
\value{
A data frame with the count, mean and standard desviation of the trait
}
\description{
This function is capable of divide the information in categorial or quantitative data based on a data dictionary for potato and sweepotato.
If it is categorical, returns the count and mode. And if it is quantitative, returns the count, media and standart desviation.
}
\author{
Omar Benites
}

