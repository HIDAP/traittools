% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trait_tools.R
\name{get_trait_name}
\alias{get_trait_name}
\title{Get the full name of the trait into crop trait dictionary}
\usage{
get_trait_name(trait, trait_dict)
}
\arguments{
\item{trait}{trait abreviation}

\item{trait_dict}{trait dictionary}
}
\value{
The full name of the trait
}
\description{
Get the full name of the trait into crop trait dictionary
}
\examples{
\dontrun{
# The data
data(potato_yield)
#str(potato_yield)
# NTP: Number of tuber planted
get_trait_name("NTP", potato_yield)
}
}
\author{
omar benites
}

