% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_agence.R
\name{clean_agence}
\alias{clean_agence}
\title{Mise en forme d'un df aspe au format d'échange commun avec wama, fédés, etc.}
\usage{
clean_agence(df_brut)
}
\arguments{
\item{df_brut}{Dataframe agence}
}
\value{
Un dataframe au format souhaité.
}
\description{
Mise en forme d'un df aspe au format d'échange commun avec wama, fédés, etc.
}
\examples{
\dontrun{
agence_propre <- agence_brut \%>\%
clean_agence()
}
}
