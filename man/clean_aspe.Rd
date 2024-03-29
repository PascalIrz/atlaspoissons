% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_aspe.R
\name{clean_aspe}
\alias{clean_aspe}
\title{Mise en forme d'un df aspe au format d'échange commun avec wama, fédés, etc.}
\usage{
clean_aspe(passerelle)
}
\arguments{
\item{passerelle}{Dataframe passerelle.}
}
\value{
Un dataframe au format souhaité avec les coordonnées reprojetées.
}
\description{
Mise en forme d'un df aspe au format d'échange commun avec wama, fédés, etc.
}
\examples{
\dontrun{
aspe <- passerelle \%>\%
clean_aspe()
}

}
