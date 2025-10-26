\name{master}
\alias{master}
\docType{data}
\title{
Example Master Table with Exposure and Outcome
}
\description{
A simulated dataset containing patient identifiers, exposure status, and outcome status for 1000 patients. This dataset is used for demonstrating the HDPS package functionality.
}
\usage{data(master)}
\format{
A data.table with the following columns:
\describe{
  \item{pid}{Patient identifier (character)}
  \item{exposure}{Exposure status (0/1, integer)}
  \item{outcome}{Outcome status (0/1, integer)}
}
}
\details{
The dataset contains 1,000 patients with approximately 30% exposure rate and 15% outcome rate. This master table is used to merge with covariate data for HDPS prioritization analysis.
}
\source{
Simulated data for demonstration purposes.
}
\examples{
data(master)
head(master)
str(master)
table(master$exposure)
table(master$outcome)
}
\keyword{datasets}
