\name{dx}
\alias{dx}
\docType{data}
\title{
Example Diagnosis Data
}
\description{
A simulated dataset containing diagnosis codes for 1000 patients. This dataset is used for demonstrating the HDPS package functionality.
}
\usage{data(dx)}
\format{
A data.table with the following columns:
\describe{
  \item{pid}{Patient identifier (character)}
  \item{icd9code}{ICD-9 diagnosis code (character)}
}
}
\details{
The dataset contains 3,176 diagnosis records for 1,000 patients. The ICD-9 codes include common conditions such as diabetes, hypertension, coronary artery disease, and other medical conditions. The data is structured in long format suitable for HDPS analysis.
}
\source{
Simulated data for demonstration purposes.
}
\examples{
data(dx)
head(dx)
str(dx)
}
\keyword{datasets}
