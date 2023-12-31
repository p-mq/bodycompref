% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reference_values.R
\name{.Get_lambda}
\alias{.Get_lambda}
\title{Get the lambda used to build a model}
\usage{
.Get_lambda(metric, sex, level = NA)
}
\arguments{
\item{metric}{character, body composition metric}

\item{sex}{character, "Female" or "Male"}

\item{level}{character, used vertebral level}
}
\value{
lambda (numeric, range [-2, 2], increments of 0.1)
}
\description{
For each reference LMSP model, get the lambdas used before fitting
}
\author{
J Peter Marquardt
}
