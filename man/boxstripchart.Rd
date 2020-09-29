\name{boxstripchart}
\alias{boxstripchart}
\title{boxplotstripchart}
\usage{
boxstripchart(y, x, ...)
}
\description{
plot boxplot and scatter points from stripchart
}
\examples{
y <- rnorm(100)
x <- sample(c(0,1), 100, replace=TRUE)
boxstripchart(y, x)
}
