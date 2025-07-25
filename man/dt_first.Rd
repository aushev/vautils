% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datatable.R
\name{dt_first}
\alias{dt_first}
\title{Get First Row per Group from a data.table}
\usage{
dt_first(dtIn, by = key(dtIn), orderby = NULL)
}
\arguments{
\item{dtIn}{A \code{data.table} object.}

\item{by}{A character vector of column names to group by. Defaults to the key columns of \code{dtIn}, if any.}
}
\value{
A \code{data.table} containing the first row of each group defined by \code{by}.
}
\description{
Returns the first row of each group in a data.table, based on grouping columns.
}
\examples{
library(data.table)
dt1 <- data.table(grp = c("A", "A", "B", "B"), val = 1:4)
setkey(dt, grp)
dt_first(dt1)  # returns first row from each group A and B

# Without key
dt_first(dt, by = "grp")

}
