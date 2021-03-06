% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monthly_pivot.R
\name{monthly_pivot}
\alias{monthly_pivot}
\title{Create monthly pivot summary}
\usage{
monthly_pivot(
  data,
  date,
  group_by,
  values_from,
  calc = sum,
  na.rm = TRUE,
  total = FALSE,
  total_name = NULL
)
}
\arguments{
\item{data}{data.frame}

\item{date}{date column name}

\item{group_by}{column(s) to group the data by}

\item{values_from}{values column}

\item{calc}{calculation function (default sum)}

\item{na.rm}{remove NAs in calculation (default is TRUE)}

\item{total}{add total column (default FALSE)}

\item{total_name}{name of total column}
}
\value{

}
\description{
Create monthly pivot summary
}
\examples{
monthly_pivot(dplyr::filter(hflights, Date >= '2011-10-01'), date = 'Date', group_by = c("Origin", "Dest"), values_from = "DepDelay", total = TRUE)
}
