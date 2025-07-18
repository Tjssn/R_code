% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/excel_in.R
\name{excel_in}
\alias{excel_in}
\title{Import Configuration Data from Excel Files}
\usage{
excel_in(Object = Param1, Path = NULL, name = NULL)
}
\arguments{
\item{Object}{A \code{super_param} object containing configuration metadata (typically generated by \code{super_param()}).
Must include \code{output$result$excel_output$file_name} if relying on built-in naming.}

\item{Path}{Character string. Path to the directory containing the Excel file. Defaults to the current working directory.}

\item{name}{Character string. Name of the Excel file (without extension if \code{.xlsx} is desired). Defaults to:
\itemize{
\item \code{Object$output$result$excel_output$file_name} (if available),
\item \code{default_output.xlsx} (if no built-in name exists).
}}
}
\value{
The input \code{Object} with an updated \code{update} field, containing:
\itemize{
\item \code{level.data}: Data from the "Category.Variable.Level" sheet,
\item \code{summary}: Data from the "Total.Variable" sheet.
Returns the original \code{Object} unchanged if any errors occur (e.g., missing file/sheets).
}
}
\description{
This function reads specific sheets ("Category.Variable.Level" and "Total.Variable") from an Excel file,
parses the data, and updates the \code{update} field of the input \code{Object} (typically a \code{super_param} object).
It supports custom file paths and names, with automatic defaults if not specified.
}
\details{
The function performs the following steps:
\enumerate{
\item Determines the Excel file path (uses current working directory if \code{Path} is \code{NULL}).
\item Generates the Excel file name (defaults to \code{default_output.xlsx} or uses the name from \code{Object} if available).
\item Checks if the Excel file and required sheets exist.
\item Reads data from "Category.Variable.Level" and "Total.Variable" sheets using \code{openxlsx}.
\item Updates the \code{update} field of the input \code{Object} with the parsed data.
}
}
\note{
\itemize{
\item Requires the \code{openxlsx} package. If not installed, an error message will prompt installation.
\item The Excel file must contain two mandatory sheets:
\itemize{
\item "Category.Variable.Level": Defines variable categories and levels.
\item "Total.Variable": Summarizes total variables and their properties.
}
\item All non-ASCII characters in the Excel file should be avoided to ensure portability.
}
}
\seealso{
\itemize{
\item \code{\link{excel_out}}: Export results to an Excel file (companion function),
\item \code{\link{super_param}}: Generate the \code{super_param} object required as input.
}
}
\author{
TjssnStat Development Team \href{mailto:tongjibbb@163.com}{tongjibbb@163.com}
}
\keyword{Excel}
\keyword{configuration}
\keyword{data-preprocessing}
\keyword{import}
