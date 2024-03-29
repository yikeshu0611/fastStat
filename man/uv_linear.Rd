% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uv_linear.R
\name{uv_linear}
\alias{uv_linear}
\title{Looping for Univariable Logistic Regression}
\usage{
uv_linear(data, y, variable, adjust, round = 3, p_threshold = 0.05,
  order_by.beta = TRUE)
}
\arguments{
\item{data}{data}

\item{y}{y}

\item{variable}{variable names for univariable logistic regression. If missing, it will be column names of data except y and adjust}

\item{adjust}{adjust variable names for univariable logistic regression}

\item{round}{digital round, 3 is defaulted}

\item{p_threshold}{threshold for p value to show star. 0.05 is defaulted}

\item{order_by.beta}{logical. TRUE means order in or by decreasing. FLASE is defaulted}
}
\value{
univariable logistic regression results
}
\description{
Looping for Univariable Logistic Regression
}
\examples{
uv_linear(data = mtcars,y = 'vs')
}
