% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export.R
\name{get_github}
\alias{get_github}
\title{Collect various data from the GitHub API}
\usage{
get_github(what, value, proxy_url = NULL, token = NULL)
}
\arguments{
\item{what}{Character string defining the api endpoint. Currently one of
\code{c("contributors", "members", "keys")}.}

\item{value}{Character string specifying what to collect from the given
endpoint. For "contributors" this should be the name of the repository, for
"members" value should be the team slug and for "keys" this should be a
github user name.}

\item{proxy_url}{Character string providing an internet proxy server url.
When set to NULL (default) no proxy server is assumed.}

\item{token}{Character string providing av valid token that will be used if
the api call requires authentication. Listing of team members do require a
token with the appropriate credentials. When NULL (default) no token will
be used.}
}
\value{
Character vector with results from the GitHub api request
}
\description{
Collect various data from the GitHub API
}
