\name{dr_hook}
\alias{dr_hook}
\title{Attach lifecycle hook}
\description{Callback runs after execution; failure cannot roll back a committed release.}
\usage{dr_hook(product, event, callback)}
\arguments{\item{callback}{See function description.}\item{event}{See function description.}\item{product}{See function description.}}
\value{A DataRaft definition, result or transition.}
