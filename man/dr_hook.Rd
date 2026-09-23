\name{dr_hook}
\alias{dr_hook}
\title{Attach lifecycle hook}
\description{Published and failed callbacks run after execution. A deprecated callback runs after the lake transition commits; callback failures cannot undo a committed transition or release.}
\usage{dr_hook(product, event, callback)}
\arguments{\item{callback}{Function receiving sanitized event metadata.}\item{event}{\code{published}, \code{failed} or \code{deprecated}.}\item{product}{DataRaft product.}}
\value{A DataRaft definition, result or transition.}
