\name{dr_policy}
\alias{dr_policy}
\title{Define organization policy}
\description{An organization policy requires specified product or contract metadata at a lifecycle event.}
\usage{dr_policy(id, when = "publish", require, version = "1", classification = NULL, action = c("block", "warn"))}
\arguments{\item{action}{See function description.}\item{classification}{See function description.}\item{id}{See function description.}\item{require}{See function description.}\item{version}{See function description.}\item{when}{See function description.}}
\value{A DataRaft definition, result or transition.}
