\name{dr_sla}
\alias{dr_sla}
\title{Define delivery SLA}
\description{Specifies a local deadline, timezone and optional freshness limit.}
\usage{dr_sla(freshness = NULL, refresh = c("daily", "weekly"), available_by, timezone = "UTC")}
\arguments{\item{available_by}{See function description.}\item{freshness}{See function description.}\item{refresh}{See function description.}\item{timezone}{See function description.}}
\value{A DataRaft definition, result or transition.}
