\name{dr_check_sla}
\alias{dr_check_sla}
\title{Evaluate delivery SLA}
\description{Returns pending, missing, late, stale or met status and timestamps.}
\usage{dr_check_sla(sla, business_date, delivered_at = NULL, at = Sys.time())}
\arguments{\item{at}{See function description.}\item{business_date}{See function description.}\item{delivered_at}{See function description.}\item{sla}{See function description.}}
\value{A DataRaft definition, result or transition.}
