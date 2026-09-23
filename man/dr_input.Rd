\name{dr_input}
\alias{dr_input}
\alias{dr_output}
\title{Define product ports}
\description{Ports describe interfaces over the existing source and target adapters. The first output is primary; later outputs use adapters implementing \code{dr_write_target()}. Publication across destinations is not atomic.}
\usage{dr_input(id, source, contract = NULL, version = "1", access = "internal", sla = NULL)
dr_output(id, target, contract = NULL, version = "1", access = "internal", sla = NULL)}
\arguments{\item{access}{See function description.}\item{contract}{See function description.}\item{id}{See function description.}\item{sla}{See function description.}\item{source}{See function description.}\item{target}{See function description.}\item{version}{See function description.}}
\value{A DataRaft definition, result or transition.}
