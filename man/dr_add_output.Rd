\name{dr_add_output}
\alias{dr_add_output}
\title{Attach output port}
\description{The first output is the primary target. Later outputs publish the same checked delivery in order through \code{dr_write_target()}. Failed later outputs leave earlier commits in place; inspect \code{result$port_outputs} and use \code{dr_retry_ports()} with the original in-memory result to resume only unpublished outputs. Cache reuse is not supported with multiple outputs.}
\usage{dr_add_output(product, port)}
\arguments{\item{port}{See function description.}\item{product}{See function description.}}
\value{A DataRaft definition, result or transition.}
