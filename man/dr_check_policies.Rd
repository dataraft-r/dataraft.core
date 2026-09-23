\name{dr_check_policies}
\alias{dr_check_policies}
\title{Evaluate organization policies}
\description{Returns decisions with policy versions, missing metadata and evaluation timestamps.}
\usage{dr_check_policies(product, policies = getOption("dataraft.policies", list()), event = "publish")}
\arguments{\item{event}{See function description.}\item{policies}{See function description.}\item{product}{See function description.}}
\value{A DataRaft definition, result or transition.}
