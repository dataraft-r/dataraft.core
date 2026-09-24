\name{dr_add_policy}
\alias{dr_add_policy}
\title{Attach a governance policy to a product}
\usage{dr_add_policy(product, policy)}
\arguments{
\item{product}{A DataRaft product definition.}
\item{policy}{A \code{dr_policy()} definition.}
}
\value{The updated product definition.}
\description{Attach a versioned governance policy to a product. The policy is evaluated at its configured lifecycle event.}
