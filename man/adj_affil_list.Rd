% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network_adjusters.R
\name{adj_affil_list}
\alias{adj_affil_list}
\title{Adjusts list of affiliations based on how we want to treat "one-off" connections
like participation in "Events" (protests, letters signing, etc.)}
\usage{
adj_affil_list(
  affils_by_date,
  lifelong = "none",
  event_length = 30,
  decay = "none",
  days_to_zero = 0
)
}
\description{
Adjusts list of affiliations based on how we want to treat "one-off" connections
like participation in "Events" (protests, letters signing, etc.)
}
