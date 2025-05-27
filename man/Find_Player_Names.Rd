\name{Find_Player_Names}
\alias{Find_Player_Names}
\title{
Find Most Common Player Names
}
\description{
Find the most common player name for each ID in a RunReplays dataframe and paste that name wherever that ID is found, then return the modified dataframe.
}
\usage{
Find_Player_Names(results)
}
\arguments{
  \item{results}{
The results of using the RunReplays function.
}
}
\details{
Dependency of Ranked function, mainly for data cleaning.
}
\value{
Returns a dataframe with names set to the most common occurrence for each player in the dataset.
}
\references{
Ballchasing API Documentation- https://ballchasing.com/doc/api \cr
Thanks to CantFly for creating Ballchasing.
}
\author{
Brett Jacot
}
