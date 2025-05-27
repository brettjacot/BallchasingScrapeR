\name{get_replay_data}
\alias{get_replay_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Replay Data
}
\description{
Processes a singular replay and returns the main stats for all players.
}
\usage{
get_replay_data(api_key, replay_id, callspersecond = 2, AllData = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
  Ballchasing API key (required).
  }
  \item{replay_id}{
  A Ballchasing replay ID (required).
}
  \item{callspersecond}{
  Calls to be made to Ballchasing API per second (required). Default - 2 \cr
  Gold patrons and below - 2 \cr
  Diamond patrons - 4 \cr
  Champion patrons - 8 \cr
  GC patrons - 16 \cr
\cr
  Note that free users only get 500 calls per hour, which this program may hit with a large list of replays if users are not careful.
  }
  \item{alldata}{
  Whether all stats produced from Ballchasing should be saved to the list (default = FALSE)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a dataframe of the "main stats" for each player. These stats are: \cr
Name \cr
Account ID (id.id) \cr
Demos Inflicted \cr
Demos Taken \cr
Shots \cr
Shots Against \cr
Goals \cr
Goals Against \cr
Saves \cr
Assists \cr
Score \cr
MVP \cr
Shooting Percentage \cr
Team \cr
Win/Loss \cr
Match GUID \cr
Match Date/Time \cr
Ballchasing Link

}
\references{
Ballchasing API Documentation- https://ballchasing.com/doc/api \cr
Thanks to CantFly for creating Ballchasing.
}
\author{
Brett Jacot
}
\note{
Note that I've changed a few digits from my own API key in the example code, as API keys are private and user-specific.
}

\examples{
get_replay_data(api_key = "P9LtEgF40vIUHUcrPrD4p9KSlYPvXPEoolUQHnqZ", replay_id = "8a39eb7a-2334-4b3b-b083-b5c18ad98140", callspersecond = 2)
}
