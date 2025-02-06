\name{get_replays_by_criteria}
\alias{get_replays_by_criteria}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Replay IDs by Criteria
}
\description{
Returns a list of replay IDs based off the criteria that the user inputs.
}
\usage{
get_replays_by_criteria(start_date, end_date, match_type, whouploader, player_ids, api_key, callspersecond)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start_date}{
Earliest date you want replays pulled from (required). RFC3339 format - example - "2020-10-01T00:00:00-05:00"
}
  \item{end_date}{
  Latest date you want replays pulled from (required). Default - current system time
  }
  \item{match_type}{
  Playlist that you want to pull replays from (optional). Playlist options can be found at https://ballchasing.com/doc/api#replays-replays-get.
  }
  \item{whouploader}{
  Uploader of the replays you want to pull (optional). Default - NULL
  }
  \item{player_ids}{
  A list of player IDs, in the format "platform:ID" (required) \cr
  Example - c("steam:76561198058699429")
  }
  \item{api_key}{
  Ballchasing API key (required).
  }
  \item{callspersecond}{
  Calls to be made to Ballchasing API per second (required). Default - 2 \cr
  Gold patrons and below - 2 \cr
  Diamond patrons - 4 \cr
  Champion patrons - 8 \cr
  GC patrons - 16 \cr
\cr
  Note that default users only get 500 calls per hour, which this program may hit with a large list of replays if users are not careful.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a LIST of replay IDs that can be passed to the RunReplays function (?RunReplays for more info)
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
get_replays_by_criteria(start_date = "2020-10-01T00:00:00-05:00", match_type = "ranked_doubles", player_ids = c("steam:76561198058699429"), api_key = "P9LtEgF40vIUHUcrPrD4p9KSlYPvXPEoolUQHnqZ")


{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
