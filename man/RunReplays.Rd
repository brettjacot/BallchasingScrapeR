\name{RunReplays}
\alias{RunReplays}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run Replays
}
\description{
Run a list of replay IDs returned by the get_replays_by_criteria function.
}
\usage{
RunReplays(replay_id_data, api_key, callspersecond)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replay_id_data}{
A list of replay IDs created from the get_replays_by_criteria function (required).
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
  Note that free users only get 500 calls per hour, which this program may hit with a large list of replays if users are not careful.
  }
}
\details{
This generates a lot of calls - be careful with long lists of replays! \cr
Depends on the get_replay_data() function. \cr
If the function is going slower than expected, Ballchasing's API may be having issues.
}
\value{
Returns a dataframe of player stats from all replays.
}
\references{
Ballchasing API Documentation- https://ballchasing.com/doc/api \cr
Thanks to CantFly for creating Ballchasing.
}
\author{
Brett Jacot
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## The function is currently defined as
function (replay_id_data, api_key = apikey)
{
    results <- get_replay_data(api_key, replay_id_data[[1]])
    TotalReplayCount = length(replay_id_data)
    ReplayCount = 1
    while (ReplayCount < TotalReplayCount) {
        message("Processing replay ", ReplayCount, " out of ",
            TotalReplayCount)
        ReplayCount <- ReplayCount + 1
        replay_results <- get_replay_data(api_key, replay_id_data[[ReplayCount]])
        results <- rbind(results, replay_results)
    }
    return(results)
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
