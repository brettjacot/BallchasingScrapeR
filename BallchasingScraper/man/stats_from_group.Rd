\name{stats_from_group}
\alias{stats_from_group}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stats from Ballchasing Group
}
\description{
Takes replays from a group and returns a dataframe with stats.
}
\usage{
stats_from_group(group_id, api_key)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{group_id}{
A Ballchasing Group ID. Example: "ontario-campus-carball-winter-20-xxukviq8ww"
}
  \item{api_key}{
  A Ballchasing API key.
  }
}
\details{
Returns a dataframe of the "main stats" for each player found within the group. These stats are: \cr
Account ID \cr
Games \cr
Wins \cr
Win Percentage \cr
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
Shooting Percentage \cr}
\value{
Returns a dataframe of stats from the given replay group.
}
\references{
Ballchasing API Documentation- https://ballchasing.com/doc/api \cr
Thanks to CantFly for creating Ballchasing.
}
\author{
Brett Jacot
\note{
Note that I've changed a few digits from my own API key in the example code, as API keys are private and user-specific.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function(group_id, api_key)
{
stats_from_group(group_id = "ontario-campus-carball-winter-20-xxukviq8ww", api_key = "P9LtEgF40vIUHUcrPrD4p9KSlYPvXPEoolUQHnqZ")
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
