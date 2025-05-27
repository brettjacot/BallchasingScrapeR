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
stats_from_group(group_id, api_key, name = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{group_id}{
A Ballchasing Group ID. Example: "ontario-campus-carball-winter-20-xxukviq8ww"
}
  \item{api_key}{
  A Ballchasing API key.
  }
  \item{name}{
  Whether names should be included in the dataset (default = FALSE)
  }
}
\details{
Returns a dataframe of the "main stats" for each player found within the group. These stats are: \cr
Name (optional) \cr
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
}
\note{
Note that I've changed a few digits from my own API key in the example code, as API keys are private and user-specific.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function(group_id, api_key, name = FALSE) {
  url <- paste0("https://ballchasing.com/api/groups/", group_id)

  # API request with query parameters
  response <- GET(
    url,
    add_headers(Authorization = api_key),
    query = list(count = 200)  # Request up to 200 replays
  )

  # Check for request success
  if (http_type(response) != "application/json") {
    stop(paste0("API request failed. Check the group ID and API key. Error code: ", status_code(response)))
  }

  # Parse JSON response
  content_data <- content(response, as = "text", encoding = "UTF-8")
  parsed_data <- fromJSON(content_data, flatten = TRUE)
  message("Group data collected!")
  datadf <- parsed_data[["players"]]
  core_stats_cols <- grep("game_average.core.", names(datadf), value = TRUE)
  name_stats <- subset(datadf, select = c(id, cumulative.games, cumulative.wins, cumulative.win_percentage, game_average.demo.taken, game_average.demo.inflicted))
  core_stats <- datadf[c(core_stats_cols)]
  if (name == TRUE) {
    player_names <- subset(datadf, select = name)
    core_stats <- cbind(player_names, core_stats)
    }
  overall_stats <- cbind(name_stats, core_stats)
  overall_stats$game_average.core.mvp = NULL
  return(overall_stats)
}


Result <- stats_from_group(group_id = "ontario-campus-carball-winter-20-xxukviq8ww", api_key = "P9LtEgF40vIUHUcrPrD4p9KSlYPvXPEoolUQHnqZ")
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
