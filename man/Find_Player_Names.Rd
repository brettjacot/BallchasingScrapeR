\name{Find_Player_Names}
\alias{Find_Player_Names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find Most Common Player Names
}
\description{
Find the most common player name for each ID in a RunReplays dataframe and paste that name wherever that ID is found, then return the modified dataframe.
}
\usage{
Find_Player_Names(results)
}
%- maybe also 'usage' for other objects documented here.
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
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
