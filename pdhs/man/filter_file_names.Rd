\name{filter_file_names}
\alias{filter_file_names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
filter_file_names(path, pattern = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
%%     ~~Describe \code{path} here~~
}
  \item{pattern}{
%%     ~~Describe \code{pattern} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (path, pattern = NULL, ...) 
{
    if (!is.null(pattern)) 
        return(path[grepl(pattern = pattern, x = path)])
    o <- vector(mode = "character", length = 0)
    for (p in path) {
        if (filter_file_name(p, ...)) 
            o <- c(o, p)
    }
    return(o)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
