\name{filtered_unzip}
\alias{filtered_unzip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
filtered_unzip(path, extension = NULL, output_dir = getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
%%     ~~Describe \code{path} here~~
}
  \item{extension}{
%%     ~~Describe \code{extension} here~~
}
  \item{output_dir}{
%%     ~~Describe \code{output_dir} here~~
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
function (path, extension = NULL, output_dir = getwd()) 
{
    if (is.null(path)) 
        stop("Must provide a source zip file or files.")
    if (length(path) != 0 && length(path) > 1) {
        return(sapply(path, filtered_unzip, extension = extension, 
            output_dir = output_dir))
    }
    file_name <- basename(path)
    contained_file_names <- unzip(zipfile = path, list = TRUE)$Name
    is_format <- has_extension(contained_file_names, extension)
    matching_files <- contained_file_names[is_format]
    if (any(is_format)) {
        unzip(zipfile = path, files = matching_files, overwrite = TRUE, 
            exdir = output_dir)
    }
    return(matching_files)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
