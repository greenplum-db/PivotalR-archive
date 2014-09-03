\name{text.dt.madlib}
\alias{text.dt.madlib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
text.dt.madlib(x, splits = TRUE, label, FUN = text, all = FALSE, pretty = NULL, digits = getOption("digits") - 3L, use.n = FALSE, fancy = FALSE, fwidth = 0.8, fheight = 0.8, bg = par("bg"), minlength = 1L, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{splits}{
%%     ~~Describe \code{splits} here~~
}
  \item{label}{
%%     ~~Describe \code{label} here~~
}
  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
}
  \item{all}{
%%     ~~Describe \code{all} here~~
}
  \item{pretty}{
%%     ~~Describe \code{pretty} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{use.n}{
%%     ~~Describe \code{use.n} here~~
}
  \item{fancy}{
%%     ~~Describe \code{fancy} here~~
}
  \item{fwidth}{
%%     ~~Describe \code{fwidth} here~~
}
  \item{fheight}{
%%     ~~Describe \code{fheight} here~~
}
  \item{bg}{
%%     ~~Describe \code{bg} here~~
}
  \item{minlength}{
%%     ~~Describe \code{minlength} here~~
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
function (x, splits = TRUE, label, FUN = text, all = FALSE, pretty = NULL, 
    digits = getOption("digits") - 3L, use.n = FALSE, fancy = FALSE, 
    fwidth = 0.8, fheight = 0.8, bg = par("bg"), minlength = 1L, 
    ...) 
{
    if (nrow(x$frame) <= 1L) 
        stop("fit is not a tree, just a root")
    frame <- x$frame
    if (!missing(label)) 
        warning("argument 'label' is no longer used")
    col <- names(frame)
    ylevels <- attr(x, "ylevels")
    if (!is.null(ylevels <- attr(x, "ylevels"))) 
        col <- c(col, ylevels)
    cxy <- par("cxy")
    if (!is.null(srt <- list(...)$srt) && srt == 90) 
        cxy <- rev(cxy)
    xy <- rpart:::rpartco(x)
    node <- as.numeric(row.names(frame))
    is.left <- (node\%\%2L == 0L)
    node.left <- node[is.left]
    parent <- match(node.left/2L, node)
    if (splits) {
        left.child <- match(2L * node, node)
        right.child <- match(node * 2L + 1L, node)
        rows <- if (!missing(pretty) && missing(minlength)) 
            labels(x, pretty = pretty)
        else labels(x, minlength = minlength)
        if (fancy) {
            xytmp <- rpart:::rpart.branch(x = xy$x, y = xy$y, 
                node = node)
            leftptx <- (xytmp$x[2L, ] + xytmp$x[1L, ])/2
            leftpty <- (xytmp$y[2L, ] + xytmp$y[1L, ])/2
            rightptx <- (xytmp$x[3L, ] + xytmp$x[4L, ])/2
            rightpty <- (xytmp$y[3L, ] + xytmp$y[4L, ])/2
            FUN(leftptx, leftpty + 0.52 * cxy[2L], rows[left.child[!is.na(left.child)]], 
                ...)
            FUN(rightptx, rightpty - 0.52 * cxy[2L], rows[right.child[!is.na(right.child)]], 
                ...)
        }
        else FUN(xy$x, xy$y + 0.5 * cxy[2L], rows[left.child], 
            ...)
    }
    leaves <- if (all) 
        rep(TRUE, nrow(frame))
    else frame$var == "<leaf>"
    stat <- x$functions$text(yval = if (is.null(frame$yval2)) 
        frame$yval[leaves]
    else frame$yval2[leaves, ], dev = frame$dev[leaves], wt = frame$wt[leaves], 
        ylevel = ylevels, digits = digits, n = frame$n[leaves], 
        use.n = use.n)
    if (fancy) {
        if (col2rgb(bg, alpha = TRUE)[4L, 1L] < 255) 
            bg <- "white"
        oval <- function(middlex, middley, a, b) {
            theta <- seq(0, 2 * pi, pi/30)
            newx <- middlex + a * cos(theta)
            newy <- middley + b * sin(theta)
            polygon(newx, newy, border = TRUE, col = bg)
        }
        rectangle <- function(middlex, middley, a, b) {
            newx <- middlex + c(a, a, -a, -a)
            newy <- middley + c(b, -b, -b, b)
            polygon(newx, newy, border = TRUE, col = bg)
        }
        maxlen <- max(string.bounding.box(stat)$columns) + 1L
        maxht <- max(string.bounding.box(stat)$rows) + 1L
        a.length <- if (fwidth < 1) 
            fwidth * cxy[1L] * maxlen
        else fwidth * cxy[1L]
        b.length <- if (fheight < 1) 
            fheight * cxy[2L] * maxht
        else fheight * cxy[2L]
        for (i in parent) oval(xy$x[i], xy$y[i], sqrt(2) * a.length/2, 
            sqrt(2) * b.length/2)
        child <- match(node[frame$var == "<leaf>"], node)
        for (i in child) rectangle(xy$x[i], xy$y[i], a.length/2, 
            b.length/2)
    }
    if (fancy) 
        FUN(xy$x[leaves], xy$y[leaves] + 0.5 * cxy[2L], stat, 
            ...)
    else FUN(xy$x[leaves], xy$y[leaves] - 0.5 * cxy[2L], stat, 
        adj = 0.5, ...)
    invisible()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
