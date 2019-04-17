\name{ZEST}
\alias{ZEST}
\alias{ZEST.start}
\alias{ZEST.step}
\alias{ZEST.stop}
\alias{ZEST.final}
\alias{QUEST}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ZEST
}
\description{
An implementation of the Bayesian test procedures of King-Smith et al. and Watson and Pelli. 
Note that we use the term \code{pdf} throughout as in the original paper, even though
they are discrete probability functions in this implementation.
}
\usage{
ZEST(domain=0:40, prior=rep(1/length(domain),length(domain)), 
     likelihood=sapply(domain,function(tt) {0.03+(1-0.03-0.03)*(1-pnorm(domain,tt,1))}),
     stopType="S", stopValue=1.5,
     minStimulus=head(domain,1), 
     maxStimulus=tail(domain,1),
     maxSeenLimit=2, minNotSeenLimit=2,
     maxPresentations=100,
     minInterStimInterval=NA,
     maxInterStimInterval=NA,
     verbose=0, 
     makeStim, 
     stimChoice="mean",
     ...) 

ZEST.start(domain=0:40, prior=rep(1/length(domain),length(domain)), 
     likelihood=sapply(domain,function(tt) {0.03+(1-0.03-0.03)*(1-pnorm(domain,tt,1))}),
     stopType="S", stopValue=1.5,
     minStimulus=head(domain,1), 
     maxStimulus=tail(domain,1),
     maxSeenLimit=2, minNotSeenLimit=2,
     maxPresentations=100,
     makeStim, 
     stimChoice="mean",
     ...) 
ZEST.step(state, nextStim=NULL)
ZEST.stop(state)
ZEST.final(state)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{domain}{
Vector of values over which pdf is kept.
}
  \item{prior}{
Starting probability distribution over domain. Same length as \code{domain}.
}
\item{likelihood}{Matrix where \code{likelihood[s,t]} is likelihood of seeing \code{s} given \code{t} 
                  is the true threshold. That is, Pr(s|t) where \code{s} and \code{t} are indexes into \code{domain}.
}
\item{stopType}{ \code{N}, for number of presentations; \code{S}, for standard deviation of the pdf; and
\code{H}, for the entropy  of the pdf.
}
\item{stopValue}{Value for number of presentations (\code{stopType=N}), standard deviation (\code{stopType=S)} or
Entropy (\code{stopType=H}).
}
\item{minStimulus}{ The smallest stimuli that will be presented. Could be different from \code{domain[1]}.}
\item{maxStimulus}{ The largest stimuli that will be presented. Could be different from \code{tail(domain,1)}.}
\item{minNotSeenLimit}{ Will terminate if \code{minStimulus} value is not seen this many times.
}
\item{maxSeenLimit}{    Will terminate if \code{maxStimulus} value is seen this many times.
}
\item{maxPresentations}{ Maximum number of presentations regarless of \code{stopType}.
}
\item{minInterStimInterval}{If both \code{minInterStimInterval} and \code{maxInterStimInterval} are not
\code{NA}, then between each stimuli there is a random wait period drawn uniformly between
\code{minInterStimInterval} and \code{maxInterStimInterval}.
}
\item{maxInterStimInterval}{See \code{minInterStimInterval}.
}
  \item{verbose}{
\code{verbose=0} does nothing, \code{verbose=1} stores pdfs for returning, and \code{verbose=2} stores pdfs and also prints each
presentaion.
}
  \item{makeStim}{
A function that takes a dB value and numPresentations 
and returns an OPI datatype ready for passing to opiPresent. See examples.
}
  \item{stimChoice}{
    A true ZEST procedure uses the \code{"mean"} of the current pdf as the stimulus, but \code{"median"} and
\code{"mode"} (as used in a QUEST procedure) are provided for your enjoyment.
}
  \item{\dots}{
Extra parameters to pass to the opiPresent function
}

\item{state}{
    Current state of the ZEST returned by \code{ZEST.start} and \code{ZEST.step}.
}

\item{nextStim}{
    A valid object for \code{opiPresent} to use as its \code{nextStim}.
}
}
\details{
    This is an implementation of King-Smith et al.'s ZEST procedure 
    and Watson and Pelli's QUEST procedure.
    All presentaions are rounded to an element of the supplied domain.

    Note this function will repeatedly call \code{opiPresent} for a stimulus 
    until \code{opiPresent} returns \code{NULL} (ie no error occured).

    The \code{checkFixationOK} function is called (if present in stim made from \code{makeStim}) 
    after each presentation, and if it returns FALSE, the
    pdf for that location is not changed (ie the presentation is ignored), but the stim, 
    number of presentations etc is recorded in the state.

    If more than one ZEST is to be interleaved (for example, testing multiple locations), then the 
    \code{ZEST.start}, \code{ZEST.step}, \code{ZEST.stop} and \code{ZEST.final} calls can maintain the state of
    the ZEST after each presentation, and should be used.
    If only a single ZEST is required, then the simpler \code{ZEST} can be used, which is a wrapper for the 
    four functions that maintain state.
    See examples below.
}
\value{

\subsection{Single location}{

\code{ZEST} returns a list containing
\itemize{
\item{npres:}{ Total number of presentations used.}
\item{respSeq:}{Response sequence stored as a matrix: row 1 is dB values of stimuli, row 2 is 1/0 for seen/not-seen, row
3 is fixated 1/0 (always 1 if \code{checkFixationOK} not present in stim objects returned from \code{makeStim}).}
\item{pdfs:}{ If \code{verbose} is bigger than 0, then this is a list of the pdfs used for each presentation,
otherwise NULL.}
\item{final}{ The mean/median/mode of the final pdf, depending on \code{stimChoice}, which is the determined threshold.}
\item{opiResp}{A list of responses received from each successful call to \code{opiPresent} within \code{ZEST}.}
}
}

\subsection{Multilple locations}{

\code{ZEST.start} returns a list that can be passed to 
\code{ZEST.step},
\code{ZEST.stop} and
\code{ZEST.final}. It represents the state of a ZEST at a single location at a point in time and 
contains the following.
\itemize{
    \item{name:}{ \code{ZEST}}
    \item{}{ A copy of all of the parameters supplied to ZEST.start: 
    \code{domain}
    \code{likelihood},
    \code{stopType},
    \code{stopValue},
    \code{minStimulus},
    \code{maxStimulus},
    \code{maxSeenLimit},
    \code{minNotSeenLimit},
    \code{maxPresentations},
    \code{makeStim},
    \code{stimChoice},
    \code{currSeenLimit},
    \code{currNotSeenLimit}, and
    \code{opiParams}.}
    \item{pdf:}{ Current pdf: vector of probabilities the same length as \code{domain}.}
    \item{numPresentations:}{ The number of times \code{ZEST.step} has been called on this state.}
    \item{stimuli:}{ A vector containing the stimuli used at each call of \code{ZEST.step}.}
    \item{responses:}{ A vector containing the responses received at each call of \code{ZEST.step}.}
    \item{responseTimes:}{ A vector containing the response times received at each call of \code{ZEST.step}.}
    \item{fixated:}{ A vector containing TRUE/FALSE if fixation was OK according to \code{checkFixationOK} for each call
            of \code{ZEST.step} (defaults to TRUE if \code{checkFixationOK} not present).}
    \item{opiResp}{A list of responses received from each call to \code{opiPresent} within \code{ZEST.step}.}
}

\code{ZEST.step} returns a list containing
\itemize{
\item{state:}{ The new state after presenting a stimuli and getting a response.}
\item{resp:}{ The return from the \code{opiPresent} call that was made.}
}


\code{ZEST.stop} returns \code{TRUE} if the ZEST has reached its stopping criteria, and \code{FALSE} otherwise.

\code{ZEST.final} returns an estimate of threshold based on state. 
If \code{state$stimChoice} is \code{mean} then the mean is returned.
If \code{state$stimChoice} is \code{mode} then the mode is returned.
If \code{state$stimChoice} is \code{median} then the median is returned.
}
}
\references{

P.E. King-Smith, S.S. Grigsny, A.J. Vingrys, S.C. Benes, and A.
Supowit.  "Efficient and Unbiased Modifications of the QUEST Threshold
Method: Theory, Simulations, Experimental Evaluation and Practical
Implementation", Vision Research 34(7) 1994. Pages 885-912.

A.B. Watson and D.G. Pelli. 
"QUEST: A Bayesian adaptive psychophysical method", 
Perception and Psychophysics 33 1983. 
Pages 113-l20.

Please cite:
A. Turpin, P.H. Artes and A.M. McKendrick
"The Open Perimetry Interface: An enabling tool for clinical visual psychophysics",
Journal of Vision 12(11) 2012.

http://perimetry.org/OPI
}
\author{
Andrew Turpin <aturpin@unimelb.edu.au>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dbTocd}}, \code{\link{opiPresent}}
}
\examples{
chooseOpi("SimHenson")
if (!is.null(opiInitialize(type="C", cap=6)))
    stop("opiInitialize failed")

    ##############################################
    # This section is for single location ZESTs
    ##############################################

    # Stimulus is Size III white-on-white as in the HFA
makeStim <- function(db, n) { 
    s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
             duration=200, responseWindow=1500, checkFixationOK=NULL)
    class(s) <- "opiStaticStimulus"

    return(s)
}

repp <- function(...) sapply(1:50, function(i) ZEST(makeStim=makeStim, ...))
a <- repp(stopType="H", stopValue=  3, verbose=0, tt=30, fpr=0.03)
b <- repp(stopType="S", stopValue=1.5, verbose=0, tt=30, fpr=0.03)
c <- repp(stopType="S", stopValue=2.0, verbose=0, tt=30, fpr=0.03)
d <- repp(stopType="N", stopValue= 50, verbose=0, tt=30, fpr=0.03)
e <- repp(prior=dnorm(0:40,m=0,s=5), tt=30, fpr=0.03)
f <- repp(prior=dnorm(0:40,m=10,s=5), tt=30, fpr=0.03)
g <- repp(prior=dnorm(0:40,m=20,s=5), tt=30, fpr=0.03)
h <- repp(prior=dnorm(0:40,m=30,s=5), tt=30, fpr=0.03)

layout(matrix(1:2,1,2))
boxplot(lapply(list(a,b,c,d,e,f,g,h), function(x) unlist(x["final",])))
boxplot(lapply(list(a,b,c,d,e,f,g,h), function(x) unlist(x["npres",])))

    ##############################################
    # This section is for multiple ZESTs
    ##############################################
makeStimHelper <- function(db,n, x, y) {  # returns a function of (db,n)
    ff <- function(db, n) db+n

    body(ff) <- substitute(
        {s <- list(x=x, y=y, level=dbTocd(db), size=0.43, color="white",
                  duration=200, responseWindow=1500, checkFixationOK=NULL)
         class(s) <- "opiStaticStimulus"
         return(s)
        }
        , list(x=x,y=y))
    return(ff)
}

    # List of (x, y, true threshold) triples
locations <- list(c(9,9,30), c(-9,-9,32), c(9,-9,31), c(-9,9,33))

    # Setup starting states for each location
states <- lapply(locations, function(loc) {
    ZEST.start(
        domain=-5:45,
        minStimulus=0,
        maxStimulus=40,
        makeStim=makeStimHelper(db,n,loc[1],loc[2]),
        stopType="S", stopValue= 1.5, tt=loc[3], fpr=0.03, fnr=0.01)
})

    # Loop through until all states are "stop"
while(!all(st <- unlist(lapply(states, ZEST.stop)))) {
    i <- which(!st)                         # choose a random, 
    i <- i[runif(1, min=1, max=length(i))]  # unstopped state
    r <- ZEST.step(states[[i]])             # step it
    states[[i]] <- r$state                  # update the states
}

finals <- lapply(states, ZEST.final)    # get final estimates of threshold
for(i in 1:length(locations)) {
    #cat(sprintf("Location (%+2d,%+2d) ",locations[[i]][1], locations[[i]][2]))
    #cat(sprintf("has threshold %4.2f\n", finals[[i]]))
}

if (!is.null(opiClose()))
    warning("opiClose() failed")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
