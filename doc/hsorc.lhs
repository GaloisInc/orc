\documentclass{article}

\usepackage{url}
\usepackage[left=1in,top=1in,right=1in,bottom=1.5in,nohead]{geometry}

%include lhs2TeX.fmt

\begin{document}

\author{Trevor Elliott}
\title{Orc in Haskell}
\date{}
\maketitle

\begin{abstract}
A library is developed in Haskell to emulate the functionality of the Orc
orchestration language.  Some examples are provided to motivate the use of
the library.
\end{abstract}

\section{Introduction}
This paper provides a very brief introduction to the Orc
language\cite{orc} in section \ref{orc-intro}, and two implementations
of a Haskell\cite{haskell} library to emulate its functionality.  The
first implementation in section \ref{attempt-1} takes a naive
approach, and produces a mostly functioning library, while the second
attempt, in section \ref{attempt-2}, addresses these problems with a
reworked version of the same library.  Conclusions are drawn about
usefulness of the library in section \ref{conclusions}.

\section{Orc}
\label{orc-intro}

\subsection{Orc Syntax}
Orc draws heavily on concepts from functional languages. It provides first class
functions, and composition via the |>>| operator.  If the result of a previous
expression needs to be named, the |>>| operator facilitates this by allowing the
programmer to place an identifier or pattern between the |>| characters
\cite{userguide}.  This syntax almost directly mirrors the two variations on the
bind operator in Haskell, either discarding or naming the result in a sequence
of operations.

\begin{verbatim}
A(b,c) >> C == A(b,c) >x> C(x)
\end{verbatim}

Central to Orc is the way values get bound to variables. Variables get
their values when an expression finishes, and ``publishes'' a value to
them. Evaluation of a function will start as soon as it is able to.
For example, given the function, \verb"def F(a,b) = a | b", F will
return a value as soon as either of the arguments have a value bound,
as there are two paths that use one of the arguments exclusively.

There are no explicit looping structures, as Orc allows recursion to handle
these cases.  Control is handled using guards, in the form of pattern matching
or conditional expressions.  Orc also provides some data structures, allowing
for more complex data to be modeled.  The following example is using two
notations for list, a static list defined with square brackets and commas, as
well as cons notation as a pattern guard.

\begin{verbatim}
def Head(l) = l >h:t> h
Head([1,2]) >> Head([])
\end{verbatim}

The example will produce no output, as the second call to Head will cause the
pattern match to fail, and cease processing.  There is one other way to cause
Orc to stop processing a branch; the |stop| value.  This term will consume
all input given to it, and publish nothing.  |Head([1,2]) >> stop| will
have the same result as the previous example.

\subsection{Concurrency}
Orc is a language that was developed with the purpose of making concurrency
easy. It has a few useful combinators that make performing tasks in parallel
and synchronization part of the language, rather than an afterthought.

The bar operator (\verb"|") allows for the two expressions that it joins
together to be executed in parallel.  In a simple case, the expression
\verb"1 | 2" will cause execution to branch, with any successive operations
being done twice -- one for the value \verb"1" and one for the value \verb"2".
This operator is extremely useful for performing operations that in Haskell
would be done with either a backtracking monad, or the list monad in simple
cases.  For example, finding all the combinations of numbers from \verb"1" to
\verb"10" that add up to \verb"10" is very easy to do using a list
comprehension, or the list monad:

\begin{verbatim}
[ (x,y) | x <- [1..10], y <- [1..10], x + y == 10 ]
-- or
do x <- [1..10]
   y <- [1..10]
   guard (x + y == 10)
   return (x,y)
\end{verbatim}

With a little scaffolding, this functionality can be recreated in Orc:

\begin{verbatim}
def Iterate(l) = l >h:t> (h | Iterate(t))
val ls = [1,2,3,4,5,6,7,8,9,10]
Iterate(ls) >x> Iterate(ls) >y> if(x+y = 10) >> (x,y)
\end{verbatim}

Another useful combinator that Orc brings to the table is the reversed version
of the sequencing operator, \verb"<<".  This operator behaves the same way
as its reverse counterpart in a syntactical sense, but has one key operational
difference: it takes the first successful result of the RHS, and discards the
rest, publishing that result to the term on the LHS. This can be extremely useful
when dealing with functions that could potentially produce infinite data, or
simply timing out long running computation.

For example, the function:

\begin{verbatim}
def Timeout(n,def,m) = v <v< ((Rtimer(n) >> def) | n)
\end{verbatim}

\noindent
makes it possible to stop long running jobs, and provide a default value back
in the event of timeout. It's worth noting that if the |def| value is the
\verb"stop" value discussed earlier, the long running job will continue to run,
even after the timeout has occurred.

\subsection{Putting It All Together}
Bringing all the concepts together from previous sections together, it's now
possible to examine an example from the Orc distribution\cite{orc-dist}.

\begin{verbatim}
def isPrime(n) = 
  def primeat(i) = 
  	val b = i * i <= n
      if(b) >> (n % i /= 0) && primeat(i+1)
    | if(~b) >> true
  primeat(2)

def Metronome(i) = i | Rtimer(500) >> Metronome(i+1)

-- Publish only prime numbers
Metronome(2) >n> if(isPrime(n)) >> n
\end{verbatim}

This example is using a user-defined function to generate a sequence of
increasing values starting at 2. Next, it guards the value generated (\verb"n")
as a prime number, and the final step merely returns the number as the result of
the computation. As the {\tt Metronome} function uses the bar operator, each
prime candidate will be checked in parallel, with the 500ms delay introduced by
the call to Rtimer.

\section{First Translation Attempt}
\label{attempt-1}

> import Control.Concurrent
> import Control.Concurrent.STM
> import Control.Monad
> import Data.Maybe
> import MonadLib hiding (collect)

Using the {\tt ChoiceT} monad transformer from the MonadLib \cite{monadlib}
library, a simple first attempt can be made:

> newtype OrcM a = OrcM { unOrcM :: ChoiceT IO a }
>   deriving (Functor,Monad,MonadPlus)

> instance BaseM OrcM IO where inBase = OrcM . inBase

> runOrcM :: OrcM a -> (a -> IO ()) -> IO ()
> runOrcM (OrcM o) f = run o
>   where
>     run m = do
>       x <- runChoiceT m
>       case x of
>         Nothing       -> return ()
>         Just (a,rest) -> f a >> run rest

This new {\tt OrcM} type allows us to use all the functionality of the
{\tt ChoiceT} transformer, from within the context of the IO monad.  Using this
new monad as a base, we can start defining many of the useful functions that Orc
provides, as well as a few new ones to help development from Haskell.

> ioM :: IO a -> OrcM a
> ioM  = inBase

> rtimerM :: Int -> OrcM ()
> rtimerM wait = ioM $ threadDelay $ wait * 1000

Using the {\tt rtimer} function just defined, it's now possible to re-create one
of the examples that ships with the Orc distribution.

> isPrimeM n = primeat 2
>   where
>     primeat i =
>       let b = i * i <= n
>        in (guard (b && n `mod` i /= 0) >> primeat (i+1)) `mplus` guard (not b)

> metronomeM n = return n `mplus` (rtimerM 500 >> metronomeM (n+1))

> primesM = do
>   n <- metronomeM 2
>   isPrimeM n
>   return n

\begin{verbatim}
*Main> runOrcM  primesM print
2
3
5
...
\end{verbatim}

Indeed, running this example produces the expected output -- a stream of prime
numbers.  However, if we were to define metronome with the order of the
arguments to {\tt mplus} flipped, a problem would appear; the recursive call
would be evaluated first due to {\tt ChoiceT}'s depth-first, left-biased choice
mechanism, and the function would never yield a value.

In order to solve this problem, what is really needed is the ability to evaluate
each arm of a conditional branch in parallel, yielding results in the order they
finish.  But let's ignore this problem for the time being, and explore the
embedding of some other examples.

The \verb"Timeout" function was a compelling example -- being able to timeout
a potentially expensive computation with a default value, taking the first
successful result. Let's give that a try in the current version:

> timeoutM :: Int -> a -> OrcM a -> OrcM a
> timeoutM wait def m = m `mplus` (rtimerM wait >> return def)

\begin{verbatim}
Orc> runOrcM (timeoutM 100 "succeed" (rtimerM 200 >> return "fail")) print
"fail"
"succeed"
\end{verbatim}

Attempting to write this function reveals a problem: there's currently no way
to communicate that we only care about the first result to the |runChoiceT|
function.  At this point we've reached an impasse; two of the most compelling
bits of functionality provided by Orc's concurrency primitives aren't
representable with the current architecture.

\section{Second Translation Attempt}
\label{attempt-2}

As the previous attempt was not successful, it's time to revisit the
implementation. To start off, let's integrate the internals of the |ChoiceT|
monad into the Orc monad, so that the constructors are exposed, and we can write
our own run function.

> data Orc a
>   = NoAnswer
>   | Answer a
>   | Cut (Orc a)
>   | Choice (Orc a) (Orc a)
>   | ChoiceEff (IO (Orc a))

> instance Monad Orc where
>   return = Answer
>   NoAnswer    >>= _ = NoAnswer
>   Answer x    >>= f = f x
>   Cut x       >>= f = Cut (f =<< x)
>   Choice x y  >>= f = Choice (f =<< x) (f =<< y)
>   ChoiceEff m >>= f = ChoiceEff (fmap (f =<<) m)

> instance MonadPlus Orc where
>   mzero = NoAnswer
>   NoAnswer `mplus` y        = y
>   x        `mplus` NoAnswer = x
>   x        `mplus` y        = Choice x y

This instantiation of the Orc monad adds a constructor, |Cut|. The purpose of
this constructor is to emulate the functionality of the \verb"<<" combinator in
Orc, allowing for the result that finishes first to be the only result that is
used.

As we're no longer using ChoiceT for the internals of the |Orc| monad, we'll
need to write a running function.  There's two defined here, so that either results
can be collected, or effects performed.  The orcStep function will be defined later.

> runOrc :: Orc a -> (a -> IO b) -> IO [b]
> runOrc o f = orcStep o >>= \x -> case x of
>   Nothing       -> return []
>   Just (a,rest) -> do
>     y  <- f a
>     ys <- runOrc rest f
>     return (y:ys)

> runOrc_ :: Orc a -> (a -> IO ()) -> IO ()
> runOrc_ o f = orcStep o >>= \x -> case x of
>   Nothing       -> return ()
>   Just (a,rest) -> f a >> runOrc_ rest f

And again, we'll define some convenience functions for working with the library.

> io :: IO a -> Orc a
> io m = ChoiceEff (Answer `fmap` m)

> rtimer wait = io $ threadDelay $ wait * 1000

As the problems that cropped up in the last implementation were to do with not
implementing a stepping function, the next section will address those by creating
a custom stepping function for the new Orc monad. There are two issues to deal with:
running choice branches in parallel, and picking off the first returned value. The
difference between these two cases is what motivates the |Cut| constructor;
if there is a |Cut| in front of a |Choice| constructor, only the first
result is returned, otherwise all results are returned in the order that they
finish.

Addressing the first problem, running branches in parallel, we can use the
concurrency functionality provided by the Haskell base library to write a new
function, |collect|.  The purpose of this function will be to run choice branches in
parallel using |forkIO|, collecting the results as they come in a
transactional channel.  The implementation that follows currently does not
attempt to handle exceptions in spawned threads.

> collect :: Orc a -> Orc a -> IO (Maybe (a, Orc a))
> collect x y = do
>   chan <- atomically newTChan
>   let run m = forkIO (orcStep m >>= atomically . writeTChan chan)
>   run x
>   run y
>   ma <- atomically (readTChan chan)
>   case ma of
>     Nothing     -> atomically (readTChan chan)
>     Just (a,as) ->
>       let b_eff = do
>             mb <- atomically (readTChan chan)
>             case mb of
>               Nothing     -> return NoAnswer
>               Just (b,bs) -> return (Answer b `mplus` bs)
>        in return (Just (a, as `mplus` ChoiceEff b_eff))

The second problem involves a race between two choice branches, corresponding to
a use of the |Cut| constructor wrapped around a |Choice| constructor. Both
choice branches will be run in separate threads, expecting to write their output
to a |TMVar|. The main thread will block, trying to read the |TMVar| until one
thread makes it's results available.  At this point, the remaining thread will
be killed, and the results returned.

> race :: Orc a -> Orc a -> IO (Maybe (a,Orc a))
> race x y = do
>   var <- atomically newEmptyTMVar
>   let run p m = forkIO (orcStep m >>= atomically . putTMVar var . (,) p)
>   pid_x <- run Nothing      x
>   pid_y <- run (Just pid_x) y
>   let kill = killThread . fromMaybe pid_y
>   (mb_pid,v) <- atomically (takeTMVar var)
>   case v of
>     Just (a,_) -> do
>       kill mb_pid
>       return (Just (a,NoAnswer))
>     Nothing -> do
>       (_,w) <- atomically (takeTMVar var)
>       case w of
>         Nothing    -> return Nothing
>         Just (a,_) -> return (Just (a,NoAnswer))

With the introduction of |collect| and |race|, it is now possible to introduce
the stepping function for the Orc monad:

> orcStep :: Orc a -> IO (Maybe (a, Orc a))
> orcStep NoAnswer           = return Nothing
> orcStep (Answer a)         = return (Just (a,NoAnswer))
> orcStep (ChoiceEff m)      = orcStep =<< m
> orcStep (Choice l r)       = l `collect` r
> orcStep (Cut (Choice x y)) = x `race` y
> orcStep (Cut o)            = do
>   mb <- orcStep o
>   return $ case mb of
>     Nothing       -> Nothing
>     Just (a,rest) -> Just (a, NoAnswer)

The effect of the |Cut| constructor is pushed all the way through, taking
advantage of parallelism when it's appropriate, and discarding extra results in
the general case. To round out the API, a way to use the |Cut| constructor
is required.

> cut :: Orc a -> Orc a
> cut  = Cut

With the addition of this new primitive, it's now possible to write the
|timeout| function that was misbehaving in the previous implementation:

> timeout :: Int -> a -> Orc a -> Orc a
> timeout wait def m = cut (m `mplus` (rtimer wait >> return def))

As expected, it now produces the value that finishes first, terminating any
computation still executing.

\begin{verbatim}
*Main> runOrc_ (timeout 100 "succeed" (rtimer 200 >> return "fail")) print
"succeed"
\end{verbatim}

Addressing the initial problem with the initial implementation, it should now
be possible to have an operation with multiple branches return the results in
the order that they come, instead of the order that they appear in the source.

> metronome :: Int -> Orc Int
> metronome n = (rtimer 500 >> metronome (n+1)) `mplus` return n

Again, running the example produces the expected result.

\begin{verbatim}
*Main> runOrc_ (metronome 2) print
2
3
4
...
\end{verbatim}

\section{Conclusions}
\label{conclusions}

Haskell has turned out to be a natural fit for the concurrency idioms expressed
by Orc. The library produced has many possible applications, including: processing
RSS and Atom feeds and aiding the development of IRC bots.  The features offered
by the |Orc| monad would be extremely useful in both cases, allowing for IRC
messages or feed items to be handled asynchronously.

\pagebreak
\begin{thebibliography}{10}

\bibitem{orc}
  \emph{Orc Language}.
  \url{http://orc.csres.utexas.edu/}

\bibitem{orc-dist}
  \emph{Orc Language}.
  \url{http://orc.csres.utexas.edu/download.shtml}

\bibitem{haskell}
  Peyton Jones, S., ed. 
  \emph{Haskell 98 Language and Libraries, The Revised Report.}
  \url{http://haskell.org/definition/haskell98-report.pdf}

\bibitem{monadlib}
  \emph{monadLib}
  \url{http://www.purely-functional.net/monadLib/}

\bibitem{userguide}
  Kitchin, David. (2008)
  \emph{A User's Guide To Orc.}
  \url{http://orc.csres.utexas.edu/userguide.pdf}

\end{thebibliography}

\end{document}
