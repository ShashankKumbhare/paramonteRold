% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ParaMonteSampler.R
\name{ParaMonteSampler}
\alias{ParaMonteSampler}
\title{Instantiate a \code{\link{R6}} \strong{\code{ParaMonteSampler}} class object for \strong{\link{paramonte}} package}
\value{
This function returns a \code{\link{R6}} \code{ParaMonteSampler} class object.
}
\description{
This is the \code{ParaMonteSampler} base class for the ParaMonte \cr
sampler routines. This class is NOT meant to be directly accessed \cr
or called by the user of the \strong{\link{paramonte}} package. However, its children, \cr
such as the \strong{\link{ParaDRAM}} sampler class will be directly accessible to the public.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{buildMode}}{optional string argument with the default value “release”. possible choices are:
\itemize{
\item “debug” \cr
to be used for identifying sources of bug and causes of code crash.
\item “release” \cr
to be used in all other normal scenarios for maximum runtime efficiency.
}}

\item{\code{mpiEnabled}}{optional logical (boolean) indicator which is \code{FALSE} by default. \cr
If it is set to \code{TRUE}, it will cause the ParaMonte simulation \cr
to run in parallel on the requested number of processors. \cr
See the class documentation guidelines in the above for \cr
information on how to run a simulation in parallel.}

\item{\code{inputFile}}{optional string input representing the path to \cr
an external input namelist of simulation specifications. \cr
USE THIS OPTIONAL ARGUMENT WITH CAUTION AND \cr
ONLY IF YOU KNOW WHAT YOU ARE DOING. \cr
\cr
\strong{WARNING} \cr
\cr
Specifying an input file will cause the sampler to ignore \cr
all other simulation specifications set by the user via \cr
sampler instance's \code{spec}-component attributes.}

\item{\code{spec}}{An R list containing all simulation specifications. \cr
All simulation attributes are by default set to appropriate \cr
values at runtime. To override the default simulation \cr
specifications, set the \code{spec} attributes to some \cr
desired values of your choice. \cr
\cr
If you need help on any of the simulation specifications, try \cr
the supplied \code{helpme()} function in this component. \cr
\cr
If you wish to reset some specifications to the default values, \cr
simply set them to \code{None}.}

\item{\code{reportEnabled}}{optional logical (boolean) indicator which is \code{TRUE} by default. \cr
If it is set to \code{TRUE}, it will cause extensive guidelines to be \cr
printed on the standard output as the simulation or post-processing \cr
continues with hints on the next possible steps that could be taken \cr
in the process. If you do not need such help and information set \cr
this variable to \code{FALSE} to silence all output messages.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{ParaMonteSampler$new()}}
\item \href{#method-readChain}{\code{ParaMonteSampler$readChain()}}
\item \href{#method-readSample}{\code{ParaMonteSampler$readSample()}}
\item \href{#method-readReport}{\code{ParaMonteSampler$readReport()}}
\item \href{#method-readRestart}{\code{ParaMonteSampler$readRestart()}}
\item \href{#method-readProgress}{\code{ParaMonteSampler$readProgress()}}
\item \href{#method-readMarkovChain}{\code{ParaMonteSampler$readMarkovChain()}}
\item \href{#method-clone}{\code{ParaMonteSampler$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Instantiates a \code{ParaMonteSampler} class object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParaMonteSampler$new(platform, website)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{platform}}{platform for \strong{\link{paramonte}} package.}

\item{\code{website}}{website for \strong{\link{paramonte}} package.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Returns a new \code{ParaMonteSampler} class object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-readChain"></a>}}
\if{latex}{\out{\hypertarget{method-readChain}{}}}
\subsection{Method \code{readChain()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParaMonteSampler$readChain(
  file,
  arg2,
  arg3 = TRUE,
  arg4 = FALSE,
  delimiter,
  parseContents,
  renabled
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-readSample"></a>}}
\if{latex}{\out{\hypertarget{method-readSample}{}}}
\subsection{Method \code{readSample()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParaMonteSampler$readSample(
  file,
  arg2,
  arg3 = TRUE,
  arg4 = FALSE,
  delimiter,
  parseContents,
  renabled
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-readReport"></a>}}
\if{latex}{\out{\hypertarget{method-readReport}{}}}
\subsection{Method \code{readReport()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParaMonteSampler$readReport(
  file,
  arg2,
  arg3 = TRUE,
  arg4 = FALSE,
  delimiter,
  parseContents,
  renabled
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-readRestart"></a>}}
\if{latex}{\out{\hypertarget{method-readRestart}{}}}
\subsection{Method \code{readRestart()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParaMonteSampler$readRestart(
  file,
  arg2,
  arg3 = TRUE,
  arg4 = FALSE,
  delimiter,
  parseContents,
  renabled
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-readProgress"></a>}}
\if{latex}{\out{\hypertarget{method-readProgress}{}}}
\subsection{Method \code{readProgress()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParaMonteSampler$readProgress(
  file,
  arg2,
  arg3 = TRUE,
  arg4 = FALSE,
  delimiter,
  parseContents,
  renabled
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-readMarkovChain"></a>}}
\if{latex}{\out{\hypertarget{method-readMarkovChain}{}}}
\subsection{Method \code{readMarkovChain()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParaMonteSampler$readMarkovChain(
  file,
  arg2,
  arg3 = TRUE,
  arg4 = FALSE,
  delimiter,
  parseContents,
  renabled
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParaMonteSampler$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
