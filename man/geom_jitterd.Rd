% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_jitterd.R
\name{geom_jitterd}
\alias{geom_jitterd}
\title{Jittered density points}
\usage{
geom_jitterd(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "jitter",
  ...,
  width = NULL,
  height = NULL,
  quad.points = 100,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}} or
\code{\link[ggplot2:aes_]{aes_()}}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, as a string.}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{width, height}{Amount of vertical and horizontal jitter. The jitter
is added in both positive and negative directions, so the total spread
is twice the value specified here.

If omitted, defaults to 40\% of the resolution of the data: this means the
jitter values will occupy 80\% of the implied bins. Categorical data
is aligned on the integers, so a width or height of 0.5 will spread the
data so it's not possible to see the distinction between the categories.}

\item{quad.points}{The number of "quadriture points" for the density function. 
Higher numbers yield a more detailed jittered density plot}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\value{
a geom of jittered data
}
\description{
The jitterd geom adds a small amount of random
variation to the location of each point, but that amount
of noise is proportional to the density. In some ways, it's
a mix between geom_jitter and geom_violin. It reduces
overplotting caused by discreteness in smaller datasets. This
function was adapted from geom_jitter within ggplot2
}
\seealso{
\link[ggplot2]{geom_point} for regular, unjittered points,
\link[ggplot2]{geom_boxplot} for another way of looking at the conditional
  distribution of a variable
\link[ggplot2]{geom_jitter} for standard jittering
}
