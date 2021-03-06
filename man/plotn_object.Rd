% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotn.R
\name{plotn_object}
\alias{plotn_object}
\title{Make command and plotn object consist of plotn and overdraw to store a figure as a object}
\usage{
plotn_object(..., insert = NULL, delete = NULL)
}
\arguments{
\item{...}{plotn command or plotn object}

\item{insert}{position of insert layer. If set nunber i, second and later arguments are inserted in i th layer in a first argument}

\item{delete}{position of delete layer. If set nunber i, i th layers in a first argument are deleted. If both insert and delete are set number, deleting is done before inserting.}
}
\description{
Make command and plotn object consist of plotn and overdraw to store a figure as a object
}
\examples{
n1 <- plotn_object(plotn(1:50),
                   overdraw(abline(v = 30), abline(h = 20),
                            points(1:10 + 1, 10:1))
      )
n1

d <- data.frame(x = c(1:10, 11:20, 21:30, 31:40),
                group = rep(c("A","B","A", "B"), each = 10),
                treatment = rep(c("X","Y"), each = 20))
n2 <- plotn_object(boxplotn(x ~ group + treatment, data = d, xaxt = "n",
                            xlab = "", mar = c(3.8, 3.8, 1, 1)),
                   overdraw(category_axis(main = "treatment",
                                         sub = "group",data = d))
      )
n2

}
