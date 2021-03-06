% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infer_ast.R
\name{infer_ast}
\alias{infer_ast}
\title{Infer abstract syntax tree}
\usage{
infer_ast(x)
}
\arguments{
\item{x}{Expression to parse (\code{language} or \code{chr})}
}
\description{
Infer abstract syntax tree
}
\examples{
e <- quote(cos(2*x1 + x2))
t1 <- infer_ast(e)
t1

t2 <- infer_ast("cos(2*x1 + x2)")
t2

all.equal(t1, t2)

}
