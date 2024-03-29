% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_sim.R
\name{get_sim}
\alias{get_sim}
\title{Get mortality data from SIM}
\usage{
get_sim(
  agg,
  agg_time = "year",
  ano,
  pcdas_token = NULL,
  sexo = NULL,
  idade_a = NULL,
  idade_b = NULL,
  cid_like = NULL,
  cid_in = NULL,
  more_filters = NULL,
  fetch_size = 65000
)
}
\arguments{
\item{agg}{character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saude of occurence. \code{regsaude_449_res} for regiao de saude (449 units) of residence. \code{regsaude_449_ocor} for regiao de saude (449 units) of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.}

\item{agg_time}{character. Time aggregation level. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.}

\item{ano}{vector. Year of death.}

\item{pcdas_token}{character. PCDaS API token. If not provided, the function will look for it on renvirom.}

\item{sexo}{character. Sex of the deceased. \code{Masculino} for males, \code{Feminino} for females and \code{Ignorado} for unknown.}

\item{idade_a}{numeric. Minimum age of the deceased, in years.}

\item{idade_b}{numeric. Maximum age of the deceased, in years.}

\item{cid_like}{character. CID-10 code of basic cause of death. Used with a \code{LIKE} operator.}

\item{cid_in}{character vector. CID-10 codes of basic cause of death. Used with a \code{IN} operator.}

\item{more_filters}{character. Additional filters can be added by using this parameter, with a SQL query.}

\item{fetch_size}{character. Pagination size for API call.}
}
\description{
Retrieves mortality data from PCDaS API.
}
\details{
This function uses raw data from the Sistema de Informações de Mortalidade (SIM) available at the PCDaS API. A documentation about this data can be found at \url{https://pcdas.icict.fiocruz.br/conjunto-de-dados/sistema-de-informacoes-de-mortalidade-sim/}.

If \code{idade_a} is supplied, the query will filter records with age less or equal. If \code{idade_b} is supplied, the query will filter records with age more or equal. If both are supplied, the query will filter records in the interval, closed on both sides.

The \code{cid_like} value is used in the query with a \code{LIKE} operator.
}
\examples{
# Some examples
get_sim(agg = "mun_res", ano = 2010)
get_sim(agg = "uf_ocor", ano = 2010)
get_sim(agg = "uf_res", ano = 2010, idade_a = 10, idade_b = 30)
get_sim(agg = "uf_res", ano = 2010, cid_like = "I")
get_sim(agg = "uf_res", ano = 2010, cid_in = c("I219", "B342", "R98"))
get_sim(agg = "uf_res", ano = 2010, cid_in = cid_seq("I01", "I10"))

}
