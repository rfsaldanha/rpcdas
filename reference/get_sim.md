# Get mortality data from SIM

Retrieves mortality data from PCDaS API.

## Usage

``` r
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
```

## Arguments

- agg:

  character. Spatial aggregation level. `uf_res` for UF of residence.
  `uf_ocor` for UF of occurrence. `regsaude_res` for regiao de saude of
  residence. `regsaude_ocor` for regiao de saude of occurence.
  `regsaude_449_res` for regiao de saude (449 units) of residence.
  `regsaude_449_ocor` for regiao de saude (449 units) of occurence.
  `mun_res` for municipality of residence. `mun_ocor` for municipality
  of ocurrence.

- agg_time:

  character. Time aggregation level. `year` for yearly data. `month` for
  monthly data. `week` for weekly data. Defaults to `year`.

- ano:

  vector. Year of death.

- pcdas_token:

  character. PCDaS API token. If not provided, the function will look
  for it on renvirom.

- sexo:

  character. Sex of the deceased. `Masculino` for males, `Feminino` for
  females and `Ignorado` for unknown.

- idade_a:

  numeric. Minimum age of the deceased, in years.

- idade_b:

  numeric. Maximum age of the deceased, in years.

- cid_like:

  character. CID-10 code of basic cause of death. Used with a `LIKE`
  operator.

- cid_in:

  character vector. CID-10 codes of basic cause of death. Used with a
  `IN` operator.

- more_filters:

  character. Additional filters can be added by using this parameter,
  with a SQL query.

- fetch_size:

  character. Pagination size for API call.

## Details

This function uses raw data from the Sistema de Informações de
Mortalidade (SIM) available at the PCDaS API. A documentation about this
data can be found at
<https://pcdas.icict.fiocruz.br/conjunto-de-dados/sistema-de-informacoes-de-mortalidade-sim/>.

If `idade_a` is supplied, the query will filter records with age less or
equal. If `idade_b` is supplied, the query will filter records with age
more or equal. If both are supplied, the query will filter records in
the interval, closed on both sides.

The `cid_like` value is used in the query with a `LIKE` operator.

## Examples

``` r
# Some examples
get_sim(agg = "mun_res", ano = 2010)
#> Error in get_pcdas_token_renviron(): PCDaS token API not provided and not found on renviron. Please provide PCDaS API token.
get_sim(agg = "uf_ocor", ano = 2010)
#> Error in get_pcdas_token_renviron(): PCDaS token API not provided and not found on renviron. Please provide PCDaS API token.
get_sim(agg = "uf_res", ano = 2010, idade_a = 10, idade_b = 30)
#> Error in get_pcdas_token_renviron(): PCDaS token API not provided and not found on renviron. Please provide PCDaS API token.
get_sim(agg = "uf_res", ano = 2010, cid_like = "I")
#> Error in get_pcdas_token_renviron(): PCDaS token API not provided and not found on renviron. Please provide PCDaS API token.
get_sim(agg = "uf_res", ano = 2010, cid_in = c("I219", "B342", "R98"))
#> Error in get_pcdas_token_renviron(): PCDaS token API not provided and not found on renviron. Please provide PCDaS API token.
get_sim(agg = "uf_res", ano = 2010, cid_in = cid_seq("I01", "I10"))
#> Error in get_pcdas_token_renviron(): PCDaS token API not provided and not found on renviron. Please provide PCDaS API token.
```
