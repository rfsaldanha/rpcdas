# Get hospitalization data from SIH

Retrieves hospitalization data from PCDaS API.

## Usage

``` r
get_sih(
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

  numeric. Year of birth

- pcdas_token:

  character. PCDaS API token. If not provided, the function will look
  for it on renvirom.

- sexo:

  character. Sex of the new birth `Masculino` for males, `Feminino` for
  females and `Ignorado` for unknown.

- idade_a:

  numeric. Minimum age of the patient, in years.

- idade_b:

  numeric. Maximum age of the patient, in years.

- cid_like:

  character. CID-10 code of principal diagnosis. Used with a `LIKE`
  operator.

- cid_in:

  character vector. CID-10 codes of principal diagnosis. Used with a
  `IN` operator.

- more_filters:

  character. Additional filters can be added by using this parameter,
  with a SQL query.

- fetch_size:

  character. Pagination size for API call.

## Details

This function uses raw data from the Sistema de Informações de
Hospitalares do SUS (SIH) available at the PCDaS API. A documentation
about this data can be found at
<https://pcdas.icict.fiocruz.br/conjunto-de-dados/sistema-de-informacoes-hospitalares-do-sus-sihsus/>.

If `idade_a` is supplied, the query will filter records with age less or
equal. If `idade_b` is supplied, the query will filter records with age
more or equal. If both are supplied, the query will filter records in
the interval, closed on both sides.

## Examples

``` r
# Some examples
get_sih(agg = "mun_res", ano = 2010)
#> Error in get_pcdas_token_renviron(): PCDaS token API not provided and not found on renviron. Please provide PCDaS API token.
get_sih(agg = "uf_ocor", ano = 2010)
#> Error in get_pcdas_token_renviron(): PCDaS token API not provided and not found on renviron. Please provide PCDaS API token.
```
