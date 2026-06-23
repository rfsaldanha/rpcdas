# Get new births data from SINASC

Retrieves new births data from PCDaS API.

## Usage

``` r
get_sinasc(
  agg,
  agg_time = "year",
  ano,
  pcdas_token = NULL,
  sexo = NULL,
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

- more_filters:

  character. Additional filters can be added by using this parameter,
  with a SQL query.

- fetch_size:

  character. Pagination size for API call.

## Details

This function uses raw data from the Sistema de Informações de Nascidos
Vivos (SINASC) available at the PCDaS API. A documentation about this
data can be found at
<https://pcdas.icict.fiocruz.br/conjunto-de-dados/sistema-de-informacao-sobre-nascidos-vivos/>.

## Examples

``` r
# Some examples
get_sinasc(agg = "mun_res", ano = 2010)
#> Error in get_pcdas_token_renviron(): PCDaS token API not provided and not found on renviron. Please provide PCDaS API token.
get_sinasc(agg = "uf_ocor", ano = 2010)
#> Error in get_pcdas_token_renviron(): PCDaS token API not provided and not found on renviron. Please provide PCDaS API token.
```
