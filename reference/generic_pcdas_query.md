# Executes a generic PCDaS query

Executes a generic PCDaS API query.

## Usage

``` r
generic_pcdas_query(pcdas_token = NULL, sql_query, fetch_size = 65000)
```

## Arguments

- pcdas_token:

  character. PCDaS API token. If not provided, the function will look
  for it on renvirom.

- sql_query:

  character. SQL.

- fetch_size:

  character. Pagination size for API call.

## Value

A tibble

## Details

The query is limited to retrieve 100 variables and 10,000 records by the
ElasticSearch cluster.

In the query, must be a usea a database listed with `list_pcdas_tables`.

## Examples

``` r
# Runs a query
generic_pcdas_query(sql_query = "SELECT N_AIH, DT_INTER, PROC_REA FROM \"datasus-sih\" LIMIT 100")
#> Error in get_pcdas_token_renviron(): PCDaS token API not provided and not found on renviron. Please provide PCDaS API token.
```
