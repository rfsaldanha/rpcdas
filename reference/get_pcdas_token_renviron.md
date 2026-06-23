# Get PCDaS token from renviron

Get PCDaS token from renviron

## Usage

``` r
get_pcdas_token_renviron()
```

## Value

A character.

## Details

PCDaS token may be stored at renviron with
`Sys.setenv("pcdas_token" = "YOUR_PCDAS_TOKEN")`. This function
retrivies the token if stored at renviron.
