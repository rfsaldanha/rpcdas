# Get a description of a data frame

Get a description of a data frame

## Usage

``` r
get_text_description(
  df,
  prompt,
  pcdas_token = NULL,
  throttle_rate = 1,
  max_tries = 10
)
```

## Arguments

- df:

  a data frame.

- prompt:

  character. A prompt asking how the data should be described.

- pcdas_token:

  character. PCDaS API token. If not provided, the function will look
  for it on renvirom.

- throttle_rate:

  Rate of requests per second allowed. Defaults to 1 request per second.

- max_tries:

  Max number of retries before fail. Defaults to 10.

## Value

a character string.

## Examples

``` r
if (FALSE) { # \dontrun{
prompt <- paste(
  "This data contains sepal and petal measurements.",
  "Write a small paragraph describing the species."
)
get_text_description(df = iris, prompt = prompt)
} # }
```
