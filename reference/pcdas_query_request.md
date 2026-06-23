# Execute PCDaS query request

Execute PCDaS API query request.

## Usage

``` r
pcdas_query_request(
  body,
  pcdas_token = NULL,
  throttle_rate = 1,
  max_tries = 10
)
```

## Arguments

- body:

  Body of the request, as a list. Character JSON bodies are still
  accepted for backward compatibility.

- pcdas_token:

  character. PCDaS API token. If not provided, the function will look
  for it on renvirom.

- throttle_rate:

  Rate of requests per second allowed. Defaults to 1 request per second.

- max_tries:

  Max number of retries before fail. Defaults to 10.

## Value

A list
