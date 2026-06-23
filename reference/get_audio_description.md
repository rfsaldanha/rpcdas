# Get audio from text string

Get audio from text string

## Usage

``` r
get_audio_description(
  text,
  dest_file,
  pcdas_token = NULL,
  throttle_rate = 1,
  max_tries = 10
)
```

## Arguments

- text:

  character. A text string.

- dest_file:

  character. Path to destination audio file. The output is a mp3 binary.

- pcdas_token:

  character. PCDaS API token. If not provided, the function will look
  for it on renvirom.

- throttle_rate:

  Rate of requests per second allowed. Defaults to 1 request per second.

- max_tries:

  Max number of retries before fail. Defaults to 10.

## Value

an audio file.

## Examples

``` r
if (FALSE) { # \dontrun{
file <- tempfile(fileext = ".mp3")
text <- paste(
  "Ciência de Dados, para nós da PCDaS, é um campo de estudo",
  "que se destaca pela capacidade de auxiliar a descoberta de informação útil."
)
get_audio_description(text = text, dest_file = file)
} # }
```
