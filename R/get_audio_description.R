#' Get audio from text string
#'
#' @param text character. A text string.
#' @param dest_file character. Path to destination audio file. The output is a mp3 binary.
#' @param pcdas_token character. PCDaS API token. If not provided, the function will look for it on renvirom.
#' @param throttle_rate Rate of requests per second allowed. Defaults to 1 request per second.
#' @param max_tries Max number of retries before fail. Defaults to 10.
#'
#' @return an audio file.
#' @export
#'
#' @examples
#' file <- tempfile(fileext = ".mp3")
#' get_audio_description(text = "Ciência de Dados, para nós da PCDaS, é um campo de estudo que se destaca pela capacidade de auxiliar a descoberta de informação útil a partir de grandes ou complexas bases de dados.", dest_file = file)
get_audio_description <- function(text, dest_file, pcdas_token = NULL, throttle_rate = 1, max_tries = 10){
  # Function argument check
  checkmate::assert_string(text)

  # Try to get PCDaS API token from renviron if not provided
  if(is.null(pcdas_token)){
    pcdas_token <- get_pcdas_token_renviron()
  }

  # Create body
  request_body <- list(
    token = list(token = pcdas_token),
    data = list(text = text)
  )

  # Request body as JSON
  request_body_json <- jsonlite::toJSON(request_body, auto_unbox = TRUE)

  # Create request
  req <- httr2::request(base_url = pcdas_url) %>%
    httr2::req_url_path_append("audio_description") %>%
    httr2::req_body_raw(request_body_json) %>%
    httr2::req_throttle(throttle_rate, realm = pcdas_url) %>%
    httr2::req_retry(max_tries = max_tries)

  # Perform request
  resp <- httr2::req_perform(req = req)

  # Get content
  content <- httr2::resp_body_json(resp)

  # Convert from base64 to binary
  res_base64_decode <- RCurl::base64Decode(txt = content$audio_description, mode = "raw")

  # Save binary to file
  writeBin(object = res_base64_decode, con = dest_file)
}
