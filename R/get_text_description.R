#' Get a description of a data frame
#'
#' @param df a data frame.
#' @param prompt character. A prompt asking how the data should be described.
#' @param pcdas_token character. PCDaS API token. If not provided, the function will look for it on renvirom.
#' @param throttle_rate Rate of requests per second allowed. Defaults to 1 request per second.
#' @param max_tries Max number of retries before fail. Defaults to 10.
#'
#' @return a character string.
#' @export
#'
#' @examples
#' get_audio_description(df = iris, prompt = "This json data contains measurements of sepal length and width and petal length and width. Write a small paragraph describing the species. Do not mention the file format.")
#'
get_audio_description <- function(df, prompt, pcdas_token = NULL, throttle_rate = 1, max_tries = 10){
  # Function argument check
  checkmate::assert_data_frame(df)
  checkmate::assert_string(prompt)

  # Try to get PCDaS API token from renviron if not provided
  if(is.null(pcdas_token)){
    pcdas_token <- get_pcdas_token_renviron()
  }

  # Try to get PCDaS API token from renviron if not provided
  if(is.null(pcdas_token)){
    pcdas_token <- get_pcdas_token_renviron()
  }

  # Create body
  request_body <- list(
    token = list(token = pcdas_token),
    data = list(
      data = jsonlite::toJSON(x = iris),
      context = prompt
    )
  )

  # Request body as JSON
  request_body_json <- jsonlite::toJSON(request_body, auto_unbox = TRUE)

  # Create request
  req <- httr2::request(base_url = pcdas_url) %>%
    httr2::req_url_path_append("text_description") %>%
    httr2::req_body_raw(request_body_json) %>%
    httr2::req_throttle(throttle_rate, realm = pcdas_url) %>%
    httr2::req_retry(max_tries = max_tries)

  # Perform request
  resp <- httr2::req_perform(req = req)

  # Get content
  content <- httr2::resp_body_json(resp)

  # Return description
  return(content$text_description)

}
