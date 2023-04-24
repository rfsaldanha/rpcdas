#' Execute PCDaS query request
#'
#' Execute PCDaS API query request.
#'
#' @param body Body of the request, in JSON format.
#' @param pcdas_token character. PCDaS API token. If not provided, the function will look for it on renvirom.
#' @param throttle_rate Rate of requests per second allowed. Defaults to 30/60 (thirty requests per minute).
#' @param max_tries Max number of retries before fail. Defaults to 3.
#'
#' @return A list
#'
pcdas_query_request <- function(body, pcdas_token = NULL, throttle_rate = 30/60, max_tries = 3){
  # Function argument check
  checkmate::assert_string(x = body)
  checkmate::assert_numeric(x = throttle_rate, lower = 0)
  checkmate::assert_numeric(x = max_tries, lower = 0)

  # Try to get PCDaS API token from renviron if not provided
  if(is.null(pcdas_token)){
    pcdas_token <- get_pcdas_token_renviron()
  }

  # Paginated request
  more <- TRUE
  content <- list()
  while(more == TRUE){
    # Create request
    req <- httr2::request(base_url = pcdas_url) %>%
      httr2::req_url_path_append("sql_query") %>%
      httr2::req_body_raw(body) %>%
      httr2::req_throttle(throttle_rate, realm = pcdas_url) %>%
      httr2::req_retry(max_tries = max_tries)

    # Perform request
    resp <- httr2::req_perform(req = req)

    # Get content
    tmp_content <- httr2::resp_body_json(resp)
    if(length(tmp_content$rows) > 0){
      content <- append(content, list(tmp_content))
    }

    # Update more
    if(!is.null(content[[length(content)]]$cursor) & length(tmp_content$rows) > 0){
      more <- TRUE
      body <- jsonlite::toJSON(
        list(
          token = list(token = pcdas_token),
          sql = list(sql = list(cursor = content[[length(content)]]$cursor))
        ), auto_unbox = TRUE
      )
    } else {
      more <- FALSE
    }
  }

  return(content)
}









