#' Get PCDaS token from renviron
#'
#' @return A character.
#'
#' @details
#' PCDaS token may be stored at renviron with `Sys.setenv("pcdas_token" = "YOUR_PCDAS_TOKEN")`. This function retrivies the token if stored at renviron.
#'
#' @export
get_pcdas_token_renviron <- function(){
  pcdas_token <- Sys.getenv("pcdas_token")
  if(pcdas_token == ""){
    stop("PCDaS token API not provided and not found on renviron. Please provide PCDaS API token.")
  }

  return(pcdas_token)
}
