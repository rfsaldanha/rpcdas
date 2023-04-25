#' Get new births data from SINASC
#'
#' Retrieves new births data from PCDaS API.
#'
#' This function uses raw data from the Sistema de Informações de Nascidos Vivos (SINASC) available at the PCDaS API. A documentation about this data can be found at \url{https://pcdas.icict.fiocruz.br/conjunto-de-dados/sistema-de-informacao-sobre-nascidos-vivos/}.
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{uf_ocor} for UF of occurrence. \code{regsaude_res} for regiao de saude of residence. \code{regsaude_ocor} for regiao de saúde of occurence. \code{mun_res} for municipality of residence. \code{mun_ocor} for municipality of ocurrence.
#' @param agg_time character. Time aggregation level. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.
#' @param ano numeric. Year of birth
#' @param pcdas_token character. PCDaS API token. If not provided, the function will look for it on renvirom.
#' @param sexo character. Sex of the new birth \code{Masculino} for males, \code{Feminino} for females and \code{Ignorado} for unknown.
#' @param more_filters character. Additional filters can be added by using this parameter, with a SQL query.
#'
#' @examples
#' # Some examples
#' get_sinasc(agg = "mun_res", ano = 2010)
#' get_sinasc(agg = "uf_ocor", ano = 2010)
#'
#' @importFrom rlang .data
#' @export
get_sinasc <- function(agg, agg_time = "year", ano, pcdas_token = NULL, sexo = NULL, more_filters = NULL, fetch_size = 65000){
  # Function argument check
  checkmate::assert_choice(x = agg, choices = c("uf_res", "uf_ocor", "mun_res", "mun_ocor", "regsaude_res", "regsaude_ocor"))
  checkmate::assert_choice(x = agg_time, choices = c("year", "month", "week"))
  checkmate::assert_vector(x = ano)
  checkmate::assert_string(x = pcdas_token, null.ok = TRUE)
  checkmate::assert_choice(x = sexo, choices = c("Masculino", "Feminino", "Ignorado"), null.ok = TRUE)
  checkmate::assert_string(x = more_filters, null.ok = TRUE)

  # Try to get PCDaS API token from renviron if not provided
  if(is.null(pcdas_token)){
    pcdas_token <- get_pcdas_token_renviron()
  }

  # Check if token have access to index
  if(!("datasus-sinasc" %in% list_pcdas_tables())){
    stop("Your token does not have access to 'datasus-sinasc' index. Please ask PCDaS to grant your access to this index.")
  }

  # Variable aggregation name
  if(agg == "uf_res"){
    agg <- "res_CODIGO_UF"
  } else if (agg == "uf_ocor"){
    agg <- "nasc_CODIGO_UF"
  } else if (agg == "mun_res"){
    agg <- "res_codigo_adotado"
  } else if (agg == "mun_ocor"){
    agg <- "nasc_codigo_adotado"
  } else if (agg == "regsaude_res"){
    agg <- "res_RSAUDCOD"
  } else if (agg == "regsaude_ocor"){
    agg <- "nasc_RSAUDCOD"
  }

  # SQL query basic partials
  sql_select <- glue::glue("SELECT {agg} AS agg, COUNT(1) AS freq")
  sql_from <- glue::glue("FROM \"datasus-sinasc\"")
  sql_where <- glue::glue("WHERE ano_nasc IN ({glue::glue_collapse(ano, sep = ', ')})")
  sql_group_by <- glue::glue("GROUP BY {agg}")

  # Time aggregation additions
  if(agg_time == "year"){
    sql_select <- glue::glue(sql_select, ", DATETIME_FORMAT(data_nasc, \'yyyy\') AS agg_time")
    sql_group_by <- glue::glue(sql_group_by, ", agg_time")
  } else if(agg_time == "month"){
    sql_select <- glue::glue(sql_select, ", DATETIME_FORMAT(data_nasc, \'yyyy-MM\') AS agg_time")
    sql_group_by <- glue::glue(sql_group_by, ", agg_time")
  } else if(agg_time == "week"){
    sql_select <- glue::glue(sql_select, ", DATETIME_FORMAT(data_nasc, \'yyyy-ww\') AS agg_time")
    sql_group_by <- glue::glue(sql_group_by, ", agg_time")
  }

  # Additions to where partial
  # Sexo
  if(!is.null(sexo)){
    sql_where <- glue::glue(sql_where, "AND def_sexo = '{sexo}'", .sep = " ")
  }

  # More filters
  if(!is.null(more_filters)){
    sql_where <- glue::glue(sql_where, "AND {more_filters}", .sep = " ")
  }

  # Create SQL query string
  sql_query <- glue::glue(sql_select, sql_from, sql_where, sql_group_by, .sep = " ")

  # Create list with token and SQL query
  request_body <- list(token = list(token = pcdas_token), sql = list(sql = list(query = sql_query, fetch_size = fetch_size)))

  # Request body as JSON
  request_body_json <- jsonlite::toJSON(request_body, auto_unbox = TRUE)

  # Execute PCDaS API request
  content <- pcdas_query_request(body = request_body_json)

  # Transform content to data.frame and tibble
  content_df <- convert_list_content_to_df(content) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      agg = as.numeric(.data$agg),
      agg_time = .data$agg_time,
      freq = as.numeric(.data$freq)
    ) %>%
    dplyr::select(.data$agg, .data$agg_time, .data$freq)

  return(content_df)
}
