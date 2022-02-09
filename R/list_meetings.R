#' This function returns a tibble with all the zoom meetings in a account
#'
#' @param user_id the email or user id of your email
#'
#' @return a tibble
#' @export
#'
#' @examples
list_meetings = function(user_id) {
  meetings = httr::GET(
    paste0('https://api.zoom.us/v2/users/', user_id, '/meetings'),
    httr::add_headers(
      authorization = paste0('Bearer', Sys.getenv('ZOOM_API'))
    )
  )
  chamadas_existentes = meetings %>%
    httr::content() %>%
    purrr::pluck('meetings') %>%
    purrr::map(unlist, recursive = T) %>%
    purrr::map_dfr(tibble::enframe, .id = 'ordem') %>%
    tidyr::pivot_wider(
      names_from = name,
      values_from = value,
      names_repair = 'unique'
    ) %>%
    janitor::clean_names()

  return(chamadas_existentes)
}
