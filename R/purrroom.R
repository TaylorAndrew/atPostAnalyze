#' purrroom
#'
#' purrroom easily integrates purrr::map modeling into broom. Providing the user with a single tidy dataframe for all models.
#'
#' @param mapData a purrr call that ends with a mapped model call
#' @param broomCall a broom call (e.g. glance(), tidy()) call wrapped in a function with a dot argument
#'
#' @return Tidy data.frame containing all split model summaries.
#' @export
#'
#' @examples
#' mapDat <-
#' mtcars %>%
#'   split(.$cyl) %>%
#'   map(~ lm(mpg ~ wt, data = .))
#' ## As a function call:
#' broomCall <- function(.) {tidy(., conf.int=T)}
#' purrroom(mapDat, broomCall)
#' ## Using piping:
#' mtcars %>%
#'   split(.$cyl) %>%
#'   map(~ lm(mpg ~ wt, data = .)) %>%
#'   purrroom()

purrroom <- function(mapData, broomCall = function(.) {tidy(.)}) {

tidyDat <-
mapDat %>%
  map(~ broomCall(.))

outDat <-
tidyDat %>%
  plyr::rbind.fill() %>%
  data.frame(Split = rep(names(mapDat), each = nrow(tidyDat[[1]])), .)
return(outDat)
}
