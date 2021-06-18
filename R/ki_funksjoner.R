#' Provide quality indicator
#'
#' Provides a tibble
#'
#' @param df dataframe
#' @return res tibble
#' @export
#' @name ki_funksjoner
#' @aliases ki_liggetid ki_30dager ki_kompl_alv

# funksjon for å regne ut kvalitetsindikatoren
# definert for liggetid (andel pasienter med 3 dager eller færre)
ki_liggetid <- function(df) {

  teljar <- sum(df$LiggeDogn <= 3)
  nemnar <- nrow(df)

  ind <- teljar / nemnar

  res <- tibble::tibble(teljar, nemnar, ind = ind)
  return(res)
}

# funksjon for å regne ut andel pasienter
#  som får innleggelse innen 30 dager etter operasjonen
#
#' @export
ki_30dager <- function(df) {

  teljar <- sum(df$`6U_Behandling30Dager` == 1)
  nemnar <- nrow(df)

  ind <- teljar / nemnar

  res <- tibble::tibble(teljar, nemnar, ind = ind)
  res
}

# funksjon for å regne ut
# andel pasienter som får en alvorlig komplikasjon
#' @export
ki_kompl_alv <- function(df) {

  # Alvorlege komplikasjonar
  teljar <- sum(df$`6U_KomplAlvorGrad` >= 4, na.rm = TRUE)
  nemnar <- nrow(df)

  ind <- teljar / nemnar

  res <- tibble::tibble(teljar, nemnar, ind = ind)
  res
}
