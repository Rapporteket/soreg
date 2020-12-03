#' Pick particular hospitals and years from a data frame
#'
#' @param dt Data frame holding full SoReg data
#'
#' @return A data frame for choice of hospitals and years
#' @return A data frame for choice of hospitals and years
#'
#' @export

TWL <- function(dt){
d_GS    <- dt %>% dplyr::filter(.data$Operasjonsmetode == 6)
d_RYGBP <- dt %>% dplyr::filter(.data$Operasjonsmetode == 1,
                                .data$Opmetode_GBP == 1)   # mini = .data$Opmetode_GBP
d_OAGB  <- dt %>% dplyr::filter(.data$Operasjonsmetode == 1,
                                .data$Opmetode_GBP == 2)

d_TWL  <- dt %>% dplyr::filter(.data$Operasjonsmetode %in% c(1, 6)) %>%
                 dplyr::filter(!is.na(.env$TWL)) %>% dplyr::mutate(del20 = .env$TWL >= 20.0)

d_slv  <- d_TWL %>% dplyr::filter(.data$Operasjonsmetode == 6)
d_gbp  <- d_TWL %>% dplyr::filter(.data$Operasjonsmetode == 1,
                                  .data$Opmetode_GBP == 1)
d_oa   <- d_TWL %>% dplyr::filter(.data$Operasjonsmetode == 1,
                                  .data$Opmetode_GBP == 2)

detail    <-  function(dmx) {dmx %>%
 dplyr::group_by(.data$OperererendeSykehus, .data$op_aar)}

slv20   <-    detail(d_slv) %>%
 dplyr::summarise("tyve"= sum(.env$del20, na.rm = TRUE),  "ops" = dplyr::n(),
                 "minst20" = mean(.env$del20, na.rm = TRUE))
gbp20   <-    detail(d_gbp) %>%
 dplyr::summarise("tyve"= sum(.env$del20, na.rm = TRUE),  "ops" = dplyr::n(),
                 "minst20" = mean(.env$del20, na.rm = TRUE))
 oa20   <-    detail(d_oa)  %>%
 dplyr::summarise("tyve"= sum(.env$del20, na.rm = TRUE),  "ops" = dplyr::n(),
                  "minst20" = mean(.env$del20, na.rm = TRUE))

slvdel20 <- function(sh, yr) {slv20 %>%
dplyr::filter(.data$OperererendeSykehus %in% .env$sh,
              .data$op_aar %in% .env$yr)}
gbpdel20 <- function(sh, yr) {gbp20 %>%
dplyr::filter(.data$OperererendeSykehus %in% .env$sh,
              .data$op_aar %in% .env$yr)}
 oadel20 <- function(sh, yr) { oa20 %>%
 dplyr::filter(.data$OperererendeSykehus %in% .env$sh,
               .data$op_aar %in% .env$yr)}

tb <-     dt %>% dplyr::group_by(.data$OperererendeSykehus) %>%
                 dplyr::summarise( "to år %TWL"= mean(.env$pTWL, na.rm = TRUE))

tb20 <-   dt %>% dplyr::mutate(del20 = .env$pTWL>=20.0 )  %>%
 dplyr::group_by(.data$OperererendeSykehus) %>%
 dplyr::summarise("ops" = dplyr::n(),
                  "del %TWL>=20.0"= mean(.env$del20, na.rm = TRUE))

tb20toar <- dt %>% dplyr::mutate(del20 = .env$pTWL>=20.0 ) %>%
 dplyr::filter(.env$to_nt)  %>% dplyr::group_by(.data$OperererendeSykehus) %>%
 dplyr::summarise("ops" = dplyr::n(),
                  "del %TWL>=20.0"= mean(.env$del20, na.rm = TRUE))

tb20_GS <- .env$d_GS %>% dplyr::mutate(del20= .env$pTWL>=20.0 ) %>%
 dplyr::filter(.env$to_nt) %>% dplyr::group_by(.data$OperererendeSykehus) %>%
 dplyr::summarise("ops" = dplyr::n(),
                  "del %TWL>=20.0"= mean(.env$del20, na.rm = TRUE))

tb20_RYGBP <- .env$d_RYGBP %>% dplyr::mutate(del20= .env$TWL>=20.0 ) %>%
 dplyr::filter(.env$to_nt) %>% dplyr::group_by(.data$OperererendeSykehus) %>%
 dplyr::summarise("ops" = dplyr::n(),
                  "del %TWL>=20.0"= mean(.env$del20, na.rm = TRUE))

tb20_OAGB <- .env$d_OAGB %>% dplyr::mutate(del20= .env$TWL>=20.0 )  %>%
 dplyr::filter(.env$to_nt) %>% dplyr::group_by(.data$OperererendeSykehus) %>%
 dplyr::summarise("ops" = dplyr::n(),
                  "del %TWL>=20.0" = mean(.env$del20, na.rm = TRUE))

tb20b <- dt %>% dplyr::mutate(del20= .env$TWL>=20.0 ) %>%
 dplyr::group_by(.data$OperererendeSykehus) %>%
 dplyr::mutate(ops = dplyr::n()) %>%
 dplyr::summarise(kt2 = sum(.env$to_nt, na.rm = T)/ .env$ops[1]) }

#' Pick particular hospitals and years from a data frame
#'
#' @param sh hospital
#' @param yr year(s)
#'
#' @return  A data frame for choice of hospitals and years
#'
#' @export

GS20 <- function(sh, yr) {.env$d_GS %>%  dplyr::mutate(del20 = .env$pTWL >= 20.0 ) %>%
dplyr::filter(.data$OperererendeSykehus %in% .env$sh, .data$op_aar %in% .env$yr) %>%
dplyr::group_by(.data$OperererendeSykehus) %>%
dplyr::filter(.env$to_nt) %>%
dplyr::summarise("ops"= dplyr::n(),
                  "del %TWL>=20.0" = mean(.env$del20, na.rm = TRUE))}

#--------------------------------------------------------------------------- 80
#' Pick particular hospitals and years from a data frame
#'
#' @param sh hospital
#' @param yr year(s)
#'
#' @return  A data frame for choice of hospitals and years
#'
#' @export

GB20 <- function(sh, yr)
        {.env$d_RYGBP %>% dplyr::mutate(del20 = .env$pTWL >= 20.0 ) %>%
 dplyr::filter(.data$OperererendeSykehus %in% .env$sh,
               .data$op_aar %in% .env$yr) %>%
 dplyr::group_by(.data$OperererendeSykehus) %>% dplyr::filter(.env$to_nt) %>%
 dplyr::summarise("ops" = dplyr::n(),
                  "del %TWL>=20.0" = mean(.env$del20, na.rm = TRUE))}
