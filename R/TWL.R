TWL <- function(dt){
# 34567890 ----------------------------------------------------------------- 80
d_GS    <- dt %>% dplyr::filter(.data$Operasjonsmetode == 6)
d_RYGBP <- dt %>% dplyr::filter(.data$Operasjonsmetode == 1,
                                .data$Opmetode_GBP == 1)   # mini = .data$Opmetode_GBP
d_OAGB  <- dt %>% dplyr::filter(.data$Operasjonsmetode == 1, 
                                .data$Opmetode_GBP == 2)

d_TWL  <- dt %>% dplyr::filter(.data$Operasjonsmetode %in% c(1, 6)) %>% 
                 dplyr::filter(!is.na(pTWL)) %>% mutate(del20 = pTWL >= 20.0)  # pTWL at 2 year must exist!  to_nt == TRUE
d_slv  <- d_TWL %>% dplyr::filter(.data$Operasjonsmetode == 6)
d_gbp  <- d_TWL %>% dplyr::filter(.data$Operasjonsmetode == 1, 
                                  .data$Opmetode_GBP == 1)
d_oa   <- d_TWL %>% dplyr::filter(.data$Operasjonsmetode == 1, 
                                  .data$Opmetode_GBP == 2)     

detail    <-  function(dmx) {dmx %>% 
 dplyr::group_by(.data$OperererendeSykehus, .data$op_aar)}
shw       <-  function(dmx) {dmx %>% tbl_df %>% rmarkdown::paged_table()}

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

 
 
#op_15m   <- dt %>% dplyr::filter(Operasjonsdato<k1_last) %>% dplyr::group_by(.data$OperererendeSykehus) %>%  dplyr::summarise("ops"=n())
# op_27m   <- dt %>% dplyr::filter(Operasjonsdato<k2_last) %>% dplyr::group_by(.data$OperererendeSykehus) %>%  dplyr::summarise("ops"=n())
# 34567890 ----------------------------------------------------------------- 80
#           tb20 tabell for årsrapporten  
tb <-     dt %>% dplyr::group_by(.data$OperererendeSykehus) %>% 
                 dplyr::summarise( "to år %TWL"= mean(pTWL, na.rm = TRUE)) 

tb20 <-   dt %>% mutate(del20 = pTWL>=20.0 )  %>% 
 dplyr::group_by(.data$OperererendeSykehus) %>% 
 dplyr::summarise("ops" = dplyr::n(), 
                  "del %TWL>=20.0"= mean(.env$del20, na.rm = TRUE))

tb20toar <- dt %>% mutate(del20 = pTWL>=20.0 ) %>% 
 dplyr::filter(to_nt)  %>% dplyr::group_by(.data$OperererendeSykehus) %>% 
 dplyr::summarise("ops" = dplyr::n(), 
                  "del %TWL>=20.0"= mean(.env$del20, na.rm = TRUE))


tb20_GS <- .env$d_GS %>% mutate(del20= pTWL>=20.0 ) %>% 
 dplyr::filter(to_nt) %>% dplyr::group_by(.data$OperererendeSykehus) %>% 
 dplyr::summarise("ops" = dplyr::n(),  
                  "del %TWL>=20.0"= mean(.env$del20, na.rm = TRUE))

tb20_RYGBP <- .env$d_RYGBP %>% mutate(del20= pTWL>=20.0 ) %>% 
 dplyr::filter(to_nt) %>% dplyr::group_by(.data$OperererendeSykehus) %>% 
 dplyr::summarise("ops" = dplyr::n(),  
                  "del %TWL>=20.0"= mean(.env$del20, na.rm = TRUE))

tb20_OAGB <- .env$d_OAGB %>% mutate(del20= pTWL>=20.0 )  %>% 
 dplyr::filter(to_nt) %>% dplyr::group_by(.data$OperererendeSykehus) %>% 
 dplyr::summarise("ops" = dplyr::n(),  
                  "del %TWL>=20.0" = mean(.env$del20, na.rm = TRUE))
 
tb20b <- dt %>% mutate(del20= pTWL>=20.0 ) %>%
 dplyr::group_by(.data$OperererendeSykehus) %>% 
 mutate(ops = dplyr::n()) %>%
 dplyr::summarise(kt2 = sum(to_nt, na.rm = T)/ .env$ops[1]) }
 
GS20 <- function(sh, yr) {.env$d_GS %>%  mutate(del20= pTWL>=20.0 ) %>%
 dplyr::filter(.data$OperererendeSykehus %in% .env$sh,
               .data$op_aar %in% .env$yr) %>% 
 dplyr::group_by(.data$OperererendeSykehus) %>% 
 dplyr::filter(to_nt) %>%  
 dplyr::summarise("ops"= dplyr::n(), 
                  "del %TWL>=20.0" = mean(.env$del20, na.rm = TRUE))}
				  
GB20 <- function(sh, yr) 
        {.env$d_RYGBP %>% mutate(del20 = pTWL >= 20.0 ) %>%
 dplyr::filter(.data$OperererendeSykehus %in% .env$sh, 
               .data$op_aar %in% .env$yr) %>%
 dplyr::group_by(.data$OperererendeSykehus) %>% dplyr::filter(to_nt) %>%
 dplyr::summarise("ops" = dplyr::n(),
                  "del %TWL>=20.0" = mean(.env$del20, na.rm = TRUE))} 