library(tidyverse)
library(lubridate)
library(datasets)
library(ggplot2)
library(tidyr)
library(dplyr)
library(DT)
library(purrr)
library(stringr)
library(magrittr)

# library(rapwhale)
library(flexdashboard)
library(shinyWidgets)


d_full <- soreg::get_arsrp("soreg")

# Legg til info om operasjonsår
d_full %<>% mutate(op_aar = year(Operasjonsdato),
                   op_primar = (TidlFedmeOp==0))
# Rekn ut BMI for alle tidspunkta
# Merk: Ein registrerer kroppshøgd på alle tidspunkta *utanom*
#       for operasjonstidspunktet! Me har altså kroppsvekt ved
#       baseline, 6 veker etter operasjon, 1 år etter operasjon,
#       2 år etter operasjon osv., men ikkje *ved* operasjon.
#       Der har me berre vekt (men heller ikkje for alle, for
#       operasjonsvekt er ikkje obligatorisk!).
#
#       Derfor må me ta ein spansk ein og bruka
#       kroppshøgd ved *baseline* for utrekning
#       av BMI på operasjonstidspunktet. :(
d_full = d_full %>% mutate(bmi_baseline = BR_Vekt/(BR_Hoyde/100)^2,
                           bmi_op = OperasjonVekt/(BR_Hoyde/100)^2,
                           bmi_6v = `6U_Vekt`/(`6U_Hoyde`/100)^2,
                           bmi_1a = `1Aar_Vekt`/(`1Aar_Hoyde`/100)^2,
                           bmi_2a = `ToAar_Vekt`/(`ToAar_Hoyde`/100)^2)

# Data for alle år opptil (og inkludert) rapporteringsår,
# men ikkje for seinare år
#
# (Brukar '< dato + 1' i staden for '<= dato' for å handtera
# det rett dersom ein i framtida endrar operasjonsdato
# til å vera operasjonstidspunkt i staden for operasjondato
# (ein dato tolka som tidspunkt er klokka 00:00 den aktuelle
# datoen, ikkje 24:00, og inkluderer derfor ikkje tidsperioden
# frå 00:00 til 24:00, så me ville mista det siste døgnet).)
rapporteringsaar = 2019
dato_opptilrapaar = as_date(paste0(rapporteringsaar, "-12-31")) # For ev. seinare bruk i teksten
d_opptilrapaar = d_full %>%
  filter(Operasjonsdato < (dato_opptilrapaar + 1))

# Data for alle år opptil (og inkludert) to år før rapporteringsåret,
# dvs. alle data me i teorien burde ha toårs oppfølgingsdata på
dato_toaarsdata = as_date(paste0(rapporteringsaar - 2, "-12-31")) # For ev. seinare bruk i teksten
d_toaarsdata = d_full %>%
  filter(Operasjonsdato < (dato_toaarsdata + 1))

# Og tilsvarande for eittårsdata
dato_eittaarsdata = as_date(paste0(rapporteringsaar - 1, "-12-31")) # For ev. seinare bruk i teksten
d_eittaarsdata = d_full %>%
  filter(Operasjonsdato < (dato_eittaarsdata + 1))

# Dei som vart operert det aktuelle året
d = d_full #  %>%  filter(op_aar == rapporteringsaar)

# Talet på operasjonsmåtar
n_op = nrow(d_full)
n_pas = n_distinct(d$PasientID)

# d_prim = d %>% filter(op_primar)
d_prim = d_full %>% filter(op_primar)


# I nokre analysar ser me berre på dei som har 6-vekesoppfølging
# registrert. Hentar ut eiga datasett for desse
d_prim_6v = d_prim %>% filter(`6U_KontrollType` %in% 1:3)

d_toaarsdata_prim = d_toaarsdata %>% filter(op_primar)
d_eittaarsdata_prim = d_eittaarsdata %>% filter(op_primar)
maksdogn_vis = 14

# Talet på primæroperasjonar og reoperasjonar
n_prim = nrow(d_prim)
n_reop = n_op - n_prim


# Kva var den vanlegaste talet døgn å ligga etter operasjon
liggedogn_typetal = d_prim_6v %>%
  count(LiggeDogn) %>%
  arrange(desc(n)) %$%
  LiggeDogn %>%
  first

# funksjon for å regne ut kvalitetsindikatoren
# definert for liggetid (andel pasienter med 3 dager eller færre)
ki_liggetid = function(df){

  teljar = sum(df$LiggeDogn <= 3)
  nemnar = nrow(df)

  ind = teljar/nemnar

  res = tibble(# forklaring="Del pasienter med 3 eller færre liggedøgn",
    # ltx_tekst = "Tre/færre liggedøgn",
    teljar, nemnar, ind = ind) # trenger ikke å definere at
  # variabler som er med i en group_by
  # skal være med, de kommer uansett
  res
}

# Kor mange låg mindre enn fire døgn
p_kortligg = ki_liggetid(d_prim_6v)$ind

# Kor mange låg mindre enn fire døgn per sjukehus
d_kortligg_sjuk = d_prim_6v %>%
  group_by(OperererendeSykehus, op_aar) %>%
  do(ki_liggetid(.)) %>%
  arrange(desc(ind))

kortligg_sh <- function(sh) {d_kortligg_sjuk %>% filter(OperererendeSykehus %in% sh)}
kortligg_yr <- function(yr) {d_kortligg_sjuk %>% filter(op_aar %in% yr)}
kortligg    <- function(sh,yr){d_kortligg_sjuk %>% filter(OperererendeSykehus %in% sh, op_aar %in% yr)}

# shiny inputs
min_dato <-min(d_full$Operasjonsdato)
max_dato <-max(d_full$Operasjonsdato)
fyrstAar<-year(min_dato)
sistAar<-year(max_dato)

pickerInput(
  inputId = "sh",
  label = "velg sjukehus",
  choices =  c("Helse Bergen","Helse Stavanger",
               "Testsjukhus Norge"),   # unique(d_full$OperererendeSykehus),  #
  selected = "Testsjukhus Norge",
  multiple = TRUE,
  options = pickerOptions(
    actionsBox = TRUE,
    title = "Please select a hospital",
    header = "This is a list of hospitals"
  ))
# checkboxGroupInput(inputId = "lggar", label ="år", choices=c(2017,2018)),  # multiple?
# selectInput(inputId= "vrb", label = "Variabel:", c("bmi_baseline")),
# sliderInput("bn",  label = "Antall grupper:",
#             min = 1,
#             max = 10,
#             value = 5)
# ),
# mainPanel(          tabsetPanel(
#   tabPanel("Plot en variabel",      plotOutput("PlotKI1")),
#   tabPanel("Data table", tableOutput("TableKI1")),
#   tabPanel("own table", dataTableOutput('ligge'))
# )
# ))
# ),
# # sliderInput("ixs", label ="år", min=fyrstAar, max=sistAar, value=c(2017,2018), step=1)
# #   checkboxGroupInput(inputId = "aar", label ="år", choices=c(2017,2018)) , # multiple?
# # radioButtons("år",label ="years", fyrstAar:sistAar)  # multiple?
# # selectInput("sykeh", label="sykehus", unique(dt$OperererendeSykehus),multiple=TRUE)
# # selectInput("n_breaks", label = "Number of bins:",
# #            choices = c(10, 20, 35, 50), selected = 20)
# dateRangeInput('dateRange',
#                label = 'Datointerval: yyyy-mm-dd',
#                start = min_dato, end = max_dato)
#
