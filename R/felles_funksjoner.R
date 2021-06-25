# Dette skriptet inneholder en rekke funksjoner som er potensielt nyttige
# (og noen, uunnværlige) i registersammenheng og i andre sammenheng. 


# LaTeX-ting --------------------------------------------------------------

# Kopier klassefila me brukar til ein plass LaTeX finn ho,
# slik at me slepp å ha ein kopi overalt.
# Sjå https://tex.stackexchange.com/a/1138 for meir informasjon.
texmappe_rot = system2("kpsewhich", "-var-value=TEXMFHOME", stdout = TRUE)
texmappe = paste0(texmappe_rot, "/tex/latex/kvalreg/")
dir.create(texmappe, showWarnings = FALSE, recursive = TRUE)
invisible(file.copy(from = "h:/kvalreg/felles/latex-klassar/kvalreg-rapport.cls",
          to = texmappe, overwrite = TRUE, copy.date = TRUE))

# Fargar og grafinnstillingar/-objekt -------------------------------------

# Dei offisielle fargene (som eg ikkje er så glad i)
# du mener, som INGEN liker. 
colPrim=c("#000059", "#084594", "#2171b5", "#4292c6", "#6baed6", "#c6dbef") # Primærfarge (mørk til lys)
colNoyt=c("#4D4D4D", "#737373", "#A6A6A6", "#DADADA") # Nøytralfarge
colKontr="#FF7260"                                    # Kontrastfarge

# ggplot2-tema for figurar
library(ggplot2)
if(!exists("skriftstorleik")) # Skriftstorleik bør vera definert i kvar årsrapportfil
   skriftstorleik = 19
tema = theme_light(base_size=skriftstorleik)
tema$panel.grid.minor$colour="white"
tema$strip.background$fill="#f3f1ee"
tema$strip.background$colour="#e4e0da"
tema$strip.text.x = element_text(colour="black")
tema$panel.spacing=unit("13" ,"pt")
tema$panel.border$colour=tema$strip.background$colour
tema$panel.grid.major$colour=tema$strip.background$colour
tema$panel.grid.minor$colour=tema$strip.background$fill
tema$axis.title.y$angle=0
tema$axis.title.y$margin=margin(r=5)
tema$axis.title.x$margin=margin(t=5)

# Fjern vannrette eller loddrette rutenett
fjern_x = theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank())
fjern_y = theme(panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank())

# Fjern strekmarkeringar for viste tal/kategoriar
# (tilsvarer «major breaks» på aksen).
# Dette er nyttig for søylediagram med kategoriske
# verdiar, der strekmarkeringane er unødvendige/stygge.
fjern_x_ticks = theme(axis.ticks.x = element_blank())
fjern_y_ticks = theme(axis.ticks.y = element_blank())

# Søyler skal i starta heilt inn til aksen, men ha litt luft
# over seg, altså asymmetriske expand-verdiar. Her er ein
# variabel som definerer dette, og som ein kan mata til
# expand-argumentet til skaladefinisjonar.
expand_soyle = expansion(mult = c(0.0, .05), add = 0)

# Fjern luft til venstre for y-akseteksten og legg
# til ekstra luft til høgre for han, fjern luft under
# x-akseteksten og legg til ekstra luft over han,
# fjern nesten all luft rundt figurane (eventuell
# nødvendig luft legg me til via LaTeX).
#
# (Merk: Me set den ytre margen til 3 punkt
# opp og nede i staden for 0 punkt for å sikra at at
# kantane på alle bokstavane alltid vert med.)
tema$axis.title.y$margin=margin(r=tema$text$size/2)
tema$axis.title.x$margin=margin(t=tema$text$size/2)
tema$plot.margin=margin(3, 3, 3, 3)


# Graffunksjoner ----------------------------------------------------------

# funksjon for å lage shewhart-diagram 
# funksjonen krever et datasett (d) som inneholder teller (y) og en variabel for x.aksen (x) som
# ofte er en datovariabel, hvilket type shewhart-diagram det er er (velges "p" må også nevner, tilgjengelig i d, oppgis) 
# gruppe er hvilken variabel man ønkser å dele opp et panel på, hvis ønskelig (default NULL), 
# periode hvilket tidsrom (f.eks "month" eller "2 months", gjelder kun tidsvising) 
# kan velge å legge til tittelen for plottet i tittel, x-aksenavn i x_navn og y-akse-navn i y-akse.
# krever pakkene tidyverse og qicharts2
lag_shewhart_fig = function(d, y, x, nevner = NULL, figtype, tittel = NULL, 
                        gruppe=NULL, periode = NULL, x_navn = NULL, y_navn = NULL, 
                        tidsvisning = TRUE, ...){
  
  # definerer alle kolonner som skal være tilgjengelig inni datasettet (d)
  qic_x = enexpr(x)
  qic_y = enexpr(y)
  qic_n = enexpr(nevner)
  qic_facet = enexpr(gruppe)
  
  if(is.Date(d[[qic_x]])){
    skal_flippes = FALSE}
  else{
    skal_flippes = TRUE
    }
    
  # lager grunnplottet med alt som alle shewhart-diagram trenger + eventuelle tilleggsvalg, og ggplot2 tema
  plot = eval_bare(expr(qic(data = d, y = !!qic_y, n = maybe_missing(!!qic_n), x = !!qic_x, chart = figtype, 
      title = tittel, xlab = x_navn, ylab = y_navn, show.labels = FALSE, x.period = periode, facets = ~ (!!qic_facet),
      flip = skal_flippes))) +
      tema +
      fjern_x +
      fjern_y +
      theme(legend.position = "none")  
  
  # legger på ekstra tema under visse forhold
  if(figtype == "p"){ # hvis det er p-chart ønsker vi norske prosenter fra funksjon i dette r-skriptet
  plot = plot + scale_y_continuous(labels = akse_prosent)   
  }
  if(is.Date(d[[qic_x]])){ # hvis det er en tidsvisning trenger vi en dot for punktene i linjediagrammet
  plot = plot + geom_point()
  } 
  plot 
}


# For å lage pene LaTeX-tabeller i et standardisert format for alle årsrapporter,
# med mulighet for å gjøre den stor nok til hele siden (wide = TRUE). 
# optional arguments inkluderer colheads=c() og caption = paste0(""). 
library(Hmisc)
library(stringr)
library(magrittr)
lag_tab_latex = function(dataframe, label, caption, wide=FALSE, ...){
  
  # Viss dataramma ikkje har nokon radar, bryt latex()-funksjonen
  # heilt saman dersom numeric.dollar er FALSE (og det er FALSE
  # me *vil* ha, for å få rett formatering av tal).
  #
  # fixme: Feilen er meldt inn til forfattaren av Hmisc-pakken
  #        i januar 2018, og er lovd retta. Fjern derfor følgjande
  #        if()-test når dette er retta og Hmisc-pakken er oppdatert.
  #
  #        Kan bruka følgjande kodesnutt for å sjekka om feilen
  #        er retta:
  #          latex(head(iris, 0), file="", numeric.dollar=FALSE)
  #        (skal gje tabell, ikkje feilmelding)
  if(nrow(dataframe) == 0 ) {
    tabell = paste0("\\begin{table}[htbp]\n",
                    "\\caption{", caption, "\\label{", label, "}}\n",
                    "{\\color{errorcolor}(Tabellen har 0 rader. Her må noko vera gale!)}\n",
                     "\\end{table}")
  } else {
  tabell = capture.output(latex(dataframe, file="", center="centering",
          label=label, caption=caption, rowname=NULL, 
          where="htbp", booktabs=TRUE, numeric.dollar=FALSE, ...))
  if (wide) {
    tabell %<>% str_replace("^\\\\(begin|end)\\{table\\}", "\\\\\\1\\{widetable\\}") # Superrobust ... ;)
  }
  tabell = paste0(tabell, sep = "\n")
  }
  
  # Returner tabellen (eller feilmelding)
  tabell
}
