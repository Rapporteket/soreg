#' Graphics settings
#'
#' @return settings
#'
#' @export

grafikk <- function(){
# -------------------------- grafikkinnstillingar ----------------------     80
# Offisielle fargar (som eg ikkje likar så godt)
colPrim=c("#000059", "#084594", "#2171b5", "#4292c6",
          "#6baed6", "#c6dbef")                       # Primærfarge (mørk til lys)
colNøyt=c("#4D4D4D", "#737373", "#A6A6A6", "#DADADA") # Nøytralfarge
colKontr="#FF7260"                                    # Kontrastfarge

# Av og til treng me litt mørkare/lysare variantar av primærfargane
# (og *førre/neste* verdi i fargeskalaen er *heilt ueigna*!).
# Legg til desse som alternativ fargeskala
##colPrim_mork = farge_morkare(colPrim, grad=5)
##colPrim_lys = farge_morkare(colPrim, grad=-5)

# Offisielle årsrapportfargar for qicharts2-diagram
options(qic.signalcol = colKontr, qic.linecol = colPrim[3])

# ggplot2-tema for figurar
#tema = theme_light(base_size=skriftstorleik)

# Finare, litt varme fargar på panelstriper
#tema$strip.background$fill="#f3f1ee"
#tema$strip.background$colour="#e4e0da"
#tema$strip.text.x = element_text(colour="black")

# Større avstand mellom panel (nødvendig sidan me
# ikkje brukar farga panelbakgrunn, og ting ser
# litt rotete ut om det er lite luft mellom panela)
#tema$panel.spacing=unit("13" ,"pt")

# Men nokre plott krev liten panelavstand
#liten_panel_avstand = theme(panel.spacing = unit(6, "pt"))

# Bruk same fargar på rutenetta som på panelbakgrunnen
# Merk: panel.background vert teikna *under* akselinjer
#       og grafelement, mens panel.border vert teikna *oppå*.
#       Det vil me ikkje, for det kan gjera at tynne søyler
#       vert usynlege (dekka av panel.border).
#
#       Derfor teiknar me *aldri* panel.border, berre panel.background.
#       (fixme: må kanskje endrast dersom me tar i bruk fleirpanelsgrafar?)
# tema$panel.border = element_blank()
# tema$panel.background$colour=tema$strip.background$colour
# tema$panel.grid.major$colour=tema$strip.background$colour
# tema$panel.grid.minor$colour=tema$strip.background$fill
#
# # La teksten på y-aksen vera vassrett
# tema$axis.title.y$angle=0

# Fjern luft til venstre for y-akseteksten og legg
# til ekstra luft til høgre for han, fjern luft under
# x-akseteksten og legg til ekstra luft over han,
# fjern nesten all luft rundt figurane (eventuell
# nødvendig luft legg me til via LaTeX).
#
# (Merk: Me set den ytre margen til 3 punkt
# opp og nede i staden for 0 punkt for å sikra at at
# kantane på alle bokstavane alltid vert med.)
# tema$axis.title.y$margin=margin(r=tema$text$size/2)
# tema$axis.title.x$margin=margin(t=tema$text$size/2)
# tema$plot.margin=margin(3, 3, 3, 3)

# Bruk temaet som standardtema
#theme_set(tema)

# Fjern vannrette eller loddrette rutenett
# ved førespurnad
# fjern_x = theme(panel.grid.major.x = element_blank(),
#                 panel.grid.minor.x = element_blank())
# fjern_y = theme(panel.grid.major.y = element_blank(),
#                 panel.grid.minor.y = element_blank())

# Fjern strekmarkeringar for viste tal/kategoriar
# (tilsvarer «major breaks» på aksen).
# Dette er nyttig for søylediagram med kategoriske
# verdiar, der strekmarkeringane er unødvendige/stygge.
#fjern_x_ticks = theme(axis.ticks.x = element_blank())
#fjern_y_ticks = theme(axis.ticks.y = element_blank())

# Søyler skal i starta heilt inn til aksen, men ha litt luft
# over seg, altså asymmetriske expand-verdiar. Her er ein
# variabel som definerer dette, og som ein kan mata til
# expand-argumentet til skaladefinisjonar.
#expand_soyle = expansion(mult = c(0.0, .05), add = 0)

# Funksjon for å rekna ut rimeleg høgd på figurar
# som har vassrette stolpediagram
# (fixme: ikkje gjennomtesta, og bør nok få litt finpussing)
fighogd_stolpe = function(n) {
  0.54 + 0.24*n
}
# -------------------------- grafikkinnstillingar ----------------------     80
}