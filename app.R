# app.R
# Tablero de producción de oleína
# Foco: oleína, consumo de ácido y de materia prima, rinde objetivo y tiempos del proceso.
# Antes de correr esto hay que haber corrido codigo/limpieza.R.
# Materia: Análisis Inteligente de Datos

library(shiny)
library(dplyr)
library(ggplot2)

# app.R vive en la raíz del proyecto, así que los datos están en datos_limpios/
ruta_datos <- "datos_limpios/produccion.rds"
produccion <- readRDS(ruta_datos)
lotes <- produccion$lotes

# Tema base para todos los gráficos. Uso theme_bw().
tema <- theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

verde    <- "#2E8B57"
naranja  <- "#E07B39"
gris     <- "#9aa0a6"

# Estilos de las tarjetas de KPI, para que las 5 queden del mismo tamaño
estilo_tarjeta  <- paste("flex:1; background:#f7f8fa; border:1px solid #e6e8ec;",
                         "border-radius:8px; padding:16px 10px; text-align:center;")
estilo_numero   <- "font-size:24px; font-weight:700; color:#2E8B57;"
estilo_etiqueta <- "color:#6b7480; font-size:13px; margin-top:4px;"

# Nombres cortos de los días (0 = domingo) y un formateador de fecha-hora
dias_cortos <- c("Dom", "Lun", "Mar", "Mié", "Jue", "Vie", "Sáb")
fmt_dh <- function(x) paste0(dias_cortos[as.POSIXlt(x)$wday + 1], " ", format(x, "%H:%M"))

# Nombre completo del día a partir del corto (para los textos)
nombre_dia <- c("Lun" = "Lunes", "Mar" = "Martes", "Mié" = "Miércoles", "Jue" = "Jueves",
                "Vie" = "Viernes", "Sáb" = "Sábado", "Dom" = "Domingo")

# Colores de las etapas para el Gantt de la semana ideal
colores_etapa <- c("Carga MP" = "#4E79A7", "Espera" = "#BAB0AC", "Carga ácido" = "#F28E2B",
                   "Reacción" = "#59A14F", "Decantación" = "#9C6ADE")

# Genera el cronograma de la semana ideal (8 lotes, uno nuevo cada 14 h) a partir
# de la hora de arranque del lunes. Devuelve los segmentos y un resumen por lote.
generar_semana_ideal <- function(hora_inicio = 6.5) {
  react <- c("A", "B", "C", "D", "E", "F")
  decs  <- rep(c("TK-13", "TK-14"), length.out = 8)
  lunes <- as.Date("2026-11-02")               # un lunes cualquiera de referencia
  base  <- as.POSIXct(paste(lunes, "00:00:00"), tz = "UTC") + hora_inicio * 3600

  segs <- list(); resumen <- list()
  for (k in 0:7) {
    inicio <- base + k * 14 * 3600             # un lote nuevo cada 14 h
    dec <- decs[k + 1]
    fin_react <- inicio
    for (i in seq_along(react)) {
      t0  <- inicio + (i - 1) * 30 * 60        # reactores escalonados cada 30 min
      mpf <- t0 + 30 * 60
      aci <- mpf + 90 * 60
      acf <- aci + 13 * 60
      rxi <- acf
      rxf <- rxi + 600 * 60                     # reacción de 10 h
      fin_react <- max(fin_react, rxf)
      segs[[length(segs) + 1]] <- data.frame(
        recurso = react[i],
        etapa = c("Carga MP", "Espera", "Carga ácido", "Reacción"),
        ini = c(t0, mpf, aci, rxi), fin = c(mpf, aci, acf, rxf))
    }
    deci <- fin_react + 66 * 60                  # el decantador se llena 1,1 h después
    decf <- deci + 1440 * 60                     # decanta 24 h
    segs[[length(segs) + 1]] <- data.frame(
      recurso = dec, etapa = "Decantación", ini = deci, fin = decf)
    resumen[[length(resumen) + 1]] <- data.frame(
      Lote = paste("Lote", k + 1), Decantador = dec,
      inicio = inicio, fin_reaccion = fin_react, fin_decantacion = decf)
  }
  list(segs = do.call(rbind, segs), resumen = do.call(rbind, resumen))
}

# ---- Interfaz ------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Producción de oleína - Tablero de proceso"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("anio", "Año:",
                  choices = c("Ambos" = "todos", "2025", "2026"),
                  selected = "todos"),
      sliderInput("objetivo", "Rinde objetivo mínimo (%):",
                  min = 10, max = 50, value = 25, step = 1),
      helpText("El rinde objetivo se usa en la pestaña 'Rinde objetivo'",
               "para ver cuántos lotes quedan por debajo de la meta."),
      hr(),
      tags$small("Datos reales de la planta (2025-2026).")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Resumen",
                 br(),
                 tags$div(
                   style = "display:flex; gap:12px; align-items:stretch; margin-bottom:14px;",
                   tags$div(style = estilo_tarjeta,
                            tags$div(style = estilo_numero, textOutput("kpi_oleina", inline = TRUE)),
                            tags$div(style = estilo_etiqueta, "Oleína total (Tn)")),
                   tags$div(style = estilo_tarjeta,
                            tags$div(style = estilo_numero, textOutput("kpi_rinde", inline = TRUE)),
                            tags$div(style = estilo_etiqueta, "Rinde medio (%)")),
                   tags$div(style = estilo_tarjeta,
                            tags$div(style = estilo_numero, textOutput("kpi_lotes", inline = TRUE)),
                            tags$div(style = estilo_etiqueta, "Lotes producidos")),
                   tags$div(style = estilo_tarjeta,
                            tags$div(style = estilo_numero, textOutput("kpi_mp", inline = TRUE)),
                            tags$div(style = estilo_etiqueta, "Materia prima (Tn)")),
                   tags$div(style = estilo_tarjeta,
                            tags$div(style = estilo_numero, textOutput("kpi_acido", inline = TRUE)),
                            tags$div(style = estilo_etiqueta, "Ácido usado (Tn)"))
                 ),
                 htmlOutput("caption_sel"),
                 actionLink("ver_total", "Ver el total del período"),
                 plotOutput("g_resumen", height = "360px", click = "clic_mes")),

        tabPanel("Oleína",
                 br(),
                 plotOutput("g_oleina_mes", height = "330px"),
                 htmlOutput("txt_oleina_mes"),
                 br(),
                 plotOutput("g_oleina_dist", height = "300px"),
                 htmlOutput("txt_oleina_dist")),

        tabPanel("Consumo de MP",
                 br(),
                 plotOutput("g_mp_barras", height = "340px", click = "clic_mp"),
                 htmlOutput("txt_mp"),
                 actionLink("ver_total_mp", "Ver el total del período")),

        tabPanel("Consumo de ácido",
                 br(),
                 plotOutput("g_acido_barras", height = "340px", click = "clic_acido"),
                 htmlOutput("txt_acido"),
                 actionLink("ver_total_acido", "Ver el total del período"),
                 hr(),
                 h4("Reactores según la dosis de ácido"),
                 p("Equivalencia entre el % en peso y los litros de ácido (la dosis se ",
                   "regula en pasos de 65 L, que son 0,5% sobre las 24 Tn de materia prima):"),
                 tableOutput("tabla_dosis_acido"),
                 plotOutput("g_acido_intervalos", height = "320px"),
                 htmlOutput("txt_acido_intervalos")),

        tabPanel("Rinde objetivo",
                 br(),
                 p("El rinde es el porcentaje de la carga que se recupera como oleína. ",
                   "Con el deslizador de la izquierda se fija un rinde objetivo mínimo y se ve ",
                   "cuántos lotes lo cumplen y cuántos quedan por debajo."),
                 plotOutput("g_objetivo", height = "340px"),
                 br(),
                 htmlOutput("txt_objetivo")),

        tabPanel("Arranque de planta",
                 br(),
                 p("Así queda una semana ideal: 8 lotes, uno nuevo cada 14 h, con los ",
                   "reactores A a F escalonados y los decantadores 13 y 14 alternados. ",
                   "Se puede ajustar la hora de arranque del lunes para observar el efecto de una demora."),
                 sliderInput("hora_lunes", "Hora de arranque del lunes:",
                             min = 6, max = 12, value = 6.5, step = 0.5, width = "320px"),
                 helpText("6,5 equivale a las 06:30. El objetivo es arrancar temprano el lunes."),
                 plotOutput("g_gantt_ideal", height = "380px"),
                 br(),
                 htmlOutput("txt_impacto"),
                 br(), br(),
                 h4("Resultados esperados por lote"),
                 tableOutput("tabla_ideal")),

        tabPanel("Semana ideal vs real",
                 br(),
                 p("Comparamos los tiempos reales de cada etapa contra los de la semana ",
                   "ideal, cuantificamos el tiempo muerto y miramos el comportamiento de ",
                   "los turnos."),
                 h4("Tiempos esperados: ideal vs real"),
                 p("Diferencia = real - ideal. Un valor positivo significa que en la ",
                   "práctica lleva más tiempo (o arranca más tarde) que el ideal."),
                 tableOutput("tabla_tiempos"),
                 hr(),
                 h4("Tiempo muerto por mes"),
                 p("Separamos el tiempo muerto en dos tramos y en cada gráfico la línea roja ",
                   "marca el valor ideal. Primero, la espera previa al agregado de ácido: ",
                   "sumamos los minutos de espera de todos los reactores del mes (barras) y los ",
                   "comparamos con el ideal del mes (línea roja), que es 90 min por reactor. El ",
                   "exceso es la parte de la barra que supera la línea."),
                 plotOutput("g_muerto_acido", height = "280px"),
                 htmlOutput("txt_muerto_acido"),
                 br(),
                 p("Segundo, el tiempo de carga del decantador: desde que termina el primer ",
                   "reactor del lote hasta que el decantador queda lleno. Acá sumamos los ",
                   "minutos reales de carga de todos los lotes del mes (barras) y los comparamos ",
                   "con el ideal del mes (línea roja). Como el decantador se llena descargando ",
                   "un reactor por vez (30 min cada uno) y el primero ya marca el arranque, el ",
                   "ideal de cada lote es 30 min x (reactores - 1), sumado sobre todos sus lotes."),
                 plotOutput("g_muerto_decant", height = "280px"),
                 htmlOutput("txt_muerto_decant"),
                 h4("Temperatura de inicio del agregado de ácido"),
                 p("Si la temperatura a la que arranca el agregado de ácido varía mucho entre ",
                   "reactores, es una posible causa de los tiempos muertos y un argumento para ",
                   "estandarizar el proceso."),
                 plotOutput("g_temp_acido", height = "300px"),
                 htmlOutput("txt_temp"),
                 h4("Producción por día de la semana"),
                 plotOutput("g_turnos_dia", height = "260px"),
                 htmlOutput("txt_dias"),
                 hr(),
                 h4("Lotes según cantidad de reactores"),
                 sliderInput("max_react", "Ver lotes con esta cantidad de reactores o menos:",
                             min = 4, max = 7, value = 5, step = 1, width = "380px"),
                 htmlOutput("txt_reactores"),
                 br(),
                 plotOutput("g_reactores", height = "280px"))
      )
    )
  )
)

# ---- Servidor ------------------------------------------------------------

server <- function(input, output) {

  # Datos filtrados según el año elegido
  datos <- reactive({
    if (input$anio == "todos") lotes else filter(lotes, anio == input$anio)
  })

  # Lo mismo pero para la tabla de reacción (una fila por reactor)
  datos_reaccion <- reactive({
    if (input$anio == "todos") produccion$reaccion
    else filter(produccion$reaccion, anio == input$anio)
  })

  # ---- Resumen ----

  # Mes seleccionado al hacer clic en el gráfico (NULL = todo el período)
  mes_sel <- reactiveVal(NULL)
  observeEvent(input$clic_mes, {
    meses <- sort(unique(datos()$mes))
    idx <- round(input$clic_mes$x)
    if (idx >= 1 && idx <= length(meses)) mes_sel(meses[idx])
  })
  observeEvent(input$ver_total, mes_sel(NULL))
  observeEvent(input$anio, mes_sel(NULL))     # al cambiar de año vuelvo al total

  # Datos que resumen las tarjetas: un mes si hay selección, si no todo el período
  datos_kpi <- reactive({
    if (is.null(mes_sel())) datos() else filter(datos(), mes == mes_sel())
  })

  output$kpi_oleina <- renderText(format(round(sum(datos_kpi()$oleina_tn, na.rm = TRUE)),
                                          big.mark = ".", decimal.mark = ","))
  output$kpi_rinde  <- renderText(round(mean(datos_kpi()$rinde_ole, na.rm = TRUE), 1))
  output$kpi_lotes  <- renderText(nrow(datos_kpi()))
  output$kpi_mp     <- renderText(format(round(sum(datos_kpi()$mp_tn, na.rm = TRUE)),
                                          big.mark = ".", decimal.mark = ","))
  output$kpi_acido  <- renderText(format(round(sum(datos_kpi()$acido_tn, na.rm = TRUE)),
                                          big.mark = ".", decimal.mark = ","))

  output$caption_sel <- renderUI({
    if (is.null(mes_sel()))
      HTML("Las tarjetas muestran el <b>total del período</b>. Haga clic en una barra para ver un mes puntual.")
    else
      HTML(paste0("Las tarjetas muestran el mes <b>", mes_sel(), "</b>."))
  })

  output$g_resumen <- renderPlot({
    d <- datos() %>%
      group_by(mes) %>%
      summarise(oleina = sum(oleina_tn, na.rm = TRUE), .groups = "drop")
    # la barra seleccionada queda en verde, el resto en gris claro
    d$sel <- if (is.null(mes_sel())) TRUE else d$mes == mes_sel()
    ggplot(d, aes(mes, oleina, fill = sel)) +
      geom_col() +
      scale_fill_manual(values = c("TRUE" = verde, "FALSE" = "#dcdcdc"), guide = "none") +
      labs(title = "Oleína producida por mes (haga clic en una barra)",
           x = "Mes", y = "Oleína (Tn)") +
      tema +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  })

  # ---- Oleína ----
  output$g_oleina_mes <- renderPlot({
    d <- datos()
    ggplot(d, aes(fecha, oleina_tn)) +
      geom_point(color = gris, alpha = 0.5) +
      geom_smooth(color = verde, se = FALSE, method = "loess", span = 0.3) +
      labs(title = "Oleína por lote a lo largo del tiempo",
           x = "Fecha", y = "Oleína (Tn)") +
      tema
  })

  output$txt_oleina_mes <- renderUI({
    d <- datos()
    n <- nrow(d)
    cero <- sum(d$oleina_tn == 0, na.rm = TRUE)
    HTML(paste0(
      "Cada punto es un lote: la fecha en la que se produjo y las toneladas de oleína que ",
      "rindió. La línea verde es la tendencia. En el período seleccionado <b>", cero,
      "</b> lotes rindieron <b>0 Tn</b> de oleína, el <b>",
      format(round(cero / n * 100, 1), decimal.mark = ","), "%</b> del total de ", n,
      " lotes. Son lotes fallidos que conviene revisar aparte."))
  })

  output$g_oleina_dist <- renderPlot({
    d <- datos()
    media <- mean(d$oleina_tn, na.rm = TRUE)
    ggplot(d, aes(oleina_tn)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30,
                     fill = verde, alpha = 0.7, color = "white") +
      geom_density(color = "black", linewidth = 0.7) +
      geom_vline(xintercept = media, color = "red", linewidth = 0.9) +
      labs(title = "Distribución de la oleína por lote",
           subtitle = "La línea roja marca la media",
           x = "Oleína (Tn)", y = "Densidad") +
      tema
  })

  output$txt_oleina_dist <- renderUI({
    d <- datos()
    media <- mean(d$oleina_tn, na.rm = TRUE)
    HTML(paste0(
      "Esta distribución muestra cómo se reparte la producción de oleína entre los lotes: ",
      "la mayoría se concentra alrededor de un valor central y pocos lotes quedan en los ",
      "extremos. La línea roja es la <b>media</b>: en el período seleccionado cada lote ",
      "produjo en promedio <b>", format(round(media, 1), decimal.mark = ","), " Tn</b> de oleína."))
  })

  # ---- Consumo de MP ----

  # Mes seleccionado en el gráfico de MP (NULL = total del período)
  mes_sel_mp <- reactiveVal(NULL)
  observeEvent(input$clic_mp, {
    meses <- sort(unique(datos()$mes))
    idx <- round(input$clic_mp$x)
    if (idx >= 1 && idx <= length(meses)) mes_sel_mp(meses[idx])
  })
  observeEvent(input$ver_total_mp, mes_sel_mp(NULL))
  observeEvent(input$anio, mes_sel_mp(NULL))

  # MP consumida por mes (interactivo, igual que el ácido)
  output$g_mp_barras <- renderPlot({
    d <- datos() %>%
      group_by(mes) %>%
      summarise(mp = sum(mp_tn, na.rm = TRUE), .groups = "drop")
    d$sel <- if (is.null(mes_sel_mp())) TRUE else d$mes == mes_sel_mp()
    ggplot(d, aes(mes, mp, fill = sel)) +
      geom_col() +
      scale_fill_manual(values = c("TRUE" = verde, "FALSE" = "#dcdcdc"), guide = "none") +
      labs(title = "Materia prima consumida por mes (haga clic en una barra)",
           x = "Mes", y = "Materia prima (Tn)") +
      tema +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  })

  output$txt_mp <- renderUI({
    if (is.null(mes_sel_mp())) {
      total <- sum(datos()$mp_tn, na.rm = TRUE)
      HTML(paste0("En el período seleccionado se consumieron en total <b>",
                  format(round(total), big.mark = ".", decimal.mark = ","),
                  " Tn</b> de materia prima. Haga clic en una barra para ver un mes."))
    } else {
      cons <- sum(filter(datos(), mes == mes_sel_mp())$mp_tn, na.rm = TRUE)
      HTML(paste0("En el período <b>", mes_sel_mp(), "</b> se consumieron <b>",
                  format(round(cons), big.mark = ".", decimal.mark = ","),
                  " Tn</b> de materia prima."))
    }
  })

  # ---- Consumo de ácido ----

  # Mes seleccionado en el gráfico de ácido (NULL = total del período)
  mes_sel_acido <- reactiveVal(NULL)
  observeEvent(input$clic_acido, {
    meses <- sort(unique(datos()$mes))
    idx <- round(input$clic_acido$x)
    if (idx >= 1 && idx <= length(meses)) mes_sel_acido(meses[idx])
  })
  observeEvent(input$ver_total_acido, mes_sel_acido(NULL))
  observeEvent(input$anio, mes_sel_acido(NULL))

  # Ácido consumido por mes (interactivo, igual que la oleína del Resumen)
  output$g_acido_barras <- renderPlot({
    d <- datos() %>%
      group_by(mes) %>%
      summarise(acido = sum(acido_tn, na.rm = TRUE), .groups = "drop")
    d$sel <- if (is.null(mes_sel_acido())) TRUE else d$mes == mes_sel_acido()
    ggplot(d, aes(mes, acido, fill = sel)) +
      geom_col() +
      scale_fill_manual(values = c("TRUE" = naranja, "FALSE" = "#dcdcdc"), guide = "none") +
      labs(title = "Ácido consumido por mes (haga clic en una barra)",
           x = "Mes", y = "Ácido (Tn)") +
      tema +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  })

  output$txt_acido <- renderUI({
    if (is.null(mes_sel_acido())) {
      total <- sum(datos()$acido_tn, na.rm = TRUE)
      HTML(paste0("En el período seleccionado se consumieron en total <b>",
                  round(total, 1), " Tn</b> de ácido. Haga clic en una barra para ver un mes."))
    } else {
      cons <- sum(filter(datos(), mes == mes_sel_acido())$acido_tn, na.rm = TRUE)
      HTML(paste0("En el período <b>", mes_sel_acido(), "</b> se consumieron <b>",
                  round(cons, 1), " Tn</b> de ácido."))
    }
  })

  # Tabla de equivalencia entre % en peso y litros de ácido (pasos de 65 L = 0,5%)
  output$tabla_dosis_acido <- renderTable({
    pct <- seq(0.5, 3.5, 0.5)
    tab <- data.frame(matrix((pct / 0.5) * 65, nrow = 1))
    names(tab) <- paste0(format(pct, decimal.mark = ","), "%")
    rownames(tab) <- "Ácido (L)"
    tab
  }, rownames = TRUE, digits = 0, align = "c", width = "100%")

  # Reactores según la dosis de ácido, en % en peso sobre las 24 Tn de MP.
  # La dosis se mueve en pasos de 65 L (= 0,5%); la especificación va de 0,5% a 3,5%.
  output$g_acido_intervalos <- renderPlot({
    d <- datos_reaccion() %>%
      filter(!is.na(acido_lts), acido_lts > 0) %>%
      mutate(pct = round((acido_lts * 1.84 / 1000 / 24 * 100) / 0.5) * 0.5) %>%
      count(pct) %>%
      mutate(valido = pct >= 0.5 & pct <= 3.5)
    # completo el eje de 0 a 3,5% (más cualquier dosis fuera de ese rango) para que no quede cortado
    niveles <- sort(union(seq(0, 3.5, 0.5), d$pct))
    d$pct <- factor(d$pct, levels = niveles)
    ggplot(d, aes(pct, n, fill = valido)) +
      geom_col() +
      geom_text(aes(label = n), vjust = -0.4, size = 3.5) +
      scale_fill_manual(values = c("TRUE" = naranja, "FALSE" = "grey70"),
                        labels = c("TRUE" = "Dentro de especificación",
                                   "FALSE" = "Fuera de especificación"),
                        name = NULL) +
      scale_x_discrete(drop = FALSE, labels = function(x) paste0(x, "%")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(title = "Reactores según la dosis de ácido (% en peso sobre 24 Tn de MP)",
           subtitle = "Especificación permitida: 0,5% a 3,5% en peso. La dosis estándar de 390 L equivale a 3%.",
           x = "Dosis de ácido (% en peso)", y = "Cantidad de reactores") +
      tema +
      theme(legend.position = "top")
  })

  output$txt_acido_intervalos <- renderUI({
    d <- datos_reaccion() %>% filter(!is.na(acido_lts), acido_lts > 0) %>%
      mutate(pct = round((acido_lts * 1.84 / 1000 / 24 * 100) / 0.5) * 0.5)
    en3 <- mean(d$pct == 3) * 100
    fuera <- mean(d$pct < 0.5 | d$pct > 3.5) * 100
    HTML(paste0(
      "<b>Conclusión:</b> la gran mayoría de los reactores, el <b>", round(en3),
      "%</b>, usa la dosis máxima habitual de 3% (390 L), y prácticamente no hay cargas ",
      "fuera de especificación (", format(round(fuera, 2), decimal.mark = ","),
      "%). Trabajar tan pegados al límite superior deja poco margen y es el punto a mirar ",
      "si se busca bajar el consumo de ácido."))
  })

  # ---- Rinde objetivo ----

  # Distribución del rinde, pintando los lotes que cumplen y los que no según el slider
  output$g_objetivo <- renderPlot({
    d <- datos() %>% filter(!is.na(rinde_ole))
    d$cumple <- d$rinde_ole >= input$objetivo
    ggplot(d, aes(rinde_ole, fill = cumple)) +
      geom_histogram(bins = 30, color = "white") +
      geom_vline(xintercept = input$objetivo, color = "red", linewidth = 0.9) +
      annotate("text", x = input$objetivo, y = Inf,
               label = paste0("Objetivo ", input$objetivo, "%"),
               vjust = 1.4, hjust = -0.05, color = "red") +
      scale_fill_manual(values = c("TRUE" = verde, "FALSE" = "grey70"),
                        labels = c("TRUE" = "Cumple el objetivo", "FALSE" = "Por debajo"),
                        name = NULL) +
      labs(title = "Rinde de oleína por lote frente al objetivo",
           x = "Rinde (%)", y = "Cantidad de lotes") +
      tema +
      theme(legend.position = "top")
  })

  output$txt_objetivo <- renderUI({
    d <- datos() %>% filter(!is.na(rinde_ole))
    n <- nrow(d)
    bajo <- sum(d$rinde_ole < input$objetivo)
    media <- mean(d$rinde_ole)
    HTML(paste0(
      "De los <b>", n, "</b> lotes del período, <b>", bajo, "</b> (",
      format(round(bajo / n * 100, 1), decimal.mark = ","),
      "%) quedaron por debajo del objetivo de <b>", input$objetivo, "%</b>. ",
      "El rinde medio del período es <b>",
      format(round(media, 1), decimal.mark = ","), "%</b>."))
  })

  # ---- Semana ideal vs real ----

  # Cuadro de tiempos esperados (ideal) vs reales (promedio)
  output$tabla_tiempos <- renderTable({
    rr <- datos_reaccion()
    ll <- datos()
    prom <- function(x, lo, hi) mean(x[x >= lo & x <= hi], na.rm = TRUE)

    # Fila 1: hora de inicio del primer reactor de cada semana productiva
    anchor <- as.numeric(as.POSIXct("2025-01-06 00:00:00", tz = "UTC"))
    rr2 <- rr %>% filter(!is.na(hora_mp), !is.na(fecha))
    rr2$mp_start <- as.numeric(as.POSIXct(paste(rr2$fecha), tz = "UTC")) + rr2$hora_mp * 3600
    rr2$wk <- floor((rr2$mp_start - anchor) / (7 * 86400))
    prim <- rr2 %>% group_by(wk) %>% summarise(h = hora_mp[which.min(mp_start)], .groups = "drop")
    hr  <- mean(prim$h, na.rm = TRUE)

    mp  <- prom(rr$dur_carga_mp, 0, 240)
    esp <- prom(rr$dur_espera, 0, 480)
    ac  <- prom(rr$dur_carga_acido[rr$acido_lts == 390], 0, 120)
    rx  <- prom(rr$dur_reaccion_h, 4, 20)
    # Fila 6: demora hasta terminar de cargar el decantador, desde el último reactor del lote
    lag_ult <- prom(ll$lag_decant_ult_h, 0, 12)
    dec <- prom(ll$horas_dec, 8, 168)

    hhmm <- function(h) sprintf("%02d:%02d", floor(h), round((h - floor(h)) * 60))
    minu <- function(x) paste0(formatC(x, 1, format = "f", decimal.mark = ","), " min")
    hora <- function(x) paste0(formatC(x, 1, format = "f", decimal.mark = ","), " h")
    dif  <- function(x, u) paste0(ifelse(x >= 0, "+", "-"),
                                  formatC(abs(x), 1, format = "f", decimal.mark = ","), " ", u)

    data.frame(
      "Etapa" = c("1. Hora de inicio de carga de MP",
                  "2. Tiempo de carga de un reactor",
                  "3. Demora hasta iniciar el agregado de ácido",
                  "4. Tiempo de agregado de ácido (390 L)",
                  "5. Tiempo de reacción",
                  "6. Tiempo de descarga de un reactor al decantador (desde el último)",
                  "7. Tiempo de decantación"),
      "Ideal" = c("06:30", "30 min", "90 min", "13 min", "10 h", "30 min", "24 h"),
      "Real (promedio)" = c(hhmm(hr), minu(mp), minu(esp), minu(ac), hora(rx),
                            hora(lag_ult), hora(dec)),
      "Diferencia" = c(dif((hr - 6.5) * 60, "min"),
                       dif(mp - 30, "min"),
                       dif(esp - 90, "min"),
                       dif(ac - 13, "min"),
                       dif((rx - 10) * 60, "min"),
                       dif(lag_ult * 60 - 30, "min"),
                       dif(dec - 24, "h")),
      check.names = FALSE)
  })

  # Turnos: en qué día de la semana se produce
  output$g_turnos_dia <- renderPlot({
    d <- datos() %>% count(dia_semana)
    ggplot(d, aes(dia_semana, n)) +
      geom_col(fill = naranja) +
      labs(title = "Lotes producidos según el día de la semana",
           x = "Día de la semana", y = "Cantidad de lotes") +
      tema
  })

  # Tiempo muerto 1: espera real por lote para agregar ácido vs ideal.
  # La espera objetivo es 90 min por reactor, así que el ideal del lote (línea roja)
  # depende de cuántos reactores tuvo cada mes.
  output$g_muerto_acido <- renderPlot({
    d <- datos_reaccion() %>%
      mutate(mes = format(fecha, "%Y-%m")) %>%
      filter(dur_espera >= 0, dur_espera <= 480) %>%   # saco valores imposibles por errores de carga
      group_by(mes) %>%
      summarise(real = sum(dur_espera, na.rm = TRUE),
                ideal = 90 * n(), .groups = "drop")      # 90 min de espera ideal por reactor
    ggplot(d, aes(mes, real)) +
      geom_col(fill = naranja) +
      geom_errorbar(aes(ymin = ideal, ymax = ideal),
                    color = "red", linewidth = 0.8) +
      labs(title = "Espera para agregar ácido: minutos totales por mes vs ideal",
           subtitle = "Barras: suma de los minutos de espera del mes - Línea roja: suma del ideal (90 min por reactor)",
           x = "Mes", y = "Minutos de espera en el mes") +
      tema +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  })

  output$txt_muerto_acido <- renderUI({
    d <- datos_reaccion() %>% filter(dur_espera >= 0, dur_espera <= 480)
    real_t <- sum(d$dur_espera); ideal_t <- 90 * nrow(d); exc <- real_t - ideal_t
    fmt <- function(x) format(round(x), big.mark = ".", decimal.mark = ",")
    HTML(paste0(
      "<b>Conclusión:</b> en el período, la espera para agregar ácido sumó <b>", fmt(real_t),
      " min</b> reales contra <b>", fmt(ideal_t), " min</b> ideales, <b>", fmt(exc),
      " min</b> de más en total. Es el principal tiempo muerto del proceso y donde más se ",
      "puede ganar si se estandariza el arranque del agregado de ácido."))
  })

  # Tiempo muerto 2: minutos totales de carga del decantador por mes vs ideal.
  # Real por lote: desde que termina el primer reactor hasta que el decantador queda lleno.
  # Ideal por lote: se descarga un reactor por vez (30 min cada uno); el primero marca el
  # arranque, así que faltan (reactores - 1) descargas => (reactores - 1) x 30 min.
  # Sumo esos minutos de todos los lotes de cada mes.
  output$g_muerto_decant <- renderPlot({
    d <- datos() %>%
      filter(carga_decant_h >= 0, carga_decant_h <= 12) %>%   # saco valores imposibles
      group_by(mes) %>%
      summarise(real = sum(carga_decant_h * 60, na.rm = TRUE),
                ideal = sum((n_reactores - 1) * 30, na.rm = TRUE), .groups = "drop")
    ggplot(d, aes(mes, real)) +
      geom_col(fill = naranja) +
      geom_errorbar(aes(ymin = ideal, ymax = ideal),
                    color = "red", linewidth = 0.8) +
      labs(title = "Carga del decantador: minutos totales por mes vs ideal",
           subtitle = "Barras: suma de los minutos reales de carga del mes - Línea roja: suma del ideal (30 min x (reactores - 1) por lote)",
           x = "Mes", y = "Minutos de carga en el mes") +
      tema +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  })

  output$txt_muerto_decant <- renderUI({
    d <- datos() %>% filter(carga_decant_h >= 0, carga_decant_h <= 12) %>%
      group_by(mes) %>%
      summarise(real = sum(carga_decant_h * 60), ideal = sum((n_reactores - 1) * 30),
                .groups = "drop")
    real_t <- sum(d$real); ideal_t <- sum(d$ideal); exc <- real_t - ideal_t
    sobre <- sum(d$real > d$ideal)
    fmt <- function(x) format(round(x), big.mark = ".", decimal.mark = ",")
    HTML(paste0(
      "<b>Conclusión:</b> mes a mes la carga del decantador se mueve por encima y por debajo ",
      "del ideal (", sobre, " de ", nrow(d), " meses por encima), y esos desvíos se compensan ",
      "entre sí. En todo el período sumó <b>", fmt(real_t), " min</b> reales contra <b>",
      fmt(ideal_t), " min</b> ideales, una diferencia neta de apenas <b>", fmt(exc), " min</b>. ",
      "No es acá donde se pierde tiempo de forma sistemática, sino en la espera previa al ",
      "agregado de ácido."))
  })

  # Distribución de la temperatura de inicio del agregado de ácido (1-Reaccion, col G)
  output$g_temp_acido <- renderPlot({
    d <- datos_reaccion() %>% filter(!is.na(temp_ini))
    # saco el 1% de cada extremo para que los errores de carga no deformen la escala
    lim <- quantile(d$temp_ini, c(0.01, 0.99), na.rm = TRUE)
    d <- d %>% filter(temp_ini >= lim[1], temp_ini <= lim[2])
    m <- mean(d$temp_ini); s <- sd(d$temp_ini)
    ggplot(d, aes(temp_ini)) +
      geom_histogram(binwidth = 2, fill = verde, color = "white", alpha = 0.85) +
      geom_vline(xintercept = m, color = "red", linetype = "dashed", linewidth = 0.8) +
      labs(title = "Distribución de la temperatura al iniciar el agregado de ácido",
           subtitle = sprintf("Media %.1f °C - desvío %.1f °C - CV %.0f%%", m, s, 100 * s / m),
           x = "Temperatura de inicio (°C)", y = "Cantidad de reactores") +
      tema
  })

  output$txt_temp <- renderUI({
    d <- datos_reaccion() %>% filter(!is.na(temp_ini))
    lim <- quantile(d$temp_ini, c(0.01, 0.99), na.rm = TRUE)
    d <- d %>% filter(temp_ini >= lim[1], temp_ini <= lim[2])
    m <- mean(d$temp_ini); s <- sd(d$temp_ini)
    HTML(paste0(
      "<b>Conclusión:</b> la temperatura de arranque no es estable: va de <b>",
      round(min(d$temp_ini)), " a ", round(max(d$temp_ini)), " °C</b> alrededor de una media de <b>",
      format(round(m, 1), decimal.mark = ","), " °C</b> (desvío ",
      format(round(s, 1), decimal.mark = ","), " °C, un ", round(100 * s / m),
      "% de variación). Esa dispersión hace que algunos reactores arranquen más fríos y ",
      "tarden más, así que estandarizar la temperatura de inicio ayudaría a reducir los ",
      "tiempos muertos."))
  })

  # Conclusión sobre el día más crítico de la semana
  output$txt_dias <- renderUI({
    d <- datos() %>%
      group_by(dia_semana) %>%
      summarise(lotes = n(), oleina = sum(oleina_tn, na.rm = TRUE), .groups = "drop")
    top <- d[which.max(d$lotes), ]
    pct <- round(top$oleina / sum(d$oleina) * 100, 1)
    dia <- nombre_dia[as.character(top$dia_semana)]
    HTML(paste0(
      "El día más cargado es el <b>", dia, "</b> (", top$lotes,
      " lotes, el ", pct, "% de la oleína de la semana). El martes, miércoles y jueves ",
      "concentran el grueso de la producción, mientras que el lunes y el sábado son de ",
      "arranque y cierre del turno, con mucha menos carga. <b>Conclusión:</b> el ",
      "<b>", dia, "</b> es el día más crítico; una parada o demora ahí ",
      "afecta más lotes que en cualquier otro día, así que conviene no tocarlo y dejar ",
      "el mantenimiento o las paradas para el lunes o el sábado."))
  })

  # Cuántos lotes se hicieron con cierta cantidad de reactores o menos
  output$txt_reactores <- renderUI({
    d <- datos()
    n <- sum(d$n_reactores <= input$max_react, na.rm = TRUE)
    tot <- sum(!is.na(d$n_reactores))
    HTML(paste0("<b>", n, "</b> lotes (", round(n / tot * 100, 1),
                "%) se hicieron con <b>", input$max_react, " reactores o menos</b>, ",
                "sobre ", tot, " lotes del período."))
  })

  output$g_reactores <- renderPlot({
    d <- datos() %>% filter(!is.na(n_reactores)) %>% count(n_reactores)
    d$marcado <- d$n_reactores <= input$max_react
    ggplot(d, aes(factor(n_reactores), n, fill = marcado)) +
      geom_col() +
      geom_text(aes(label = n), vjust = -0.4, size = 4) +
      scale_fill_manual(values = c("TRUE" = naranja, "FALSE" = "grey75"), guide = "none") +
      # dejo un 12% de aire arriba para que la etiqueta del valor más alto no se corte
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(title = "Lotes según cantidad de reactores usados",
           subtitle = "En naranja, los que tienen la cantidad elegida o menos",
           x = "Reactores por lote", y = "Cantidad de lotes") +
      tema
  })

  # ---- Semana ideal (Gantt + resultados esperados) ----

  output$g_gantt_ideal <- renderPlot({
    sem <- generar_semana_ideal(input$hora_lunes)
    d <- sem$segs
    niveles <- c("TK-14", "TK-13", "F", "E", "D", "C", "B", "A")
    d$recurso <- factor(d$recurso, levels = niveles)
    d$etapa <- factor(d$etapa, levels = names(colores_etapa))

    lunes <- as.Date("2026-11-02")
    mon06 <- as.POSIXct(paste(lunes, "06:00"), tz = "UTC")
    sat14 <- as.POSIXct(paste(lunes + 5, "14:00"), tz = "UTC")

    ggplot(d) +
      geom_vline(xintercept = as.numeric(c(mon06, sat14)),
                 linetype = "dashed", color = "grey55") +
      geom_rect(aes(xmin = ini, xmax = fin,
                    ymin = as.numeric(recurso) - 0.4, ymax = as.numeric(recurso) + 0.4,
                    fill = etapa), color = "white", linewidth = 0.2) +
      scale_y_continuous(breaks = 1:8, labels = niveles) +
      scale_fill_manual(values = colores_etapa, name = NULL) +
      scale_x_datetime(date_labels = "%a %H:%M", date_breaks = "12 hours") +
      labs(title = "Semana ideal: 8 lotes, uno nuevo cada 14 h",
           subtitle = "Líneas punteadas: apertura y cierre del turno (lun 06:00 - sáb 14:00)",
           x = NULL, y = "Recurso") +
      tema +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$txt_impacto <- renderUI({
    s <- generar_semana_ideal(input$hora_lunes)$resumen
    base <- generar_semana_ideal(6.5)$resumen
    ult <- s[nrow(s), ]
    corr <- round(as.numeric(difftime(ult$fin_decantacion,
                                      base$fin_decantacion[nrow(base)], units = "hours")), 1)
    lunes <- as.Date("2026-11-02")
    sat14 <- as.POSIXct(paste(lunes + 5, "14:00"), tz = "UTC")
    n_turno <- sum(s$fin_reaccion <= sat14)
    hh <- sprintf("%02d:%02d", floor(input$hora_lunes), round((input$hora_lunes %% 1) * 60))

    HTML(paste0(
      "Arrancando a las <b>", hh, "</b>, el 8º lote termina de reaccionar el <b>",
      fmt_dh(ult$fin_reaccion), "</b> y cierra su decantación el <b>",
      fmt_dh(ult$fin_decantacion), "</b>. ",
      if (corr > 0) paste0("Toda la semana se corre <b>", corr,
                           " h</b> respecto de arrancar a las 06:30. ") else "",
      "Lotes que completan su reacción dentro del turno: <b>", n_turno, " de 8</b>. ",
      "Si el arranque se demora demasiado, el 8º lote ya no entra y la semana baja a 7 lotes (menos oleína)."))
  })

  output$tabla_ideal <- renderTable({
    s <- generar_semana_ideal(input$hora_lunes)$resumen
    data.frame(
      Lote = s$Lote,
      Decantador = s$Decantador,
      "Inicio" = fmt_dh(s$inicio),
      "Fin de reacción" = fmt_dh(s$fin_reaccion),
      "Fin de decantación" = fmt_dh(s$fin_decantacion),
      check.names = FALSE)
  })
}

shinyApp(ui, server)
