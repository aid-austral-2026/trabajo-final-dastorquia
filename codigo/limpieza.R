# limpieza.R
# Lectura y limpieza de la planilla de producción (hojas 1-Reaccion y 2-Decantacion).
# Genera las tablas ordenadas que después usa la app.
# Materia: Análisis Inteligente de Datos

library(readxl)
library(dplyr)
library(stringr)

# Densidad del ácido sulfúrico 98% para pasar litros a toneladas
densidad_acido <- 1.84   # Tn/m3

ruta <- "datos_limpios/SF-PROD-RE-11 - Planilla de Producción.xlsx"

# Las dos hojas traen una fila de grupo arriba y los nombres reales en la segunda fila,
# así que leo sin encabezado (saltando esas 2 filas) y pongo los nombres a mano.
reaccion_cols <- c("fecha","tk_origen","lote","reactor","mp_ini","mp_fin",
                   "temp_ini","acido_lts","ac_ini","ac_fin","rx_ini","rx_fin",
                   "temp_fin","t_reaccion","tk_dec","operarios","niv_ini",
                   "niv_fin","vapor_acido")

reaccion_raw <- read_excel(ruta, sheet = "1-Reaccion", skip = 2,
                           col_names = FALSE, col_types = "text")
names(reaccion_raw)[seq_along(reaccion_cols)] <- reaccion_cols

decant_cols <- c("f_ini","lote","h_ini","tk_dec","reactores","f_fin","h_fin",
                 "horas_dec","alt_luz","alt_agua","agua_tn","tk_agua","alt_int",
                 "int_tn","tk_int","flag","alt_ole","ole_tn","tk_ole","operario",
                 "rinde_agua","rinde_int","rinde_ole")

decant_raw <- read_excel(ruta, sheet = "2-Decantacion", skip = 2,
                         col_names = FALSE, col_types = "text")
names(decant_raw)[seq_along(decant_cols)] <- decant_cols

# Función corta para pasar texto a número sin que moleste con los avisos
a_num <- function(x) suppressWarnings(as.numeric(x))

# ---- Hoja de Reacción ----------------------------------------------------

reaccion <- reaccion_raw %>%
  # me quedo solo con las filas que tienen un lote válido (ej: 25-001, 26-300)
  filter(str_detect(lote, "^\\d{2}-\\d{3}$")) %>%
  mutate(
    across(c(mp_ini, mp_fin, ac_ini, ac_fin, rx_ini, rx_fin,
             temp_ini, temp_fin, acido_lts, niv_ini, niv_fin, vapor_acido),
           a_num),
    fecha = as.Date(a_num(fecha), origin = "1899-12-30"),
    anio  = paste0("20", str_sub(lote, 1, 2))
  )

# Arreglo de fechas:
# dentro de un mismo lote todos los reactores deberían tener la misma fecha,
# así que le asigno a cada reactor la fecha más frecuente de su lote.
reaccion <- reaccion %>%
  group_by(lote) %>%
  mutate(fecha = {
    tt <- table(fecha)
    as.Date(names(tt)[which.max(tt)])
  }) %>%
  ungroup()

# El lote 26-300 quedó cargado como 02-10-2026 cuando en realidad es 02-11-2026.
reaccion <- reaccion %>%
  mutate(fecha = if_else(lote == "26-300", as.Date("2026-11-02"), fecha))

# Duraciones. Las horas vienen como fracción de día (0,5 = 12:00).
# Si el fin da antes que el inicio es porque cruzó la medianoche, le sumo un día.
dur_min <- function(ini, fin) {
  d <- fin - ini
  d[!is.na(d) & d < 0] <- d[!is.na(d) & d < 0] + 1
  d * 24 * 60
}

# Nombres de los días para armar el día de la semana (0 = domingo)
dias_semana <- c("Dom", "Lun", "Mar", "Mié", "Jue", "Vie", "Sáb")
orden_dias  <- c("Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom")

reaccion <- reaccion %>%
  mutate(
    acido_tn        = acido_lts * densidad_acido / 1000,   # litros -> toneladas
    dur_carga_mp    = dur_min(mp_ini, mp_fin),
    dur_espera      = dur_min(mp_fin, ac_ini),             # espera entre cargar MP y empezar con el ácido
    dur_carga_acido = dur_min(ac_ini, ac_fin),
    dur_reaccion_h  = dur_min(rx_ini, rx_fin) / 60,        # recalculada, no uso t_reaccion (venía con errores)
    dia_semana      = factor(dias_semana[as.POSIXlt(fecha)$wday + 1], levels = orden_dias),
    hora_mp         = mp_ini * 24,                         # hora de inicio de carga (0 a 24)
    # fecha-hora en que termina la reacción (corrijo si cruzó la medianoche)
    rx_fin_dt       = as.POSIXct(paste(fecha), tz = "UTC") + rx_fin * 86400,
    rx_fin_dt       = if_else(!is.na(rx_ini) & !is.na(rx_fin) & rx_fin < rx_ini,
                              rx_fin_dt + 86400, rx_fin_dt)
  )

# ---- Hoja de Decantación -------------------------------------------------

decantacion <- decant_raw %>%
  filter(str_detect(lote, "^\\d{2}-\\d{3}$")) %>%
  mutate(
    across(c(h_ini, h_fin, horas_dec, agua_tn, int_tn, ole_tn,
             rinde_agua, rinde_int, rinde_ole), a_num),
    fecha = as.Date(a_num(f_ini), origin = "1899-12-30"),
    anio  = paste0("20", str_sub(lote, 1, 2)),
    horas_dec = horas_dec * 24,                 # venía guardado en días
    n_reactores = str_count(reactores, "[A-Z]"), # cuento las letras de reactores
    inicio_dt = as.POSIXct(paste(fecha), tz = "UTC") + h_ini * 86400  # fecha-hora de inicio de carga (col C)
  ) %>%
  select(lote, anio, fecha, decantador = tk_dec, reactores, n_reactores,
         horas_dec, agua_tn, int_tn, ole_tn, rinde_agua, rinde_int, rinde_ole,
         operario, inicio_dt)

# ---- Tabla por lote (une reacción con decantación) -----------------------
# Es la tabla principal para los análisis de producción y de ácido.

acido_por_lote <- reaccion %>%
  group_by(lote) %>%
  summarise(
    n_reactores = n(),
    acido_tn    = sum(acido_tn, na.rm = TRUE),
    mp_tn       = 24 * n(),              # 24 Tn de materia prima por reactor
    .groups = "drop"
  )

# Fin de reacción del primer y del último reactor de cada lote (col L de 1-Reaccion).
# El primero marca cuándo se podría empezar a cargar el decantador y el último cuándo
# terminó de reaccionar todo el lote.
cierre_lote <- reaccion %>%
  group_by(lote) %>%
  summarise(
    ini_reaccion_dt = suppressWarnings(min(rx_fin_dt, na.rm = TRUE)),  # primer reactor
    fin_reaccion_dt = suppressWarnings(max(rx_fin_dt, na.rm = TRUE)),  # último reactor
    .groups = "drop")

lotes <- decantacion %>%
  select(lote, anio, fecha, decantador, oleina_tn = ole_tn,
         agua_tn, interfase_tn = int_tn, rinde_ole, horas_dec, inicio_dt) %>%
  left_join(acido_por_lote, by = "lote") %>%
  left_join(cierre_lote, by = "lote") %>%
  mutate(
    mes = format(fecha, "%Y-%m"),
    dia_semana = factor(dias_semana[as.POSIXlt(fecha)$wday + 1], levels = orden_dias),
    # demora hasta terminar de cargar el decantador: horas entre el fin de reacción del
    # último reactor del lote (1-Reaccion col L) y el fin de carga del decantador
    # (2-Decantacion col C: hora en que el decantador queda lleno con todo el lote)
    lag_decant_ult_h = as.numeric(difftime(inicio_dt, fin_reaccion_dt, units = "hours")),
    # tiempo total de carga del decantador: desde que termina el primer reactor del lote
    # hasta que el decantador queda lleno (col C). Abarca la carga de todos los reactores.
    carga_decant_h = as.numeric(difftime(inicio_dt, ini_reaccion_dt, units = "hours"))
  ) %>%
  arrange(fecha)

# ---- Guardado ------------------------------------------------------------

produccion <- list(reaccion = reaccion, decantacion = decantacion, lotes = lotes)
saveRDS(produccion, "datos_limpios/produccion.rds")

# dejo también la tabla por lote en csv por si la quiero mirar aparte
write.csv(lotes, "datos_limpios/lotes.csv", row.names = FALSE)

cat("Listo. Lotes:", nrow(lotes),
    "| Reactores:", nrow(reaccion),
    "| Rango:", format(min(lotes$fecha)), "a", format(max(lotes$fecha)), "\n")
