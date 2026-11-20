# Tablero de producción de oleína

Trabajo práctico final de **Análisis Inteligente de Datos** (Universidad Austral).
El entregable es una **Shiny App** en R que analiza datos reales de producción de una
planta de oleína desde enero 2025 hasta noviembre 2026. 

---

## 1. El problema y los datos (selección)

La planta produce **oleína** a partir de un proceso por lotes (batch). Cada lote sigue
siempre los mismos pasos:

1. Carga de la materia prima en el reactor (24 Tn por reactor).
2. Agregado de ácido sulfúrico al 98% (medido en litros).
3. Calentamiento con vapor directo y reacción.
4. Trasvase a un decantador, donde el lote se separa por gravedad en tres fases:
   agua, interfase y **oleína** (la fase de interés).

La empresa tiene dos objetivos concretos: **producir más oleína** y **gastar menos
ácido sulfúrico**. Esas dos preguntas son las que guían todo el análisis.

**Fuente de los datos.** Planilla interna de producción de la planta
(`SF-PROD-RE-11`), en `datos_crudos/`. La planilla tiene dos hojas que
se pueden vincular por el número de lote:

- **1-Reaccion:** una fila por cada reactor cargado (3.599 cargas en total). Trae las
  fechas y horas de cada etapa, la temperatura de inicio y fin, los litros de ácido,
  la cantidad de operarios, el reactor y el decantador asignado.
- **2-Decantacion:** una fila por lote (620 lotes). Trae la hora de inicio y fin de la
  decantación, las toneladas y alturas de cada fase (agua, interfase, oleína) y el
  rinde de cada una.

El número de lote codifica el año (`25-xxx` = 2025, `26-xxx` = 2026). La semana
productiva va de **lunes 06:00 a sábado 14:00**, con carga escalonada de los reactores.

---

## 2. Limpieza y ordenamiento (limpieza)

Todo el preprocesamiento está en `codigo/limpieza.R`, que deja los datos listos en
`datos_limpios/produccion.rds`. Los pasos principales:

- **Lectura con encabezados dobles.** Las dos hojas traen una fila de grupo arriba de
  los nombres reales, así que se leen salteando esas filas y se nombran las columnas a
  mano.
- **Filtrado de filas válidas.** Me quedo solo con las filas cuyo lote tiene el formato
  esperado (por ejemplo `26-300`), lo que descarta subtotales y filas en blanco.
- **Fechas y horas.** Las fechas venían como número de serie de Excel y las horas como
  fracción de día, así que se convierten a fecha y a hora real.
- **Duraciones que cruzan la medianoche.** Cuando una etapa termina en un número menor
  al de inicio es porque pasó de un día al otro; en esos casos se suma un día para que
  la duración no dé negativa.
- **Correcciones de carga.** Se detectaron y corrigieron 4 fechas mal cargadas (por
  ejemplo, el lote `26-300` figuraba en octubre cuando era de noviembre). Dentro de un
  mismo lote todos los reactores deberían tener la misma fecha, así que a cada reactor
  se le asigna la fecha más frecuente de su lote.
- **Unidades.** El ácido se pasa de litros a toneladas usando la densidad del ácido
  sulfúrico 98% (1,84 Tn/m3) y las horas de decantación, que estaban guardadas en días,
  se multiplican por 24.
- **Recálculo de la reacción.** La columna de tiempo de reacción original tenía valores
  inconsistentes, así que se recalcula a partir de las horas de inicio y fin.

Con eso se arman tres tablas ordenadas (reacción, decantación y una tabla por lote que
une todo) que son las que consume la aplicación.

---

## 3. Exploración y análisis (exploración)

La app está organizada en pestañas. Cada una responde a una parte del problema:

- **Resumen:** indicadores generales (oleína total, rinde medio, lotes, materia prima y
  ácido) y la evolución mensual de la oleína producida.
- **Oleína:** la oleína producida por lote en el tiempo y cómo se distribuye, con su media.
- **Consumo de MP:** la materia prima consumida por mes.
- **Consumo de ácido:** cuánto ácido se usa por mes y cómo se reparten las dosis por
  reactor respecto del rango permitido.
- **Rinde objetivo:** la distribución del rinde frente a un objetivo que se elige con el
  deslizador, mostrando cuántos lotes quedan por debajo de la meta.
- **Arranque de planta:** un cronograma teórico de cómo debería cargarse una semana de 8 lotes.
- **Semana ideal vs real:** compara los tiempos reales de cada etapa contra la semana
  ideal y mide los tiempos muertos (espera para agregar ácido y carga del decantador),
  además de la distribución de temperaturas y el comportamiento por día.

**Componente dinámico.** La aplicación deja que quien la usa sea parte de la
exploración. Algunos de los controles disponibles:

- Un **selector de año** (2025, 2026 o ambos) que recalcula todos los gráficos e
  indicadores.
- Un **deslizador de rinde objetivo**, para ver cuántos lotes quedan por debajo de la
  meta que uno elija.
- **Gráficos mensuales interactivos**: al hacer clic en el mes de un gráfico de barras,
  los indicadores se actualizan a ese mes; un enlace vuelve al total del período.
- Un **deslizador de hora de arranque** del lunes, que corre todo el cronograma de la
  semana ideal.
- Un **deslizador de cantidad de reactores**, para filtrar los lotes según cuántos
  reactores usaron.

---

## 4. Comunicación

El trabajo se comunica a través de **Shiny**, que aporta reproducibilidad (los datos se
regeneran siempre con el mismo script) y un formato ordenado para presentar los
resultados. Todos los gráficos usan `ggplot2` con un mismo tema y criterio visual
(colores con intención, ejes con sus unidades, barras y áreas desde cero), siguiendo las
buenas prácticas vistas en clase.

---

## Cómo correr el proyecto

1. Tener instalado R y los paquetes: `shiny`, `dplyr`, `ggplot2`, `readxl`, `stringr`.
2. Generar los datos limpios (solo hace falta la primera vez o si cambia la planilla):

   ```r
   source("codigo/limpieza.R")
   ```

3. Levantar la aplicación (parado en la carpeta del proyecto, donde está `app.R`):

   ```r
   shiny::runApp()
   ```

---

## Conclusiones

La pestaña **Arranque de planta** da un punto de partida para organizar las tareas de
mantenimiento: al mostrar cómo debería cargarse una semana ideal, ayuda a ubicar las
paradas en los momentos de menor impacto sobre la producción.

Por otro lado, el análisis confirma que el proceso es **muy variable**, algo que quien
tiene experiencia en planta ya conoce. Aun así, aparecen dos oportunidades concretas de
mejora: **reducir los tiempos de agregado de ácido**, que es el principal tiempo muerto,
y **estandarizar la temperatura** a la que se agrega el ácido, hoy bastante dispersa y
que probablemente explique parte de esas demoras.
