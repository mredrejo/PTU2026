library(plyr)
encuestas <- read.csv2("encuestas/data_survey.csv")
preguntas<- encuestas[1:11,2:3]
en2023 <- read.csv("encuestas/survey_strbio2023_respuestas_por_estudiante.csv")
en2023 <- cbind(c("2023"),preguntas,en2023[,-1])
colnames(en2023)[1] <- "YEAR"
colnames(en2023)[4:28] <- colnames(encuestas)[4:28]
encuestas <- rbind.fill(encuestas,en2023)

en2024 <- read.csv("encuestas/survey_strbio2024_respuestas_anonimas.csv")
en2024 <- cbind(c("2024"),preguntas,en2024[,-1])
colnames(en2024)[1] <- "YEAR"
colnames(en2024)[4:30] <- colnames(encuestas)[4:30]
encuestas <- rbind.fill(encuestas,en2024)

en2025 <- read.csv("encuestas/survey_strbio2025_respuestas_anonimas.csv")
en2025 <- cbind(c("2025"),preguntas,en2025[,-1])
colnames(en2025)[1] <- "YEAR"
colnames(en2025)[4:28] <- colnames(encuestas)[4:28]
encuestas <- rbind.fill(encuestas,en2025)

en2026 <- read.csv("encuestas/survey_strbio2026_respuestas_anonimas.csv")
en2026 <- cbind(c("2026"),preguntas,en2026[,-1])
colnames(en2026)[1] <- "YEAR"
colnames(en2026)[4:27] <- colnames(encuestas)[4:27]
encuestas <- rbind.fill(encuestas,en2026)


library(dplyr)
encuestas[,4:33] <- encuestas[,4:33] %>% mutate(across(everything(), ~ as.integer(sub("-.*", "", .x))))
write.csv2(encuestas,"encuestas/strbio_surveys.csv",quote=F,row.names = F)


# Leer datos
df <- read.csv("encuestas/strbio_surveys.csv",
               sep = ";",
               check.names = FALSE,
               stringsAsFactors = FALSE)

# Columnas de respuestas (todo excepto las 3 primeras)
resp_cols <- names(df)[-(1:3)]

# Separar por año y transformar cada bloque
res <- do.call(rbind, lapply(split(df, df$YEAR), function(d) {
  
  # Transponer solo las respuestas
  m <- t(as.matrix(d[, resp_cols]))
  
  # Quitar filas vacías (encuestas sin datos)
  m <- m[rowSums(!is.na(m) & m != "") > 0, , drop = FALSE]
  
  # Convertir a data.frame
  out <- data.frame(YEAR = d$YEAR[1], m,
                    check.names = FALSE,
                    stringsAsFactors = FALSE)
  
  # Poner como nombres de columna las preguntas
  names(out)[-1] <- d$Quesion   # usa d$X si prefieres el texto largo
  out
}))

rownames(res) <- NULL

preguntas <- c("a"="Nivel de dificultad de la asignatura",
               "b"="Interés general de la asignatura",
               "c"="Carga de trabajo",
               "d"="Interés introducción estructura proteínas",
               "e"="Materiales del curso",
               "f"="Blance teoría/práctica",
               "g"="Tiempo de trabajo práctico en clase",
               "h"="Preferiría aula invertida",
               "i"="Preferiría más teoría en clase",
               "j"="Evaluación del trabajo de otros grupos",
               "k"="Comentarios de evaluación recibidos"
               )




library(ggstats)
gglikert(res,
         2:12,
         variable_labels = preguntas,
         facet_cols = vars(YEAR),
         labels_size = 4
)

res[ , 2:5] <-
  lapply(res[ , 2:5], function(x) {
    factor(x,
           levels = c(1:5),
           labels = c("Muy poco", "Poco", "Intermedio", "Bastante", "Mucho"),
           ordered = TRUE)
  })
library("RColorBrewer")   
g1 <- gglikert(res,
         2:5,
         variable_labels = preguntas,
         facet_cols = vars(YEAR), totals_hjust = -.05, labels_hide_below = 0.15,
         labels_size = 4) + scale_fill_brewer(palette = "RdYlBu", direction=-1)+
  theme(axis.text.y=element_text(size=12)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

res[ , 6:8] <-
  lapply(res[ , 6:8], function(x) {
    factor(x,
           levels = c(1:5),
           labels = c("Muy malo", "Malo", "Intermedio", "Bueno", "Muy bueno"),
           ordered = TRUE)
  })
g2 <- gglikert(res,
         6:8,
         variable_labels = preguntas,
         facet_cols = vars(YEAR), totals_hjust = -.05, labels_hide_below = 0.15,
         labels_size = 4) +
  theme(axis.text.y=element_text(size=12)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))




res[ , 11:12] <-
  lapply(res[ , 11:12], function(x) {
    factor(x,
           levels = c(1:5),
           labels = c("Inútil", "Poco útil", "Regular", "Útil", "Muy útil"),
           ordered = TRUE)
  })

g3 <- gglikert(res,
         11:12,
         variable_labels = preguntas, totals_hjust = -.05, labels_hide_below = 0.15,
         facet_cols = vars(YEAR),
         labels_size = 4) + scale_fill_brewer(palette = "RdYlBu") +
  theme(axis.text.y=element_text(size=12)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggpubr)
ggarrange(g1,g2,g3, nrow=3,labels="AUTO", heights=c(3.5,3,2.5))
