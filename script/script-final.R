library(tidyverse)
library(lubridate)
library(sf)
library(readxl)


piramide <- read_excel("./data/Poblacion.xlsx", sheet = "Piramide Poblacional")

sum(piramide$Total[piramide$Rango_etario=="0-4" |
                     piramide$Rango_etario=="5-9" |
                     piramide$Rango_etario=="10-14" |
                     piramide$Rango_etario=="15-19" |
                     piramide$Rango_etario=="20-24" |
                     piramide$Rango_etario=="25-29" |
                     piramide$Rango_etario=="30-34" |
                     piramide$Rango_etario=="35-39"])


poblacion_total1 <- sum(piramide$Total)

piramide <- piramide %>%
  mutate(proporcion = Total/poblacion_total1,
         proporcion_varones = Varones/sum(Varones),
         proporcion_mujeres = Mujeres/sum(Mujeres),
  )



piramide_1 <- piramide %>%
  dplyr::select(Rango_etario, Varones, Mujeres) %>%
  pivot_longer(! "Rango_etario", names_to = "Sexo", values_to = "Poblacion_sexo") %>%
  mutate(Poblacion = poblacion_total1,
         proporcion = (Poblacion_sexo/Poblacion)*100,
         porcentaje_total = ifelse(
           Sexo == "Varones", -proporcion, proporcion
         ),
         poblacion_total_1 = ifelse(Sexo=="Varones", -Poblacion, Poblacion)
  )

# Transformo a factor Rango_etario para que me ordene las escalas
lvl <- unique(piramide_1$Rango_etario)
piramide_1$Rango_etario <- factor(piramide_1$Rango_etario, levels=lvl)

ggplot(data = piramide_1, aes(Rango_etario, porcentaje_total, fill=Sexo)) +
  geom_col(position = "stack", alpha=0.8) +
  coord_flip() +
  scale_y_continuous("Porcentaje de la población total", labels = abs) +
  scale_x_discrete("") +
  scale_fill_manual(values = c("#E3226E", "#2942B4")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#99A3A4", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"),
        axis.line = element_line(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic", size = 10.5)) +
  labs(title = "Piramide de población de la Provincia de Misiones, año 2020",
       subtitle = "Proyecciones poblacionales INDEC",
       caption = "Fuente: MAyP Misiones en base a INDEC")

ggsave("./plot/piramide_poblacional.pdf", dpi =300)
ggsave("./plot/piramide_poblacional.png", dpi = 300)

