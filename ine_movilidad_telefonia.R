library(tidyverse)
library(readxl)
library(treemapify) # For geom_treemap and friends
library(paletteer) # For color palette 
library(ggtext) # For customize text (used in this script with element_markdown)
library(ragg) # For the device for save the plot
library(extrafont)
windowsFonts("Bahnschrift" = windowsFont("Bahnschrift"))


# https://www.ine.es/experimental/movilidad/experimental_em4.htm
# https://www.ine.es/experimental/movilidad/movilidad_cotidiana_enero_julio_2021.zip
df1 <- read_excel("C:/Users/ANTONIO/Desktop/github_/dfine.xlsx")

unique(df1$`Provincia de residencia`)


df1 <- df1 %>% 
  drop_na(`Provincia de destino`) %>% 
  select(`Provincia de residencia`, `Provincia de destino`, `Flujo origen-destino (nº de personas)` ) %>% 
  filter(`Provincia de residencia` == "Madrid",
         !`Provincia de destino` %in% c("Madrid")) %>% 
  group_by(`Provincia de residencia`,`Provincia de destino`) %>% 
  summarise(Frequency = sum(`Flujo origen-destino (nº de personas)`)) %>% 
  mutate(percentage = Frequency / sum(Frequency)*100) %>% 
  mutate(percentage = round(percentage, 1))

  
df1

extended_palette <- colorRampPalette(paletteer_d("rcartocolor::Prism", 12)) 


df1 %>%
  ggplot(aes(fill = as.factor(`Provincia de destino`), area = percentage, 
             label = glue::glue(" {`Provincia de destino`} \n ({percentage})"))) + 
  geom_treemap(color = "black", size = 1) + 
  geom_treemap_text(family = "Lato Black",fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE) + 
  labs(title = "Flujos de origen - destino en la provincia de Madrid\nel 30 de junio 2021",
       subtitle = "* Omitidos los flujos con destino en la propia provincia y limítrofes",
       caption = "INE | @dataR_amateur") + 
  scale_fill_manual(values = extended_palette(nrow(df1))) + 
  theme(text =element_text(family = "Lato"),
        plot.background = element_rect(fill = "grey95"),
        panel.spacing = unit(2.5, units = "cm"),
        plot.title = element_text(family = "Lato Black",size = rel(2.5), hjust = .5, margin = margin(t = 15,b = 10)),
        plot.subtitle = element_text(family = "Lato Black",size = rel(1.2), hjust = 0, margin = margin(t = 15,b = 10)),
        plot.caption = element_markdown(color = "black", size = rel(1.2), margin = margin(t = 20,b = 10)),
        legend.position = "none"
  )




# barcelona------------

df2 <- read_excel("C:/Users/ANTONIO/Desktop/github_/dfine.xlsx")

unique(df2$`Provincia de residencia`)



df2 <- df2 %>% 
  drop_na(`Provincia de destino`) %>% 
  select(`Provincia de residencia`, `Provincia de destino`, `Flujo origen-destino (nº de personas)` ) %>% 
  filter(`Provincia de residencia` == "Barcelona",
         !`Provincia de destino` %in% c("Barcelona")) %>% 
  group_by(`Provincia de residencia`, `Provincia de destino`) %>% 
  summarise(Frequency = sum(`Flujo origen-destino (nº de personas)`)) %>% 
  mutate(percentage = Frequency / sum(Frequency)*100) %>% 
  mutate(percentage = round(percentage, 1))
df2

extended_palette <- colorRampPalette(paletteer_d("rcartocolor::Prism",12)) 


df2 %>%
  ggplot(aes(fill = as.factor(`Provincia de destino`), area = percentage, 
             label = glue::glue(" {`Provincia de destino`} \n ({percentage})"))) + 
  geom_treemap(color = "black", size = 1) + 
  geom_treemap_text(family = "Lato Black",fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE) + 
  labs(title = "Flujos de origen - destino en la provincia de Barcelona\nel 30 de junio 2021",
       subtitle = "* Omitidos los flujos con destino en la propia provincia y limítrofes",
       caption = "INE | @dataR_amateur") + 
  scale_fill_manual(values = extended_palette(nrow(df2))) + 
  theme(text =element_text(family = "Lato"),
        plot.background = element_rect(fill = "grey95"),
        panel.spacing = unit(2.5, units = "cm"),
        plot.title = element_text(family = "Lato Black",size = rel(2.5), hjust = .5, margin = margin(t = 15,b = 10)),
        plot.subtitle = element_text(family = "Lato Black",size = rel(1.2), hjust = 0, margin = margin(t = 15,b = 10)),
        plot.caption = element_markdown(color = "black", size = rel(1.2), margin = margin(t = 20,b = 10)),
        legend.position = "none"
  )


#sevila-------

df3 <- read_excel("C:/Users/ANTONIO/Desktop/github_/dfine.xlsx")

unique(df3$`Provincia de residencia`)



df3 <- df3 %>% 
  drop_na(`Provincia de destino`) %>% 
  select(`Provincia de residencia`, `Provincia de destino`, `Flujo origen-destino (nº de personas)` ) %>% 
  filter(`Provincia de residencia` == "Sevilla",
         !`Provincia de destino` %in% c("Sevilla")) %>% 
  group_by(`Provincia de residencia`, `Provincia de destino`) %>% 
  summarise(Frequency = sum(`Flujo origen-destino (nº de personas)`)) %>% 
  mutate(percentage = Frequency / sum(Frequency)*100) %>% 
  mutate(percentage = round(percentage, 1))
df3

extended_palette <- colorRampPalette(paletteer_d("rcartocolor::Prism", 12)) 


df3 %>%
  ggplot(aes(fill = as.factor(`Provincia de destino`), area = percentage, 
             label = glue::glue(" {`Provincia de destino`} \n ({percentage})"))) + 
  geom_treemap(color = "black", size = 1) + 
  geom_treemap_text(family = "Lato Black",fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE) + 
  labs(title = "Flujos de origen - destino en la provincia de Sevilla\nel 30 de junio 2021",
       subtitle = "* Omitidos los flujos con destino en la propia provincia y limítrofes",
       caption = "INE | @dataR_amateur") + 
  scale_fill_manual(values = extended_palette(nrow(df3))) + 
  theme(text =element_text(family = "Lato"),
        plot.background = element_rect(fill = "grey95"),
        panel.spacing = unit(2.5, units = "cm"),
        plot.title = element_text(family = "Lato Black",size = rel(2.5), hjust = .5, margin = margin(t = 15,b = 10)),
        plot.subtitle = element_text(family = "Lato Black",size = rel(1.2), hjust = 0, margin = margin(t = 15,b = 10)),
        plot.caption = element_markdown(color = "black", size = rel(1.2), margin = margin(t = 20,b = 10)),
        legend.position = "none"
  )


# valencia --------------------

df4 <- read_excel("C:/Users/ANTONIO/Desktop/github_/dfine.xlsx")

unique(df4$`Provincia de residencia`)



df4 <- df4 %>% 
  drop_na(`Provincia de destino`) %>% 
  select(`Provincia de residencia`, `Provincia de destino`, `Flujo origen-destino (nº de personas)` ) %>% 
  filter(`Provincia de residencia` == "Valencia/Valéncia",
         !`Provincia de destino` %in% c("Valencia/Valéncia")) %>% 
  group_by(`Provincia de residencia`, `Provincia de destino`) %>% 
  summarise(Frequency = sum(`Flujo origen-destino (nº de personas)`)) %>% 
  mutate(percentage = Frequency / sum(Frequency)*100) %>% 
  mutate(percentage = round(percentage, 1))
df4








# unir -------------------


df <- bind_rows(df1, df2, df3, df4)

unique(df$`Provincia de destino`)



extended_palette <- colorRampPalette(paletteer_d("rcartocolor::Prism", 12)) 


# treemap --------------


df %>% 
  mutate(`Provincia de destino`= recode(`Provincia de destino`, 
                                        "Araba/Álava" = "Álava",
                                        "Castellón/Castelló" = "Castellón",
                                        "Santa Cruz de Tenerife" = "SC Tenerife",
                                        "Valencia/Valéncia" = "Valencia"
  )) %>% 
  mutate(`Provincia de residencia`= recode(`Provincia de residencia`, 
                                           "Araba/Álava" = "Álava",
                                           "Castellón/Castelló" = "Castellón",
                                           "Santa Cruz de Tenerife" = "SC Tenerife",
                                           "Valencia/Valéncia" = "Valencia"
  )) %>% 
  ggplot(aes(fill = as.factor(`Provincia de destino`), area = percentage, 
             label = glue::glue(" {`Provincia de destino`} \n ({percentage})"))) + 
  geom_treemap(color = "black", size = 1) + 
  geom_treemap_text(family = "Lato Black",fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE) + 
  facet_wrap(~ `Provincia de residencia`) +
  labs(title = "Flows of origin - destination by\nprovince of June 30, 2021",
       subtitle = "* Omitted flows to the province itself \n** Values in%",
       caption = "INE https://www.ine.es/experimental/movilidad/experimental_em4.htm | @dataR_amateur",
       x = "Destination province",
       y = "Province of residence") + 
  scale_fill_manual(values = extended_palette(nrow(df))) + 
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "grey80"),
        panel.spacing = unit(2.5, units = "cm"),
        strip.background = element_rect(fill = "grey80"),
        strip.text.x = element_text(color = "black", face = "bold", size = 10),
        plot.title = element_text(family = "Lato Black", size = 22, hjust = 0.5),
        plot.subtitle = element_text(family = "Lato Black",size = 12),
        plot.caption = element_text(color = "black", size = 10),
        legend.position = "none"
  )


ggsave("prov_treemap.png", width = 12.5, height = 12, device = agg_png, dpi = 500)




# heatmap -----------------




df %>%  
  mutate(`Provincia de destino`= recode(`Provincia de destino`, 
                                        "Araba/Álava" = "Álava",
                                        "Castellón/Castelló" = "Castellón",
                                        "Santa Cruz de Tenerife" = "SC Tenerife",
                                        "Valencia/Valéncia" = "Valencia"
                                        )) %>% 
  mutate(`Provincia de residencia`= recode(`Provincia de residencia`, 
                                        "Araba/Álava" = "Álava",
                                        "Castellón/Castelló" = "Castellón",
                                        "Santa Cruz de Tenerife" = "SC Tenerife",
                                        "Valencia/Valéncia" = "Valencia"
  )) %>% 
ggplot(aes(`Provincia de destino`, `Provincia de residencia`)) +
  geom_tile(aes(fill = percentage)) + 
  geom_text(aes(label = percentage), size = 3.5, family = "Bahnschrift") +
  labs(title = "Flows of origin - destination by\nprovince of June 30, 2021",
       subtitle = "* Omitted flows to the province itself \n** Values in%",
       caption = "INE https://www.ine.es/experimental/movilidad/experimental_em4.htm | @dataR_amateur",
       x = "Destination province",
       y = "Province of residence") + 
  scale_fill_gradient(low = "white", high = "red") +
  theme(text =element_text(family = "Bahnschrift", face = "bold"),
        plot.background = element_rect(fill = "grey80"),
        panel.background = element_rect(fill = "grey80"),
        panel.grid = element_line(color = "grey80"),
        panel.spacing = unit(2.5, units = "cm"),
        plot.title = element_text(family = "Bahnschrift", size = 22, hjust = 0.5),
        plot.subtitle = element_text(family = "Bahnschrift", size = 12),
        plot.caption = element_text(color = "black", size = 10),
        legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
        axis.text.y = element_text(vjust = 0.5, hjust=1, size = 12)
  ) 

ggsave("prov_heatmap.png", width = 17.5, height = 12, device = agg_png, dpi = 500)



