setwd("00.PROGETTO/")
data <- read.csv("realestate_texas.csv")
attach(data)

library(dplyr)
library(moments)
library(ggplot2)
library(RColorBrewer)
library(lubridate)


indici.statistici <- function(x){
    
    indici_posizione <- summary(x)

    indici_dispersione <- c("Intervallo:" = max(x)-min(x),
                            "IQR:" = IQR(x),
                            "dev.st:" = sd(x),
                            "var:" = var(x),
                            "CV:" = sd(x)/mean(x) * 100)

    indici_forma <- c("Asimmetria" = skewness(x),
                      "Curtosi" = kurtosis(x) - 3)
    
    indici <- c(indici_posizione, indici_dispersione, indici_forma)
    return(indici)
}

quantitative_vars <- names(data)[!(names(data) %in% c("city", "year", "month"))]

for (x in quantitative_vars){
    print(x)
    idx <- indici.statistici(data[[x]])
    df_idx <- t(round(as.data.frame(idx), 2))
    
    write.table(x, file = "indici_statistici.csv", append = T)
    write.table(df_idx, file = "indici_statistici.csv", append = T)
}

freq_table_city <- table(city)


# PUNTO 5 ######################################################################

gini.index <- function(x){
    fi <- table(x)/length(x)    # freq relative
    G <- 1 - sum(fi^2)
    
    J <- sum(table(x) != 0)    # numero modalità
    G_norm <- G / ((J-1)/J)
    return(G_norm)
}

volume_cl <- cut(volume, breaks = seq(5,85,5))
freq_table_volume_cl <- table(volume_cl)

ggplot()+
    geom_bar(aes(x = volume_cl),
             fill = "lightblue",
             col = "black")+
    labs(title = "Volumi mensili delle vendite",
         x = "Volume delle vendite (mln $)",
         y = "Numero di mesi")+
    scale_y_continuous(breaks = seq(0,40,5))+
    theme_bw()


print(c("Indice di Gini:", round(gini.index(volume_cl), 3)))


# PUNTO 6 ######################################################################
gini.index(city) # 1, perché le città hanno la stessa frequenza <-> eterogeneità massima


# PUNTO 7 ######################################################################

N <- nrow(data)
p1 <- length(city[city=="Beaumont"]) / N
p2 <- length(month[month==7]) / N
p3 <- nrow(data[data$month==12 & data$year==2012,]) / N
print(c(p1, p2, p3))


# PUNTO 8 ######################################################################

data$mean_price <- (volume / sales) * 1000000


# PUNTO 9 ######################################################################

data$listings_effectiveness <- sales/listings

ggplot(data=data)+
    geom_boxplot(aes(x = city,
                     y = listings_effectiveness,
                     fill = factor(year)))+
    scale_y_continuous(breaks = seq(0.05, 0.35, 0.05))+
        labs(title = "Efficacia delle inserzioni di immobili",
         x = "Città",
         y = "Indice di efficacia",
         fill = "Anno")+
    theme_bw()
    

# PUNTO 10 #####################################################################

# Average number of sales each month
data %>% 
    group_by(mese=month) %>% 
    summarise(numero_medio_vendite=mean(sales)) %>% 
    pivot_wider(names_from = mese, values_from = numero_medio_vendite)

# Top 2 months by number of sales for each city
data %>% 
    group_by(city) %>% 
    top_n(sales, n=2) %>% 
    select(citta = city, year, month, num_vendite = sales)
    
# Average number of sales each year in Tyler
data %>%
    filter(city == "Tyler") %>% 
    group_by(year) %>% 
    summarise(numero_medio_vendite=mean(sales), dev_st=sd(sales))
    


# PUNTO 11 #####################################################################

ggplot(data=data)+
    geom_boxplot(aes(x = city,
                     y = median_price),
                 fill = "lightblue")+
    labs(title = "Prezzo mediano delle case nelle varie città",
         x = "Città",
         y = "Prezzo mediano ($)")+
    theme_bw()+
    theme(panel.grid.major.x = element_blank())


# PUNTO 12 #####################################################################

ggplot(data=data)+
    geom_boxplot(aes(x = city,
                     y = volume,
                     fill = factor(year)))+
    scale_y_continuous(breaks = seq(10,80,5))+
    labs(title = "Volume delle vendite per città negli anni",
         x = "Città",
         y = "Volume delle vendite (mln $)",
         fill = "Anno")+
    theme_bw()+
    theme(panel.grid.major.x = element_blank())


# PUNTO 13 #####################################################################

data_2012 <- filter(data, year==2012)
library(RColorBrewer)

# Barre sovrapposte
ggplot(data = data)+
    geom_col(aes(x = month,
                 y = volume,
                 fill = city),
             col = "black")+
    scale_x_continuous(breaks = 1:12)+
    scale_y_continuous(breaks = seq(0,210,10))+
    facet_wrap(~year,
               nrow = 1)+
    scale_fill_brewer(palette = "Set2")+
    labs(title = "Totale delle vendite nei vari mesi",
         x = "Mese",
         y = "Totale delle vendite (mln $)",
         fill = "Città")+
    theme_bw()+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())


# Barre sovrapposte normalizzate
ggplot(data = data)+
    geom_col(aes(x = month,
                 y = volume,
                 fill = city),
             position = "fill",
             col = "black")+
    scale_x_continuous(breaks = 1:12)+
    scale_y_continuous(breaks = seq(0,1,0.1))+
    facet_wrap(~year,
               nrow = 1)+
    labs(title = "Volume delle vendite per città (%) nei vari mesi",
         x = "Mese",
         y = "Percentuale del totale delle vendite",
         fill = "Città")+
    scale_fill_brewer(palette = "Set2")+
    theme_bw()+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())

#sum(data_2012[data_2012$city=="Tyler",]$volume)


# PUNTO 14 #####################################################################
data$date <- make_date(data$year, data$month)

ggplot(data = data)+
    geom_line(aes(x = date,
                  y = volume,
                  col = city))+
    geom_point(aes(x = date,
                   y = volume,
                   col = city))+
    scale_x_date(date_breaks = "3 months",
                 date_minor_breaks = "month",
                 date_labels = "%Y-%m",
                 expand = c(0, 0))+
    geom_vline(xintercept = date_breaks("1 year")(range(data$date)),
               lwd = 0.5,
               alpha = 0.3)+
    scale_y_continuous(breaks = seq(5,85,5))+
    labs(title = "Andamento delle vendite di immobili",
         x = "Data",
         y = "Volume mensile delle vendite (mln $)",
         col = "Città")+
    theme_bw()

    









