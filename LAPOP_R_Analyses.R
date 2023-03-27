  
  
  # Polarization in Brazil - An Analysis with LAPOP Data - Ipsos Public Affairs Brazil 
  
  # 1.1 Pre - Calling Required Libraries -----
  library(dplyr)
  library(foreign)
  library(haven)
  library(tidyverse)
  library(CPC)
  library(utils)
  library(stringi)
  library(moments)
  library(factoextra)
  library(srvyr)
  library(ggrepel)
  library(ggtext)
  library(showtext)
  library(rcartocolor)
  library(gridExtra)
  library(cluster)
  
  # 1.2 Pre - Graph Theme ----
  
 source("https://github.com/jdallapola/polarization.BR/raw/main/graphr.R")
  
  # 2. Reading LAPOP Files -----
  
  # lapop_2006 # Unweighted 
  # lapop_2008 # Unweighted 
  # lapop_2010 # Weighted 
  # lapop_2012 # Unweighted 
  # lapop_2014 # Unweighted 
  # lapop_2016 # Weighted 
  # lapop_2018 # Weighted
  
  lapop_2006 <- read.dta("http://datasets.americasbarometer.org/database/files/2138048899brazil_lapop_dims%20final%202007%20v5.dta") 
  
  lapop_2008 <- read.dta("http://datasets.americasbarometer.org/database/files/30541815brazil_lapop_dims_2008_final_data_set_v10.dta")%>%
                mutate(d5 = recode(d5, "Aprova totalmente" = "10", "Desaprova totalmente" = "1"),                   
                       ros4 = recode(ros4, "Concorda muito" = "7", "Discorda muito" = "1"))
  
  lapop_2010 <- read.dta("http://datasets.americasbarometer.org/database/files/7948266051039660950Brazil_LAPOP_AmericasBarometer%202010%20data%20set%20%20approved%20v4.dta")%>%
                mutate(d5 = as.numeric(recode(d5, "Apoia Fortemente" = "10", "Desaprova Fortemente" = "1")),
                       ros4 = as.numeric(recode(ros4, "Concorda Muito" = "7", "Discorda Muito" = "1")),
                       wt.nm = wt/sum(wt))     
  
  lapop_2012 <- read.dta("http://datasets.americasbarometer.org/database/files/54861031Brazil%20LAPOP%20AmericasBarometer%202012%20Rev1_W.dta")
  lapop_2014 <- read_dta("http://datasets.americasbarometer.org/database/files/636339374Brazil%20LAPOP%20AmericasBarometer%202014%20v3.0_W.dta")
  lapop_2016 <- read.dta("http://datasets.americasbarometer.org/database/files/780314464Brazil%20LAPOP%20AmericasBarometer%202017%20V1.0_W.dta")%>%
                mutate(wt.nm = wt/sum(wt))
  lapop_2018 <- read_dta("http://datasets.americasbarometer.org/database/files/Brazil%20LAPOP%20AmericasBarometer%202019%20v1.0_W.dta")%>%
    mutate(wt.nm = wt/sum(wt))
  
  
  lapop_2021 <- read_dta("http://datasets.americasbarometer.org/database/files/BRA_2021_LAPOP_AmericasBarometer_v1.2_w.dta")   
  
  # 3. Kernel Density Plots ----
  
      # D5. E agora, mudando de assunto e pensando nos homossexuais, quanto o(a) sr./sra. aprova ou desaprova que estas pessoas possam candidatar-se para cargos públicos?
      # Escala de 10 pontos
      
      par(mfrow=c(2,4))
      
      d.lgbt.06 <- density(as.numeric(lapop_2006$D5), na.rm=T)%>%
      plot(main = "LGBTs em Cargos Públicos - 2006", ylim = c(0,.25))
      
      d.lgbt.08 <- density(as.numeric(lapop_2008$d5), na.rm=T)%>%
      plot(main = "LGBTs em Cargos Públicos - 2008", ylim = c(0,.25))
      
      d.lgbt.10 <- density(as.numeric(lapop_2010$d5), na.rm=T, weights = lapop_2010$wt.nm)%>% 
      plot(main = "LGBTs em Cargos Públicos - 2010", ylim = c(0,.25))
      
      d.lgbt.12 <- density(as.numeric(lapop_2012$d5), na.rm=T)%>% 
      plot(main = "LGBTs em Cargos Públicos - 2012", ylim = c(0,.25))
      
      d.lgbt.14 <- density(as.numeric(lapop_2014$d5), na.rm=T)%>% 
      plot(main = "LGBTs em Cargos Públicos - 2014", ylim = c(0,.25))
      
      d.lgbt.16_17 <- density(as.numeric(lapop_2016$d5), na.rm=T, weights = lapop_2016$wt.nm)%>% 
      plot(main = "LGBTs em Cargos Públicos - 2016_17", ylim = c(0,.25))
      
      d.lgbt.18_19 <- density(as.numeric(lapop_2018$d5), na.rm=T, weights = lapop_2018$wt.nm)%>% 
      plot(main = "LGBTs em Cargos Públicos - 2018_19", ylim = c(0,.25))
      
      # ROS4. O Estado brasileiro deve implementar políticas públicas para reduzir a desigualdade de  renda entre ricos e pobres. Até que ponto concorda ou discorda desta frase?
      # Escala de 7 pontos
      
      par(mfrow=c(2,3))
      
      d.socioe.08 <- density(as.numeric(lapop_2008$ros4), na.rm=T)%>% 
        plot(main = "Políticas - Desigualdade- 2008", ylim = c(0,.8))
      
      d.socioe.10 <- density(as.numeric(lapop_2010$ros4), na.rm=T, weights = lapop_2010$wt.nm)%>%
        plot(main = "Políticas - Desigualdade- 2010", ylim = c(0,.8))
      
      d.socioe.12 <- density(as.numeric(lapop_2012$ros4), na.rm=T)%>%
        plot(main = "Políticas - Desigualdade- 2012", ylim = c(0,.8))
      
      d.socioe.14 <- density(as.numeric(lapop_2014$ros4), na.rm=T)%>%
        plot(main = "Políticas - Desigualdade- 2014", ylim = c(0,.8))
      
      d.socioe.16_17 <- density(as.numeric(lapop_2016$ros4), na.rm=T, weights = lapop_2016$wt.nm)%>%
        plot(main = "Políticas - Desigualdade- 2016_17", ylim = c(0,.8))
      
      d.socioe.18_19 <- density(as.numeric(lapop_2018$ros4), na.rm=T, weights = lapop_2018$wt.nm)%>%
        plot(main = "Políticas - Desigualdade- 2018_19", ylim = c(0,.8))
      
      
  
  # 4. Average, Kurtosis, and Variance Analyses - METHOD ADOPTED BY ORTELLADO ET AL., 2022 -----
  
    # 4.1 - PAUTA MORAL - D5. E agora, mudando de assunto e pensando nos homossexuais, quanto o(a) sr./sra. aprova ou desaprova que estas pessoas possam candidatar-se para cargos públicos? ----
            # Escala de 10 pontos
  
    
    avg_LGBT <- c("2006" = mean(as.numeric(lapop_2006$D5), na.rm = T), 
                       "2008" = mean(as.numeric(lapop_2008$d5), na.rm = T),
                       "2010" = mean(as.numeric(lapop_2010$d5), na.rm = T),
                       "2012" = mean(as.numeric(lapop_2012$d5), na.rm = T),
                       "2014" = mean(as.numeric(lapop_2014$d5), na.rm = T),
                       "2016" = mean(as.numeric(lapop_2016$d5), na.rm = T),
                       "2018" = mean(as.numeric(lapop_2018$d5), na.rm = T))
    
    krtsis_LGBT <- c("2006" = kurtosis(as.numeric(lapop_2006$D5), na.rm = T), 
                       "2008" = kurtosis(as.numeric(lapop_2008$d5), na.rm = T),
                       "2010" = kurtosis(as.numeric(lapop_2010$d5), na.rm = T),
                       "2012" = kurtosis(as.numeric(lapop_2012$d5), na.rm = T),
                       "2014" = kurtosis(as.numeric(lapop_2014$d5), na.rm = T),
                       "2016" = kurtosis(as.numeric(lapop_2016$d5), na.rm = T),
                       "2018" = kurtosis(as.numeric(lapop_2018$d5), na.rm = T))
    
    var_LGBT <- c("2006" = var(as.numeric(lapop_2006$D5), na.rm = T), 
                       "2008" = var(as.numeric(lapop_2008$d5), na.rm = T),
                       "2010" = var(as.numeric(lapop_2010$d5), na.rm = T),
                       "2012" = var(as.numeric(lapop_2012$d5), na.rm = T),
                       "2014" = var(as.numeric(lapop_2014$d5), na.rm = T),
                       "2016" = var(as.numeric(lapop_2016$d5), na.rm = T),
                       "2018" = var(as.numeric(lapop_2018$d5), na.rm = T))
    
    med_LGBT <- c("2006" = median(as.numeric(lapop_2006$D5), na.rm = T), 
                  "2008" = median(as.numeric(lapop_2008$d5), na.rm = T),
                  "2010" = median(as.numeric(lapop_2010$d5), na.rm = T),
                  "2012" = median(as.numeric(lapop_2012$d5), na.rm = T),
                  "2014" = median(as.numeric(lapop_2014$d5), na.rm = T),
                  "2016" = median(as.numeric(lapop_2016$d5), na.rm = T),
                  "2018" = median(as.numeric(lapop_2018$d5), na.rm = T))
    
    df.lgbt.1 <- data.frame(year = as.numeric(names(krtsis_LGBT)), 
                          average = avg_LGBT,
                          kurtosis = krtsis_LGBT,
                          variance = var_LGBT, 
                          median = med_LGBT)
  
  df.lgbt.1%>%
    gather(variable, value, -year)%>%
    ggplot(aes(x = as.Date(paste0(year, "-01-01")), y = value, color = variable, group = variable))+
    geom_line(size = 1) +
    geom_point()+
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(title = "LGBT Trends over Time",
         x = "Year",
         y = "Values",
         color = "Variables")
  
    # 4.2 - PAUTA ECONÔMICA - ROS4. O Estado brasileiro deve implementar políticas públicas para reduzir a desigualdade de renda entre ricos e pobres. Até que ponto concorda ou discorda desta frase? ----
    
    
      avg_socioe <- c("2006" = NA, 
                        "2008" = mean(as.numeric(lapop_2008$ros4), na.rm = T),
                        "2010" = mean(as.numeric(lapop_2010$ros4), na.rm = T),
                        "2012" = mean(as.numeric(lapop_2012$ros4), na.rm = T),
                        "2014" = mean(as.numeric(lapop_2014$ros4), na.rm = T),
                        "2016" = mean(as.numeric(lapop_2016$ros4), na.rm = T),
                        "2018" = mean(as.numeric(lapop_2018$ros4), na.rm = T))
      
      krtsis_socioe <- c("2006" = NA, 
                       "2008" = kurtosis(as.numeric(lapop_2008$ros4), na.rm = T),
                       "2010" = kurtosis(as.numeric(lapop_2010$ros4), na.rm = T),
                       "2012" = kurtosis(as.numeric(lapop_2012$ros4), na.rm = T),
                       "2014" = kurtosis(as.numeric(lapop_2014$ros4), na.rm = T),
                       "2016" = kurtosis(as.numeric(lapop_2016$ros4), na.rm = T),
                       "2018" = kurtosis(as.numeric(lapop_2018$ros4), na.rm = T))
      
      var_socioe <- c("2006" = NA, 
                      "2008" = var(as.numeric(lapop_2008$ros4), na.rm = T),
                      "2010" = var(as.numeric(lapop_2010$ros4), na.rm = T),
                      "2012" = var(as.numeric(lapop_2012$ros4), na.rm = T),
                      "2014" = var(as.numeric(lapop_2014$ros4), na.rm = T),
                      "2016" = var(as.numeric(lapop_2016$ros4), na.rm = T),
                      "2018" = var(as.numeric(lapop_2018$ros4), na.rm = T))
      
      
      df.socioe <- data.frame(year = as.numeric(names(krtsis_socioe)),
                              average = avg_socioe,
                              kurtosis = krtsis_socioe,
                              variance = var_socioe)%>%
                    gather(variable, value, -year)
      
      ggplot(df.socioe, aes(x = as.Date(paste0(year, "-01-01")), y = value, color = variable, group = variable)) +
        geom_line(size = 1) +
        geom_point()+
        scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
        scale_y_continuous(limits = c(1, 10), expand = c(0, 0)) +
        labs(title = "Kurtosis of Socioeconomic Data over Time",
             x = "Year",
             y = "Kurtosis") +
        theme_minimal()
      
    
    
  # 5. Cluster-Polarization Coefficient - METHOD ADOPTED BY MEHLAHAFF, 2020 ----
    # 5.1 Using Elbow graph method to determine optimal number of clusters ("3" is the optimal number for all waves) ----
    
          
        
        
        # insert data.frame to test: 
          df.to.test <- lapop_2008$d5
          df.to.test <- lapop_2010$d5
          df.to.test <- lapop_2012$d5
          df.to.test <- lapop_2014$d5
          df.to.test <- lapop_2016$d5
          df.to.test <- lapop_2018$d5
          
          df.to.test <- lapop_2008$ros4
          df.to.test <- lapop_2010$ros4
          df.to.test <- lapop_2012$ros4
          df.to.test <- lapop_2014$ros4
          df.to.test <- lapop_2016$ros4
          df.to.test <- lapop_2018$ros4
   
        
          
          
          fviz_nbclust(lapop_2008, 
                       kmeans, 
                       k.max = 7,
                       method = "wss") # Optimal number of clusters = 3 
     
      
        # 5.2 Calculating CPC Coefficient in LAPOP Data for LGBT. PAUTA MORAL - D5. E agora, mudando de assunto e pensando nos homossexuais, quanto o(a) sr./sra. aprova ou desaprova que estas pessoas possam candidatar-se para cargos públicos? Escala de 10 pontos -----
  
          CPC_LGBT <- c("2006" = CPC(data = as.numeric(lapop_2006$D5), k = 2, adjust = T, type = "kmeans"),
            "2008" = CPC(data = as.numeric(lapop_2008$d5), k = 2, adjust = T, type = "kmeans"),
            "2010" = CPC(data = as.numeric(lapop_2010$d5), k = 2, adjust = T, type = "kmeans"),
            "2012" = CPC(data = as.numeric(lapop_2012$d5), k = 2, adjust = T, type = "kmeans"),
            "2014" = CPC(data = as.numeric(lapop_2014$d5), k = 2, adjust = T, type = "kmeans"),
            "2016" = CPC(data = as.numeric(lapop_2016$d5), k = 2, adjust = T, type = "kmeans"),
            "2018" = CPC(data = as.numeric(lapop_2018$d5), k = 3, adjust = T, type = "kmeans"))
            
          df.cpc_lgbt <- data.frame(year = as.numeric(names(CPC_LGBT)), lgbt = (CPC_LGBT))%>%
            gather(variable, value, -year)
  
        # 5.3 Calculating CPC Coefficient in LAPOP Data for Socioeconomic Questions ----
          CPC_socioe <- c("2006" = NA,
                        "2008" = CPC(data = as.numeric(lapop_2008$ros4), k = 2, adjust = T, type = "kmeans"),
                        "2010" = CPC(data = as.numeric(lapop_2010$ros4), k = 2, adjust = T, type = "kmeans"),
                        "2012" = CPC(data = as.numeric(lapop_2012$ros4), k = 2, adjust = T, type = "kmeans"),
                        "2014" = CPC(data = as.numeric(lapop_2014$ros4), k = 2, adjust = T, type = "kmeans"),
                        "2016" = CPC(data = as.numeric(lapop_2016$ros4), k = 2, adjust = T, type = "kmeans"),
                        "2018" = CPC(data = as.numeric(lapop_2018$ros4), k = 2, adjust = T, type = "kmeans"))
          
          df.cpc_socioe <- data.frame(year = as.numeric(names(CPC_socioe)), socio = (CPC_socioe))%>%
            gather(variable, value, -year)
          
         

          gg.cpc <- rbind(df.cpc_lgbt, df.cpc_socioe)%>%
            ggplot(aes(x = as.Date(paste0(year, "-01-01")), y = value, color = variable, group = variable)) +
            geom_line(linewidth = 1) +
            geom_point()+
            scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
            scale_y_continuous(limits = c(.50, 1), expand = c(0, 0)) +
            labs(title = "CPC Data over Time",
                 x = "Year",
                 y = "CPC") +
            theme_minimal()
        
          gg.cpc
          
          CPC
          
          
          # 5.3 Calculating CPC Coefficient in LAPOP Data for Socioeconomic Questions ----
          CPC_d1 <- c("2006" = NA,
                      "2008" = CPC(data = as.numeric(lapop_2008$d1), k = 2, adjust = T, type = "kmeans"),
                      "2010" = CPC(data = as.numeric(lapop_2010$d1), k = 2, adjust = T, type = "kmeans"),
                      "2012" = CPC(data = as.numeric(lapop_2012$d1), k = 2, adjust = T, type = "kmeans"),
                      "2014" = CPC(data = as.numeric(lapop_2014$d1), k = 2, adjust = T, type = "kmeans"),
                      "2016" = CPC(data = as.numeric(lapop_2016$d1), k = 2, adjust = T, type = "kmeans"),
                      "2018" = CPC(data = as.numeric(lapop_2018$d1), k = 2, adjust = T, type = "kmeans"))
          
          df.cpc_d1 <- data.frame(year = as.numeric(names(CPC_d1)), democ = (CPC_d1))%>%
            gather(variable, value, -year)
          
          
          rbind(df.cpc_lgbt, df.cpc_socioe, df.cpc_d1)%>%
            ggplot(aes(x = as.Date(paste0(year, "-01-01")), y = value, color = variable, group = variable)) +
            geom_line(linewidth = 1) +
            geom_point()+
            scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
            scale_y_continuous(limits = c(.50, 1), expand = c(0, 0)) +
            labs(title = "CPC Data over Time",
                 x = "Year",
                 y = "CPC") +
            theme_minimal()
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          lapop_2016%>%colnames()%>%sort()
          
        
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
            par(mfrow=c(2,4))
          density(as.numeric(lapop_2006$D1), na.rm=T)%>%plot()
          density(as.numeric(lapop_2008$d1), na.rm=T)%>%plot()
          density(as.numeric(lapop_2010$d1), na.rm=T)%>%plot()
          density(as.numeric(lapop_2012$d1), na.rm=T)%>%plot()
          density(as.numeric(lapop_2014$d1), na.rm=T)%>%plot()
          density(as.numeric(lapop_2016$d1), na.rm=T)%>%plot()
          density(as.numeric(lapop_2018$d1), na.rm=T)%>%plot()
          
          par(mfrow=c(2,4))
          density(as.numeric(lapop_2006$D2), na.rm=T)%>%plot()
          density(as.numeric(lapop_2008$d2), na.rm=T)%>%plot()
          density(as.numeric(lapop_2010$d2), na.rm=T)%>%plot()
          density(as.numeric(lapop_2012$d2), na.rm=T)%>%plot()
          density(as.numeric(lapop_2014$d2), na.rm=T)%>%plot()
          density(as.numeric(lapop_2016$d2), na.rm=T)%>%plot()
          density(as.numeric(lapop_2018$d2), na.rm=T)%>%plot()
          
          
          
          par(mfrow=c(2,4))
          density(as.numeric(lapop_2006$D3), na.rm=T)%>%plot()
          density(as.numeric(lapop_2008$d3), na.rm=T)%>%plot()
          density(as.numeric(lapop_2010$d3), na.rm=T)%>%plot()
          density(as.numeric(lapop_2012$d3), na.rm=T)%>%plot()
          density(as.numeric(lapop_2014$d3), na.rm=T)%>%plot()
          density(as.numeric(lapop_2016$d3), na.rm=T)%>%plot()
          density(as.numeric(lapop_2018$d3), na.rm=T)%>%plot()
          
          par(mfrow=c(2,4))
          density(as.numeric(lapop_2006$D4), na.rm=T)%>%plot()
          density(as.numeric(lapop_2008$d4), na.rm=T)%>%plot()
          density(as.numeric(lapop_2010$d4), na.rm=T)%>%plot()
          density(as.numeric(lapop_2012$d4), na.rm=T)%>%plot()
          density(as.numeric(lapop_2014$d4), na.rm=T)%>%plot()
          density(as.numeric(lapop_2016$d4), na.rm=T)%>%plot()
          density(as.numeric(lapop_2018$d4), na.rm=T)%>%plot()
          
          par(mfrow=c(2,4))
          density(as.numeric(lapop_2006$ING4), na.rm=T)%>%plot()
          density(as.numeric(lapop_2008$ing4), na.rm=T)%>%plot()
          density(as.numeric(lapop_2010$ing4), na.rm=T)%>%plot()
          density(as.numeric(lapop_2012$ing4), na.rm=T)%>%plot()
          density(as.numeric(lapop_2014$ing4), na.rm=T)%>%plot()
          density(as.numeric(lapop_2016$ing4), na.rm=T)%>%plot()
          density(as.numeric(lapop_2018$ing4), na.rm=T)%>%plot()
   
          
  
          
          
          
          
          
          
          
          
          
          
          
          L <- list(lapop_2008,lapop_2012,lapop_2014,lapop_2016,lapop_2018)
          Reduce(intersect, lapply(L, names))
         