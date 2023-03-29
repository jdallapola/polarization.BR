  
  
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
  library(Hmisc)
  
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


  
  lapop_2006 <- read.dta("http://datasets.americasbarometer.org/database/files/2138048899brazil_lapop_dims%20final%202007%20v5.dta")%>% #----
    rename_all(tolower)%>%
    mutate(gen.06 = recode(as.numeric(gen3), `1` = 5, `2` = 4, `3` = 2, `4` = 1, `5` = 3),
           econ.06 = recode(as.numeric(pr6),`1` = 1, `2` = 2, `3` = 4, `4` = 5, `5` = 3),
           lgbt.pol = impute(as.numeric(d5), mean),
           gen.06 = impute(as.numeric(gen.06), mean),
           econ.06 = impute(as.numeric(econ.06), mean),
           democ = impute(as.numeric(ing4), mean),
           golpe.crim = impute(as.numeric(jc10), mean),
           golpe.corrup = impute(as.numeric(jc13), mean),
           golpe.congr = impute(as.numeric(jc15), mean),
           year = 2006) 
    
  lapop_2008 <- read.dta("http://datasets.americasbarometer.org/database/files/30541815brazil_lapop_dims_2008_final_data_set_v10.dta")%>% #----
    mutate(gen = recode(as.numeric(vb50), `1` = 4, `2` = 3, `3` = 2, `4` = 1),
           lgbt.pol = impute(as.numeric(d5), mean),
           gen = impute(as.numeric(gen), mean),
           econ = impute(as.numeric(ros4), mean),
           democ = impute(as.numeric(ing4), mean),
           golpe.crim = impute(as.numeric(jc10)),
           golpe.corrup = impute(as.numeric(jc13), mean),
           golpe.congr = impute(as.numeric(jc15), mean),
           year = 2008) 
  
  lapop_2010 <- read.dta("http://datasets.americasbarometer.org/database/files/7948266051039660950Brazil_LAPOP_AmericasBarometer%202010%20data%20set%20%20approved%20v4.dta")%>% #----
    mutate(wt.nm = wt/sum(wt),
           lgbt.pol = impute(as.numeric(d5), mean),
           lgbt.casar = impute(as.numeric(d6), mean),
           gen = NA,
           econ = impute(as.numeric(ros4), mean),
           democ = impute(as.numeric(ing4), mean),
           golpe.crim = impute(as.numeric(jc10)),
           golpe.corrup = impute(as.numeric(jc13), mean),
           golpe.congr = impute(as.numeric(jc15a), mean),
           year = 2010)      
  
  lapop_2012 <- read.dta("http://datasets.americasbarometer.org/database/files/54861031Brazil%20LAPOP%20AmericasBarometer%202012%20Rev1_W.dta")%>% #----
    mutate(lgbt.pol = impute(as.numeric(d5), mean),
           lgbt.casar = impute(as.numeric(d6), mean),
           gen = recode(as.numeric(vb50), `1` = 4, `2` = 3, `3` = 2, `4` = 1),
           gen = impute(as.numeric(gen), mean),
           econ = impute(as.numeric(ros4), mean),
           democ = impute(as.numeric(ing4), mean),
           golpe.crim = impute(as.numeric(jc10)),
           golpe.corrup = impute(as.numeric(jc13), mean),
           golpe.congr = impute(as.numeric(jc15a), mean),
           year = 2012) 
  

  lapop_2014 <- read_dta("http://datasets.americasbarometer.org/database/files/636339374Brazil%20LAPOP%20AmericasBarometer%202014%20v3.0_W.dta")%>% #----
    mutate(lgbt.pol = impute(as.numeric(d5), mean),
           lgbt.casar = impute(as.numeric(d6), mean),
           gen = recode(as.numeric(vb50), `1` = 4, `2` = 3, `3` = 2, `4` = 1),
           gen = impute(as.numeric(gen), mean),
           econ = impute(as.numeric(ros4), mean),
           democ = impute(as.numeric(ing4), mean),
           golpe.crim = impute(as.numeric(jc10)),
           golpe.corrup = impute(as.numeric(jc13), mean),
           golpe.congr = impute(as.numeric(jc15a), mean),
           year = 2014) 
  
  lapop_2016 <- read.dta("http://datasets.americasbarometer.org/database/files/780314464Brazil%20LAPOP%20AmericasBarometer%202017%20V1.0_W.dta")%>% #----
    mutate(wt.nm = wt/sum(wt),
           lgbt.pol = impute(as.numeric(d5), mean),
           lgbt.casar = impute(as.numeric(d6), mean),
           gen = recode(as.numeric(vb50), `1` = 4, `2` = 3, `3` = 2, `4` = 1),
           gen = impute(as.numeric(gen), mean),
           econ = impute(as.numeric(ros4), mean),
           democ = impute(as.numeric(ing4), mean),
           golpe.crim = impute(as.numeric(jc10)),
           golpe.corrup = impute(as.numeric(jc13), mean),
           golpe.congr = impute(as.numeric(jc15a), mean),
           year = 2016) 

  lapop_2018 <- read_dta("http://datasets.americasbarometer.org/database/files/Brazil%20LAPOP%20AmericasBarometer%202019%20v1.0_W.dta")%>% #----
    mutate(wt.nm = wt/sum(wt),
           lgbt.pol = impute(as.numeric(d5), mean),
           lgbt.casar = impute(as.numeric(d6), mean),
           gen = recode(as.numeric(vb50), `1` = 4, `2` = 3, `3` = 2, `4` = 1),
           gen = impute(as.numeric(gen), mean),
           econ = impute(as.numeric(ros4), mean),
           democ = impute(as.numeric(ing4), mean),
           golpe.crim = impute(as.numeric(jc10)),
           golpe.corrup = impute(as.numeric(jc13), mean),
           golpe.congr = impute(as.numeric(jc15a), mean),
           year = 2018) 
  

  
  
  lapop_2021 <- read_dta("http://datasets.americasbarometer.org/database/files/BRA_2021_LAPOP_AmericasBarometer_v1.2_w.dta") #----   
  
  
  
  # 3. Kernel Density Plots ----
  
      # D5. E agora, mudando de assunto e pensando nos homossexuais, quanto o(a) sr./sra. aprova ou desaprova que estas pessoas possam candidatar-se para cargos públicos?
      # Escala de 10 pontos

          
     par(mfrow=c(2,4))
     
      d.lgbt.06 <- density(as.numeric(lapop_2006$lgbt.pol), na.rm=T)%>%
      plot(main = "LGBTs em Cargos Públicos - 2006", ylim = c(0,.25))
      
      d.lgbt.08 <- density(as.numeric(lapop_2008$lgbt.pol), na.rm=T)%>%
      plot(main = "LGBTs em Cargos Públicos - 2008", ylim = c(0,.25))
      
      d.lgbt.10 <- density(as.numeric(lapop_2010$lgbt.pol), na.rm=T, weights = lapop_2010$wt.nm)%>% 
      plot(main = "LGBTs em Cargos Públicos - 2010", ylim = c(0,.25))
      
      d.lgbt.12 <- density(as.numeric(lapop_2012$lgbt.pol), na.rm=T)%>% 
      plot(main = "LGBTs em Cargos Públicos - 2012", ylim = c(0,.25))
      
      d.lgbt.14 <- density(as.numeric(lapop_2014$lgbt.pol), na.rm=T)%>% 
      plot(main = "LGBTs em Cargos Públicos - 2014", ylim = c(0,.25))
      
      d.lgbt.16_17 <- density(as.numeric(lapop_2016$lgbt.pol), na.rm=T, weights = lapop_2016$wt.nm)%>% 
      plot(main = "LGBTs em Cargos Públicos - 2016_17", ylim = c(0,.25))
      
      d.lgbt.18_19 <- density(as.numeric(lapop_2018$lgbt.pol), na.rm=T, weights = lapop_2018$wt.nm)%>% 
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
  
    # 5.2  Using Elbow graph method to determine optimal number of clusters ("3" is the optimal number for all waves) ----
    
          
        # insert data.frame to test: 
          df.to.test <- lapop_2006%>%select(l1, lgbt.pol, gen.06,econ.06, golpe.crim, golpe.corrup, golpe.congr) # k = 10
          df.to.test <- lapop_2008%>%select(l1, lgbt.pol, gen, econ, golpe.crim, golpe.corrup, golpe.congr) # k = 9
          df.to.test <- lapop_2010%>%select(l1,lgbt.pol, econ, golpe.crim, golpe.corrup, golpe.congr) # k = 10
          df.to.test <- lapop_2012%>%select(l1,lgbt.pol, gen, econ, golpe.crim, golpe.corrup, golpe.congr) # k = 10
          df.to.test <- lapop_2014%>%select(l1,lgbt.pol, gen, econ, golpe.crim, golpe.corrup, golpe.congr) # k = 7
          df.to.test <- lapop_2016%>%select(l1,lgbt.pol, gen, econ, golpe.crim, golpe.corrup, golpe.congr) # k = 10
          df.to.test <- lapop_2018%>%select(l1,lgbt.pol, gen, econ, golpe.crim, golpe.corrup, golpe.congr) # k = 11
          
      set.seed(123) 
      fviz_nbclust(scale(na.omit(df.to.test[,-1])), 
                       kmeans, 
                       k.max = 25,
                       method = "wss", 
                       print.summary = T) 

       
       describe(scale(df.to.test))
       
       set.seed(123)
       clustered.df <- kmeans(scale(df.to.test[,-1]), 4, nstart = 25)
       # K-means clusters showing the group of each individuals
       
       
    df.to.test.clustered <-  df.to.test%>%
         mutate(cluster = clustered.df$cluster,
                ideol = impute(as.numeric(l1),mean))%>%
          select(-l1)
    
    summary <- df.to.test.clustered%>%
         group_by(cluster)%>%
         summarize_all(mean, na.rm = T)%>%
         mutate(omnibus = lgbt.pol*econ*golpe.crim*golpe.corrup*golpe.congr)%>%
         arrange(-omnibus)

       
    summary   
    
    table(df.to.test.clustered$cluster)
    
       
       
       fviz_cluster(clustered.df, data = df.to.test[,-1],
                    geom = "point",
                    ellipse.type = "convex", 
                    ggtheme = theme_bw()
       )
       
    
       
      df.to.test <- lapop_2006%>%select(l1, lgbt.pol, gen.06,econ.06, golpe.crim, golpe.corrup, golpe.congr)
      set.seed(123)
      CPC(data = scale(df.to.test[,-1]), k = 10, adjust = TRUE, type = "kmeans")
      df.to.test <- lapop_2008%>%select(l1, lgbt.pol, gen, econ, golpe.crim, golpe.corrup, golpe.congr)
      set.seed(123)
      CPC(data = scale(df.to.test[,-1]), k = 9, adjust = TRUE, type = "kmeans")
      df.to.test <- lapop_2010%>%select(l1,lgbt.pol, econ, golpe.crim, golpe.corrup, golpe.congr)
      set.seed(123)
      CPC(data = scale(df.to.test[,-1]), k = 10, adjust = TRUE, type = "kmeans")
      df.to.test <- lapop_2012%>%select(l1,lgbt.pol, gen, econ, golpe.crim, golpe.corrup, golpe.congr)
      set.seed(123)
      CPC(data = scale(df.to.test[,-1]), k = 10, adjust = TRUE, type = "kmeans")
      df.to.test <- lapop_2014%>%select(l1,lgbt.pol, gen, econ, golpe.crim, golpe.corrup, golpe.congr)
      set.seed(123)
      CPC(data = scale(df.to.test[,-1]), k = 7, adjust = TRUE, type = "kmeans")
      df.to.test <- lapop_2016%>%select(l1,lgbt.pol, gen, econ, golpe.crim, golpe.corrup, golpe.congr)
      set.seed(123)
      CPC(data = scale(df.to.test[,-1]), k = 10, adjust = TRUE, type = "kmeans")
      df.to.test <- lapop_2018%>%select(l1,lgbt.pol, gen, econ, golpe.crim, golpe.corrup, golpe.congr)
      set.seed(123)
      CPC(data = scale(df.to.test[,-1]), k = 11, adjust = TRUE, type = "kmeans")
      
      #
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
        # 5.3 Plotting Graph

          gg.cpc <- rbind(democ, moral,econ)%>%
            ggplot(aes(x = as.Date(paste0(year, "-01-01")), y = value, color = variable, group = variable)) +
            geom_line(linewidth = 1) +
            geom_point()+
            scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
            scale_y_continuous(limits = c(.50, 1), expand = c(0, 0)) +
           # scale_color_discrete(labels = c("Pauta Institucional", "Pauta Moral", "Pauta Econômica"))+
            labs(title = "Evolução da Polarização no Brasil - Temas Sociais e Econômicos",
                 subtitle = "Cluster-Polarization Coefficient aplicado a escalas de favorabilidade",
                 x = "Year",
                 y = "CPC",
                 color = "Coeficiente") +
            theme_minimal()
        
          gg.cpc
          
          
  
          
         