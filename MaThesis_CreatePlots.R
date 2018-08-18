{
  
  ## print status
  cat(paste("\n\nRunning project code...\n\n"))
  
  #############
  ## PREPARE ##
  #############
  {

    cat(paste("Importing libraries..."))
    library(dplyr)
    library(tidyr)
    library(coin)
    library(lsr)
    library(Kendall)
    cat(paste("\t\t\tDone!\n"))
    cat(paste("Importing data..."))
    data <- read.csv("input/rawdata.csv", header = TRUE, sep = ";")
    questionColors = c("darkolivegreen1", "lawngreen", "limegreen", "springgreen4", "darkgreen")
    vAreas = c("Rombergpark", "Talwiese", "Zollverein", "Ringpark")
    cat(paste("\t\tDone!\n\n"))
    
  }
  
  
  ###############################################################################
  ## BOXPLOTS #########################################################
  ###########################################################
  {
    
    ######################################
    ## NONE VERSUS TEXT (ALL QUESTIONS) ##
    ######################################
    {
      
      x11(10,5)
      plot.new()
      mat = layout(matrix(c(1,1,2,3,1,1,4,5), 2, 4, byrow = TRUE),
                   widths = c(1,1,1,1,1,1,1,1),
                   heights = c(1,1,1,1,1,1,1,1))
      layout.show(mat)
      par(cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.5, oma = c(0,1,0,0))
      
      # assign filename
      filename = "boxplot_questions_vs_group_per_area"
      
      # print status
      cat(paste0("Creating plot ", filename, ".\n"))
      
      ## total
      {
        
        # build data.frame
        df = data.frame(group = unique(data$Gruppe),
                        n = NA,
                        median_Natuerlichkeit = NA,
                        n_Natuerlichkeit = NA,
                        median_Schoenheit = NA,
                        n_Schoenheit = NA,
                        median_WichtigkeitSpez = NA,
                        n_WichtigkeitSpez = NA,
                        median_WichtigkeitAll = NA,
                        n_WichtigkeitAll = NA,
                        median_Zufriedenheit = NA,
                        n_Zufriedenheit = NA,
                        row.names = NULL,
                        check.rows = FALSE)
        
        # iterate through all groups
        for (i in 1:nrow(df))
        {
          
          # build data.frame
          df$n[i] = length(subset(data$Gruppe, data$Gruppe == df$group[i]))
          df$median_Natuerlichkeit[i] = median(subset(data$Natuerlichkeit, data$Gruppe == df$group[i]), na.rm = TRUE)
          df$n_Natuerlichkeit[i] = length(subset(data$Natuerlichkeit[!is.na(data$Natuerlichkeit)], data$Gruppe[!is.na(data$Natuerlichkeit)] == df$group[i]))
          df$median_Schoenheit[i] = median(subset(data$Schoenheit, data$Gruppe == df$group[i]), na.rm = TRUE)
          df$n_Schoenheit[i] = length(subset(data$Schoenheit[!is.na(data$Schoenheit)], data$Gruppe[!is.na(data$Schoenheit)] == df$group[i]))
          df$median_WichtigkeitSpez[i] = median(subset(data$WichtigkeitSpez, data$Gruppe == df$group[i]), na.rm = TRUE)
          df$n_WichtigkeitSpez[i] = length(subset(data$WichtigkeitSpez[!is.na(data$WichtigkeitSpez)], data$Gruppe[!is.na(data$WichtigkeitSpez)] == df$group[i]))
          df$median_WichtigkeitAll[i] = median(subset(data$WichtigkeitAll, data$Gruppe == df$group[i]), na.rm = TRUE)
          df$n_WichtigkeitAll[i] = length(subset(data$WichtigkeitAll[!is.na(data$WichtigkeitAll)], data$Gruppe[!is.na(data$WichtigkeitAll)] == df$group[i]))
          df$median_Zufriedenheit[i] = median(subset(data$Zufriedenheit, data$Gruppe == df$group[i]), na.rm = TRUE)
          df$n_Zufriedenheit[i] = length(subset(data$Zufriedenheit[!is.na(data$Zufriedenheit)], data$Gruppe[!is.na(data$Zufriedenheit)] == df$group[i]))
          
        }
        
        # build statistics data.frame
        dfp = data.frame(statistics = c("shapiro-wilk (CTRL)", "shapiro-wilk (TEXT)", "model" ,"p.value", "model", "p.value", "effect.size"),
                         Natuerlichkeit = NA,
                         Schoenheit = NA,
                         WichtigkeitSpez = NA,
                         WichtigkeitAll = NA,
                         Zufriedenheit = NA)
        
        for (i in 2:6)
        {
          
          # check if one of the groups does not have normal distributed values
          dfp[1,i] = round(shapiro.test(subset(data[,i+5], data$Gruppe == "NONE"))$p.value, digits = 5)
          dfp[2,i] = round(shapiro.test(subset(data[,i+5], data$Gruppe == "TEXT"))$p.value, digits = 5)
          
          # check for variance homogenity
          if (as.numeric(dfp[1,i]) > 0.05 && as.numeric(dfp[2,i]) > 0.05) # if both groups show normal distribution
          {
            
            dfp[3,i] = "bartlett"
            dfp[4,i] = round(bartlett.test(data[,i+5]~data$Gruppe)$p.value, digits = 5)
            
          } else # if one of the groups does not show normal distribution
          {
            
            dfp[3,i] = "fligner-killeen"
            dfp[4,i] = round(fligner.test(data[,i+5]~data$Gruppe)$p.value, digits = 5)
            
          }
          
          # select statistical test
          groupA <- subset(data[,i+5], data$Gruppe == "NONE")
          groupB <- subset(data[,i+5], data$Gruppe == "TEXT")
          if (as.numeric(dfp[1,i]) > 0.05 && as.numeric(dfp[2,i]) > 0.05 && as.numeric(dfp[4,i]) > 0.05) # normal distributions and equal variances
          {
            
            dfp[5,i] = "t.test"
            dfp[6,i] = round(t.test(groupA, groupB)$p.value, digits = 5)
            dfp[7,i] = round((mean(groupA) - mean(groupB))/sd(groupA), digits = 5)
            
          } else # if data not distributed normally OR varations aren't equal
          {
            
            dfp[5,i] = "wilcoxon"
            wTest <- wilcox.test(groupA, groupB, exact = TRUE, paired = FALSE)
            dfp[6,i] = round(wTest$p.value, digits = 5)
            dfp[7,i] = round((mean(groupA, na.rm = TRUE) - mean(groupB, na.rm = TRUE))/sd(groupA, na.rm = TRUE), digits = 5)
            
          }
          
        }
        
        # create empty boxplot
        boxplot(data[,7:11],
                main = paste0("(a) Alle Flächen"),
                ylab = "Antwortwert",
                boxfill = rgb(1, 1, 1, alpha = 1),
                border = rgb(1, 1, 1, alpha = 1),
                ylim = c(1, 10),
                xlim = c(0.5, 5.5),
                names = c("F1", "F2", "F3", "F4", "F5"),
                axes = FALSE)
        
        # create y and x axis
        axis(side = 2,
             at = c(1:10),
             col = 'black')
        axis(side = 1,
             at = c(1:5),
             col = 'white',
             labels = c("F1", "F2", "F3", "F4", "F5"))
        
        # add legend
        legend("bottomright",
               c("CTRL", "TEXT"),
               fill = c(questionColors[2], questionColors[3]),
               bty = "n",
               cex = 1.1)
        
        # create grouped boxplots for each question
        for (i in 7:11)
        {
          
          # plot data for CTRL GROUP
          boxplot(subset(data[,i], data$Gruppe == "NONE"),
                  at = (i-6) - 0.2,
                  axes = FALSE,
                  ylim = c(1,10),
                  add = TRUE,
                  col = questionColors[2],
                  boxwex = 0.6)
          
          # plot data for TEXT GROUP
          boxplot(subset(data[,i], data$Gruppe == "TEXT"),
                  at = (i-6) + 0.2,
                  axes = FALSE,
                  ylim = c(1,10),
                  add = TRUE,
                  col = questionColors[3],
                  boxwex = 0.6)
          
          # add indicators of significance
          if (as.numeric(dfp[6,i-5]) < 0.05 && 0.01 < as.numeric(dfp[6,i-5])) # *
          {
            mtext("*", side = 1, at = i-6, cex = 2.5, col = "darkgreen")
          } else if (as.numeric(dfp[6,i-5]) < 0.01 && 0.001 < as.numeric(dfp[6,i-5])) # **
          {
            mtext("**", side = 1, at = i-6, cex = 2.5, col = "darkgreen")
          } else if (as.numeric(dfp[6,i-5]) < 0.001) # ***
          {
            mtext("***", side = 1, at = i-6, cex = 2.5, col = "darkgreen")
          }
          
        }
        
        write.table(as.matrix(df),file=paste("output/", filename, "_matrix.csv", sep = ""),sep=";", col.names=TRUE, row.names=FALSE)
        write.table(as.matrix(dfp),file=paste("output/", filename, "_statistics.csv", sep = ""),sep=";", col.names=TRUE, row.names=FALSE)
        
      }; rm(df, dfp, i)
      
      ## per area
      for (area in 1:length(vAreas))
      {
        
        # create data subset
        subset <- subset(data, data$Flaeche == vAreas[area])
        
        # build data.frame
        df = data.frame(group = unique(subset$Gruppe),
                        n = NA,
                        median_Natuerlichkeit = NA,
                        n_Natuerlichkeit = NA,
                        median_Schoenheit = NA,
                        n_Schoenheit = NA,
                        median_WichtigkeitSpez = NA,
                        n_WichtigkeitSpez = NA,
                        median_WichtigkeitAll = NA,
                        n_WichtigkeitAll = NA,
                        median_Zufriedenheit = NA,
                        n_Zufriedenheit = NA,
                        row.names = NULL,
                        check.rows = FALSE)
        
        # iterate through all groups
        for (i in 1:nrow(df))
        {
          
          # build data.frame
          df$n[i] = length(subset(subset$Gruppe, subset$Gruppe == df$group[i]))
          df$median_Natuerlichkeit[i] = median(subset(subset$Natuerlichkeit, subset$Gruppe == df$group[i]), na.rm = TRUE)
          df$n_Natuerlichkeit[i] = length(subset(subset$Natuerlichkeit[!is.na(subset$Natuerlichkeit)], subset$Gruppe[!is.na(subset$Natuerlichkeit)] == df$group[i]))
          df$median_Schoenheit[i] = median(subset(subset$Schoenheit, subset$Gruppe == df$group[i]), na.rm = TRUE)
          df$n_Schoenheit[i] = length(subset(subset$Schoenheit[!is.na(subset$Schoenheit)], subset$Gruppe[!is.na(subset$Schoenheit)] == df$group[i]))
          df$median_WichtigkeitSpez[i] = median(subset(subset$WichtigkeitSpez, subset$Gruppe == df$group[i]), na.rm = TRUE)
          df$n_WichtigkeitSpez[i] = length(subset(subset$WichtigkeitSpez[!is.na(subset$WichtigkeitSpez)], subset$Gruppe[!is.na(subset$WichtigkeitSpez)] == df$group[i]))
          df$median_WichtigkeitAll[i] = median(subset(subset$WichtigkeitAll, subset$Gruppe == df$group[i]), na.rm = TRUE)
          df$n_WichtigkeitAll[i] = length(subset(subset$WichtigkeitAll[!is.na(subset$WichtigkeitAll)], subset$Gruppe[!is.na(subset$WichtigkeitAll)] == df$group[i]))
          df$median_Zufriedenheit[i] = median(subset(subset$Zufriedenheit, subset$Gruppe == df$group[i]), na.rm = TRUE)
          df$n_Zufriedenheit[i] = length(subset(subset$Zufriedenheit[!is.na(subset$Zufriedenheit)], subset$Gruppe[!is.na(subset$Zufriedenheit)] == df$group[i]))
          
        }
        
        # build statistics data.frame
        dfp = data.frame(statistics = c("shapiro-wilk (CTRL)", "shapiro-wilk (TEXT)", "model" ,"p.value", "model", "p.value", "effect.size"),
                         Natuerlichkeit = NA,
                         Schoenheit = NA,
                         WichtigkeitSpez = NA,
                         WichtigkeitAll = NA,
                         Zufriedenheit = NA)
        for (i in 2:6)
        {
          
          # check if one of the groups does not have normal distributed values
          dfp[1,i] = round(shapiro.test(subset(subset[,i+5], subset$Gruppe == "NONE"))$p.value, digits = 5)
          dfp[2,i] = round(shapiro.test(subset(subset[,i+5], subset$Gruppe == "TEXT"))$p.value, digits = 5)
          
          # check for variance homogenity
          if (as.numeric(dfp[1,i]) > 0.05 && as.numeric(dfp[2,i]) > 0.05) # if both groups show normal distribution
          {
            
            dfp[3,i] = "bartlett"
            dfp[4,i] = round(bartlett.test(subset[,i+5]~subset$Gruppe)$p.value, digits = 5)
            
          } else # if one of the groups does not show normal distribution
          {
            
            dfp[3,i] = "fligner-killeen"
            dfp[4,i] = round(fligner.test(subset[,i+5]~subset$Gruppe)$p.value, digits = 5)
            
          }
          
          # select statistical test
          groupA <- subset(subset[,i+5], subset$Gruppe == "NONE")
          groupB <- subset(subset[,i+5], subset$Gruppe == "TEXT")
          if (as.numeric(dfp[1,i]) > 0.05 && as.numeric(dfp[2,i]) > 0.05 && as.numeric(dfp[4,i]) > 0.05) # normal distributions and equal variances
          {
            
            dfp[5,i] = "t.test"
            dfp[6,i] = round(t.test(groupA, groupB)$p.value, digits = 5)
            dfp[7,i] = round(cohensD(groupA, groupB), digits = 5)
            
          } else # if data not distributed normally OR varations aren't equal
          {
            
            dfp[5,i] = "wilcoxon"
            wTest <- wilcox.test(groupA, groupB, exact = TRUE, paired = FALSE)
            dfp[6,i] = round(wTest$p.value, digits = 5)
            dfp[7,i] = round((mean(groupA, na.rm = TRUE) - mean(groupB, na.rm = TRUE))/sd(groupA, na.rm = TRUE), digits = 5)
            
          }
          
        }
        
        # create empty boxplot
        boxplot(subset[,7:11],
                main = paste0("(",letters[area+1] ,") ", vAreas[area]),
                boxfill = rgb(1, 1, 1, alpha = 1),
                border = rgb(1, 1, 1, alpha = 1),
                ylim = c(1, 10),
                xlim = c(0.5, 5.5),
                names = c("F1", "F2", "F3", "F4", "F5"),
                axes = FALSE)
        
        # create y and x axis
        axis(side = 2,
             at = c(1,10),
             col = 'white',
             labels = c(1,10))
        axis(side = 2,
             at = c(1:10),
             col = 'black',
             labels = FALSE)
        axis(side = 1,
             at = c(1:5),
             col = 'white',
             labels = c("F1", "F2", "F3", "F4", "F5"))
        
        # create grouped boxplots for each question
        for (i in 7:11)
        {
          
          # plot subset for CTRL GROUP
          boxplot(subset(subset[,i], subset$Gruppe == "NONE"),
                  at = (i-6) - 0.2,
                  axes = FALSE,
                  ylim = c(1,10),
                  add = TRUE,
                  col = questionColors[2],
                  boxwex = 0.6)
          
          # plot data for TEXT GROUP
          boxplot(subset(subset[,i], subset$Gruppe == "TEXT"),
                  at = (i-6) + 0.2,
                  axes = FALSE,
                  ylim = c(1,10),
                  add = TRUE,
                  col = questionColors[3],
                  boxwex = 0.6)

          # add indicators of significance
          if (as.numeric(dfp[6,i-5]) < 0.05 && 0.01 < as.numeric(dfp[6,i-5])) # *
          {
            mtext("*", side = 1, at = i-6, cex = 2.5, col = "darkgreen")
          } else if (as.numeric(dfp[6,i-5]) < 0.01 && 0.001 < as.numeric(dfp[6,i-5])) # **
          {
            mtext("**", side = 1, at = i-6, cex = 2.5, col = "darkgreen")
          } else if (as.numeric(dfp[6,i-5] < 0.001)) # ***
          {
            mtext("***", side = 1, at = i-6, cex = 2.5, col = "darkgreen")
          }
          
        }

        write.table(as.matrix(df),file=paste("output/", filename, "_", vAreas[area], "_matrix.csv", sep = ""),sep=";", col.names=TRUE, row.names=FALSE)
        write.table(as.matrix(dfp),file=paste("output/", filename, "_", vAreas[area], "_statistics.csv", sep = ""),sep=";", col.names=TRUE, row.names=FALSE)
        rm(df, dfp, i, subset, groupA, groupB, wTest)
        
      }
      
      # export and finish
      cat(paste("Exporting to output directory..."))
      dev.copy2pdf(file = paste0("output/", filename, ".pdf"))
      rm(filename, area, mat)
      dev.off()
      cat(paste("\tDone!\n\n"))
      
    } # working
    
    ###########################################
    ## ADDITIONAL PARAMETERS (ALL QUESTIONS) ##
    ###########################################
    {
      
      x11(10,7.5)
      plot.new()
      mat = layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE),
                   widths = c(1,1,1,1),
                   heights = c(1,1,1,1))
      layout.show(mat)
      par(cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.5, oma = c(0,1,0,0))
      
      filename = "boxplot_additional"
      cat(paste0("Creating plot ", filename, "..."))
      
      ## GENDER ######################################################################
      {
        
        df = data.frame(question = c("F1", "F2", "F3", "F4", "F5"),
                        nW = NA,
                        nM = NA,
                        shapiroW = NA,
                        shapiroM = NA,
                        fligner = NA,
                        wilcox = NA,
                        glass = NA)
        
        # create empty boxplot
        boxplot(data[,7:11],
                main = paste0("(a) Geschlecht"),
                ylab = "Antwortwert",
                boxfill = rgb(1, 1, 1, alpha = 1),
                border = rgb(1, 1, 1, alpha = 1),
                ylim = c(1, 10),
                xlim = c(0.5, 7),
                names = c("F1", "F2", "F3", "F4", "F5"),
                axes = FALSE)
        
        # create y and x axis
        axis(side = 2,
             at = c(1,10),
             col = 'white',
             labels = c(1,10))
        axis(side = 2,
             at = c(1:10),
             col = 'black',
             labels = FALSE)
        axis(side = 1,
             at = c(1:5),
             col = 'white',
             labels = c("F1", "F2", "F3", "F4", "F5"))
        # add legend
        legend("right", inset = c(0.1,0),
               horiz = FALSE,
               c("w", "m"),
               fill = c(questionColors[2], questionColors[3]),
               bty = "n",
               cex = 1.1)
        
        for (i in 7:11)
        {
          
          # fill df
          groupA = subset(data[,i], data$Geschlecht == "w")
          groupB = subset(data[,i], data$Geschlecht == "m")
          df$nW[i-6] = length(groupA)
          df$nM[i-6] = length(groupB)
          df$shapiroW[i-6] = round(shapiro.test(groupA)$p.value, digits = 3)
          df$shapiroM[i-6] = round(shapiro.test(groupB)$p.value, digits = 3)
          df$fligner[i-6] = round(fligner.test(data[,i]~data$Geschlecht)$p.value, digits = 3)
          df$wilcox[i-6] = round(wilcox.test(groupA, groupB)$p.value, digits = 3)
          df$glass[i-6] = round((mean(groupA, na.rm = TRUE) - mean(groupB, na.rm = TRUE))/sd(groupA, na.rm = TRUE), digits = 3)
          
          # plot data for w
          boxplot(groupA,
                  at = (i-6) - 0.2,
                  axes = FALSE,
                  ylim = c(1,10),
                  add = TRUE,
                  col = questionColors[2],
                  boxwex = 0.6)
          
          # plot data for m
          boxplot(groupB,
                  at = (i-6) + 0.2,
                  axes = FALSE,
                  ylim = c(1,10),
                  add = TRUE,
                  col = questionColors[3],
                  boxwex = 0.6)
          
        }
        
        write.table(as.matrix(df),file=paste("output/", filename, "_gender_matrix.csv", sep = ""),sep=";", col.names=TRUE, row.names=FALSE)
      
      }
      
      ## HERITAGE ######################################################################
      {
        
        df = data.frame(question = c("F1", "F2", "F3", "F4", "F5"),
                        nA = NA,
                        nB = NA,
                        shapiroA = NA,
                        shapiroB = NA,
                        fligner = NA,
                        wilcox = NA,
                        glass = NA)
        
        # create empty boxplot
        boxplot(data[,7:11],
                main = paste0("(b) Staatsangehörigkeit"),
                ylab = "Antwortwert",
                boxfill = rgb(1, 1, 1, alpha = 1),
                border = rgb(1, 1, 1, alpha = 1),
                ylim = c(1, 10),
                xlim = c(0.5, 7),
                names = c("F1", "F2", "F3", "F4", "F5"),
                axes = FALSE)
        
        # create y and x axis
        axis(side = 2,
             at = c(1,10),
             col = 'white',
             labels = c(1,10))
        axis(side = 2,
             at = c(1:10),
             col = 'black',
             labels = FALSE)
        axis(side = 1,
             at = c(1:5),
             col = 'white',
             labels = c("F1", "F2", "F3", "F4", "F5"))
        # add legend
        legend("right", inset = c(0,0),
               horiz = FALSE,
               c("deutsch", "andere"),
               fill = c(questionColors[2], questionColors[3]),
               bty = "n",
               cex = 1.1)
        
        for (i in 7:11)
        {
          
          # fill df
          groupA = subset(data[,i], data$Staatsangehoerigkeit == "deutsch")
          groupB = subset(data[,i], data$Staatsangehoerigkeit == "andere")
          df$nA[i-6] = length(groupA)
          df$nB[i-6] = length(groupB)
          df$shapiroA[i-6] = round(shapiro.test(groupA)$p.value, digits = 3)
          df$shapiroB[i-6] = round(shapiro.test(groupB)$p.value, digits = 3)
          df$fligner[i-6] = round(fligner.test(data[,i]~data$Geschlecht)$p.value, digits = 3)
          df$wilcox[i-6] = round(wilcox.test(groupA, groupB)$p.value, digits = 3)
          df$glass[i-6] = round((mean(groupA, na.rm = TRUE) - mean(groupB, na.rm = TRUE))/sd(groupA, na.rm = TRUE), digits = 3)
          
          # plot data for A
          boxplot(groupA,
                  at = (i-6) - 0.2,
                  axes = FALSE,
                  ylim = c(1,10),
                  add = TRUE,
                  col = questionColors[2],
                  boxwex = 0.6)
          
          # plot data for B
          boxplot(groupB,
                  at = (i-6) + 0.2,
                  axes = FALSE,
                  ylim = c(1,10),
                  add = TRUE,
                  col = questionColors[3],
                  boxwex = 0.6)
          
        }
        
        write.table(as.matrix(df),file=paste("output/", filename, "_nationality_matrix.csv", sep = ""),sep=";", col.names=TRUE, row.names=FALSE)
        
      }
      
      ## EDUCATION ######################################################################
      {
        
        df = data.frame(question = c("F1", "F2", "F3", "F4", "F5"),
                        nA = NA,
                        nB = NA,
                        nC = NA,
                        shapiroA = NA,
                        shapiroB = NA,
                        shapiroC = NA,
                        fligner = NA,
                        wilcoxAB = NA,
                        wilcoxAC = NA,
                        wilcoxBC = NA,
                        glassAB = NA,
                        glassAC = NA,
                        glassBC = NA)
        
        # create empty boxplot
        boxplot(data[,7:11],
                main = paste0("(c) Abschluss"),
                ylab = "Antwortwert",
                boxfill = rgb(1, 1, 1, alpha = 1),
                border = rgb(1, 1, 1, alpha = 1),
                ylim = c(1, 10),
                xlim = c(0.5, 7),
                names = c("F1", "F2", "F3", "F4", "F5"),
                axes = FALSE)
        
        # create y and x axis
        axis(side = 2,
             at = c(1,10),
             col = 'white',
             labels = c(1,10))
        axis(side = 2,
             at = c(1:10),
             col = 'black',
             labels = FALSE)
        axis(side = 1,
             at = c(1:5),
             col = 'white',
             labels = c("F1", "F2", "F3", "F4", "F5"))
        # add legend
        legend("right", inset = c(0.05,0),
               horiz = FALSE,
               c("ohne", "mittel", "hoch"),
               fill = c(questionColors[2], questionColors[3], questionColors[4]),
               bty = "n",
               cex = 1.1)
        
        for (i in 7:11)
        {
          
          # fill df
          groupA = subset(data[,i], data$Abschluss == "(noch) ohne Abschluss")
          groupB = subset(data[,i], data$Abschluss == "Lehre oder schulischer Abschluss")
          groupC = subset(data[,i], data$Abschluss == "(Fach)Hochschulabschluss")
          df$nA[i-6] = length(groupA)
          df$nB[i-6] = length(groupB)
          df$nC[i-6] = length(groupC)
          df$shapiroA[i-6] = round(shapiro.test(groupA)$p.value, digits = 3)
          df$shapiroB[i-6] = round(shapiro.test(groupB)$p.value, digits = 3)
          df$shapiroC[i-6] = round(shapiro.test(groupC)$p.value, digits = 3)
          df$fligner[i-6] = round(fligner.test(data[,i]~data$Abschluss)$p.value, digits = 3)
          df$wilcoxAB[i-6] = round(wilcox.test(groupA, groupB)$p.value, digits = 3)
          df$wilcoxAC[i-6] = round(wilcox.test(groupA, groupC)$p.value, digits = 3)
          df$wilcoxBC[i-6] = round(wilcox.test(groupB, groupC)$p.value, digits = 3)
          df$glassAB[i-6] = round((mean(groupA, na.rm = TRUE) - mean(groupB, na.rm = TRUE))/sd(groupA, na.rm = TRUE), digits = 3)
          df$glassAC[i-6] = round((mean(groupA, na.rm = TRUE) - mean(groupC, na.rm = TRUE))/sd(groupA, na.rm = TRUE), digits = 3)
          df$glassBC[i-6] = round((mean(groupB, na.rm = TRUE) - mean(groupC, na.rm = TRUE))/sd(groupA, na.rm = TRUE), digits = 3)
          
          # plot data for A
          boxplot(groupA,
                  at = (i-6) - 0.25,
                  axes = FALSE,
                  ylim = c(1,10),
                  add = TRUE,
                  col = questionColors[2],
                  boxwex = 0.4)
          
          # plot data for B
          boxplot(groupB,
                  at = (i-6),
                  axes = FALSE,
                  ylim = c(1,10),
                  add = TRUE,
                  col = questionColors[3],
                  boxwex = 0.4)
          
          # plot data for C
          boxplot(groupC,
                  at = (i-6) + 0.25,
                  axes = FALSE,
                  ylim = c(1,10),
                  add = TRUE,
                  col = questionColors[4],
                  boxwex = 0.4)
          
        }
        
        write.table(as.matrix(df),file=paste("output/", filename, "_education_matrix.csv", sep = ""),sep=";", col.names=TRUE, row.names=FALSE)
        
      }
      
      ## DISTANCE ######################################################
      {
        
        df = data.frame(question = c("F1", "F2", "F3", "F4", "F5"),
                        nA = NA,
                        nB = NA,
                        nC = NA,
                        nD = NA,
                        shapiroA = NA,
                        shapiroB = NA,
                        shapiroC = NA,
                        shapiroD = NA,
                        fligner = NA,
                        wilcoxAB = NA,
                        wilcoxAC = NA,
                        wilcoxAD = NA,
                        wilcoxBC = NA,
                        wilcoxBD = NA,
                        wilcoxCD = NA,
                        glassAB = NA,
                        glassAC = NA,
                        glassAD = NA,
                        glassBC = NA,
                        glassBD = NA,
                        glassCD = NA)
        
        # create empty boxplot
        boxplot(data[,7:11],
                main = paste0("(d) Entfernung in Gehmin."),
                ylab = "Antwortwert",
                boxfill = rgb(1, 1, 1, alpha = 1),
                border = rgb(1, 1, 1, alpha = 1),
                ylim = c(1, 10),
                xlim = c(0.5, 7),
                names = c("F1", "F2", "F3", "F4", "F5"),
                axes = FALSE)
        
        # create y and x axis
        axis(side = 2,
             at = c(1,10),
             col = 'white',
             labels = c(1,10))
        axis(side = 2,
             at = c(1:10),
             col = 'black',
             labels = FALSE)
        axis(side = 1,
             at = c(1:5),
             col = 'white',
             labels = c("F1", "F2", "F3", "F4", "F5"))
        # add legend
        legend("right", inset = c(0,0),
               horiz = FALSE,
               c("< 5", "5 - 15", "15 - 30", "> 30"),
               fill = c(questionColors[2], questionColors[3], questionColors[4], questionColors[5]),
               bty = "n",
               cex = 1.1)
        
        for (i in 7:11)
        {
          
          # fill df
          groupA = subset(data[,i], data$Entfernung == "weniger als 5")
          groupB = subset(data[,i], data$Entfernung == "5 bis 15")
          groupC = subset(data[,i], data$Entfernung == "15 bis 30")
          groupD = subset(data[,i], data$Entfernung == "mehr als 30")
          df$nA[i-6] = length(groupA)
          df$nB[i-6] = length(groupB)
          df$nC[i-6] = length(groupC)
          df$nD[i-6] = length(groupD)
          df$shapiroA[i-6] = round(shapiro.test(groupA)$p.value, digits = 3)
          df$shapiroB[i-6] = round(shapiro.test(groupB)$p.value, digits = 3)
          df$shapiroC[i-6] = round(shapiro.test(groupC)$p.value, digits = 3)
          df$shapiroD[i-6] = round(shapiro.test(groupD)$p.value, digits = 3)
          df$fligner[i-6] = round(fligner.test(data[,i]~data$Entfernung)$p.value, digits = 3)
          df$wilcoxAB[i-6] = round(wilcox.test(groupA, groupB)$p.value, digits = 3)
          df$wilcoxAC[i-6] = round(wilcox.test(groupA, groupC)$p.value, digits = 3)
          df$wilcoxAD[i-6] = round(wilcox.test(groupA, groupD)$p.value, digits = 3)
          df$wilcoxBC[i-6] = round(wilcox.test(groupB, groupC)$p.value, digits = 3)
          df$wilcoxBD[i-6] = round(wilcox.test(groupB, groupD)$p.value, digits = 3)
          df$wilcoxCD[i-6] = round(wilcox.test(groupC, groupD)$p.value, digits = 3)
          df$glassAB[i-6] = round((mean(groupA, na.rm = TRUE) - mean(groupB, na.rm = TRUE))/sd(groupA, na.rm = TRUE), digits = 3)
          df$glassAC[i-6] = round((mean(groupA, na.rm = TRUE) - mean(groupC, na.rm = TRUE))/sd(groupA, na.rm = TRUE), digits = 3)
          df$glassAD[i-6] = round((mean(groupA, na.rm = TRUE) - mean(groupD, na.rm = TRUE))/sd(groupA, na.rm = TRUE), digits = 3)
          df$glassBC[i-6] = round((mean(groupB, na.rm = TRUE) - mean(groupC, na.rm = TRUE))/sd(groupB, na.rm = TRUE), digits = 3)
          df$glassBD[i-6] = round((mean(groupB, na.rm = TRUE) - mean(groupD, na.rm = TRUE))/sd(groupB, na.rm = TRUE), digits = 3)
          df$glassCD[i-6] = round((mean(groupC, na.rm = TRUE) - mean(groupD, na.rm = TRUE))/sd(groupC, na.rm = TRUE), digits = 3)
          
          # plot data for A
          boxplot(groupA,
                  at = (i-6) - 0.3,
                  axes = FALSE,
                  ylim = c(1,10),
                  add = TRUE,
                  col = questionColors[2],
                  boxwex = 0.3)
          
          # plot data for B
          boxplot(groupB,
                  at = (i-6) - 0.1,
                  axes = FALSE,
                  ylim = c(1,10),
                  add = TRUE,
                  col = questionColors[3],
                  boxwex = 0.3)
          
          # plot data for C
          boxplot(groupC,
                  at = (i-6) + 0.1,
                  axes = FALSE,
                  ylim = c(1,10),
                  add = TRUE,
                  col = questionColors[4],
                  boxwex = 0.3)
          
          # plot data for D
          boxplot(groupD,
                  at = (i-6) + 0.3,
                  axes = FALSE,
                  ylim = c(1,10),
                  add = TRUE,
                  col = questionColors[5],
                  boxwex = 0.3)
          
        }
        
        write.table(as.matrix(df),file=paste("output/", filename, "_distance_matrix.csv", sep = ""),sep=";", col.names=TRUE, row.names=FALSE)
        
      }
      
      dev.copy2pdf(file = paste0("output/", filename, ".pdf"))
      dev.off()
      rm(groupA, groupB, groupC, groupD, i, mat, filename, df)
      cat("\t\tDone!\n")
      
    } # working
    
  }
  
  ###############################################################################
  ## BARPLOTS #########################################################
  ###########################################################
  {
    
    #####################################
    ## MEAN EVALUATION (ALL QUESTIONS) ##
    #####################################
    {
      
      # set up graphics device
      n_plots = length(unique(data$Flaeche)) # get number of plots
      x11()
      plot.new()
      par (oma = c(4,4,2,2), # outer margins
           mai = c(0.3, 0, 0.3, 0), # plot margins
           mfrow = c(n_plots,1),
           cex.lab = 1.4, # label font size
           cex.axis = 1.4) # axis font size
      rm(n_plots)
      
      # assign filename
      filename = "Mean_Evaluation"
      
      # assign headers
      headerX = "Frage"
      headerY = "Bewertung"
      
      # print status
      cat(paste("Creating plot \"", filename, "\" "))
      cat(paste("with \"", headerX, "\" as independent variable"))
      cat(paste(" and \"", headerY, "\" as dependent variable.\n"))
      
      # build data.frame
      df = data.frame(area = unique(data$Flaeche),
                      n = NA,
                      mean_Schoenheit = NA,
                      mean_Natuerlichkeit = NA,
                      mean_WichtigkeitSpez = NA,
                      mean_WichtigkeitAll = NA,
                      mean_Zufriedenheit = NA,
                      row.names = NULL,
                      check.rows = FALSE)
      
      # interate through all areas
      for (i in 1:nrow(df))
      {
        
        # build data.frame
        df$n[i] = length(subset(data$Flaeche, data$Flaeche == df$area[i]))
        df$mean_Schoenheit[i] = mean((subset(data$Schoenheit, data$Flaeche == df$area[i])), na.rm = TRUE)
        df$mean_Natuerlichkeit[i] = mean((subset(data$Natuerlichkeit, data$Flaeche == df$area[i])), na.rm = TRUE)
        df$mean_WichtigkeitSpez[i] = mean((subset(data$WichtigkeitSpez, data$Flaeche == df$area[i])), na.rm = TRUE)
        df$mean_WichtigkeitAll[i] = mean((subset(data$WichtigkeitAll, data$Flaeche == df$area[i])), na.rm = TRUE)
        df$mean_Zufriedenheit[i] = mean((subset(data$Zufriedenheit, data$Flaeche == df$area[i])), na.rm = TRUE)
        
        
        # create barplot (without xlab)
        mp <-barplot(c(df$mean_Zufriedenheit[i],
                       df$mean_WichtigkeitAll[i],
                       df$mean_WichtigkeitSpez[i],
                       df$mean_Natuerlichkeit[i],
                       df$mean_Schoenheit[i]),
                     main = paste0("(", letters[i],") ", df$area[i], " (n = ", df$n[i], ")"),
                     names.arg = c("F5","F4","F3","F2","F1"),
                     las = 1,
                     xlim = range(0:10),
                     horiz = TRUE,
                     border = questionColors[5],
                     col = questionColors[5])
        
        # add text
        for (ii in 3:ncol(df))
        {
          
          v <- subset(data[,ii+4], data$Flaeche == df$area[i])
          for (iii in 1:length(mp))
          {
            
            if (iii == ii - 2)
            {
              
              text(3, mp[6-iii], paste0("μ = ", round(df[i,ii], digits = 2), " (n = ", length(v[!is.na(v)]), ")"), col = "White", cex = 1)
              
            }
            
          }
          
        }
        
      }
      
      # add xlabel to bottom outer margin
      mtext("Mittlerer Antwortwert", side = 1, outer = TRUE, padj = 0.5)
      
      # print status
      cat(paste("Exporting to output directory..."))
      
      # export and finish
      dev.copy2pdf(file = paste0("output/", filename, ".pdf"))
      df = as.matrix(df)
      write.table(df,file=paste("output/", filename, "_matrix.csv", sep = ""),sep=";", col.names=TRUE, row.names=FALSE)
      rm(filename, df, headerX, headerY, i, ii, iii, v, mp)
      dev.off()
      
      # print status
      cat(paste("\tDone!\n\n"))
      
    } # working
    
    ##################
    ## DATA QUALITY ##
    ##################
    {
      
      ## print status
      cat(paste0("Creating plots for data quality visualization..."))
      filename = "data_summary"
      
      # first row
      plot.new()
      x11(10,2.5)
      mat = layout(matrix(c(1,2,3,3), 1, 4, byrow = TRUE),
                   widths = c(4,4,4,4),
                   heights = c(4,4,4,4))
      layout.show(mat)
      par(cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.3, oma = c(0,1,0,0))
      
      ## GENDER ################################################################
      {

        bp = barplot(c(nrow(subset(data, data$Geschlecht == "w")), nrow(subset(data, data$Geschlecht == "m"))),
                border = "white",
                main = "(a) Geschlecht",
                col = questionColors[2],
                cex.names = 1.2,
                axes = FALSE,
                ylab = "Häufigkeit",
                space = 0.1,
                ylim = c(0,250))
        axis(2, at = c(0,125,250))
        axis(1, at = bp, tick = FALSE, labels = c("weiblich", "männlich"))
        rm(bp)
        
      }
      ## HERITAGE ##############################################################
      {
        
        bp = barplot(c(nrow(subset(data, data$Staatsangehoerigkeit == "deutsch")), nrow(subset(data, data$Staatsangehoerigkeit == "andere"))),
                border = "white",
                main = "(b) Staatsangehörigkeit",
                col = questionColors[2],
                ylab = "Häufigkeit",
                cex.names = 1.2,
                axes = FALSE,
                space = 0.1,
                ylim = c(0,450))
        axis(2, at = c(0,225,450))
        axis(1, at = bp, tick = FALSE, labels = c("deutsch", "andere"))
        rm(bp)
        
      }
      ## AGE #######################################################
      {
        
        hist(data$Alter,
             main = "(c) Altersverteilung",
             xlab = "Alter",
             ylab = "Häufigkeit",
             axes = FALSE,
             xlim = c(10,100),
             ylim = c(0,125),
             breaks = c(10,20,30,40,50,60,70,80,90,100),
             freq = TRUE,
             col = questionColors[2],
             border = "white")
        axis(1, at = c(10,20,30,40,50,60,70,80,90,100))
        axis(2, at = c(0,60,120))
        
      }
      
      dev.copy2pdf(file = paste0("output/", filename, "_row1.pdf"))
      # second row
      dev.off()
      plot.new()
      x11(10,2.5)
      mat = layout(matrix(c(1,2,3), 1, 3, byrow = TRUE),
                   widths = c(4,4,4),
                   heights = c(4,4,4))
      layout.show(mat)
      par(cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.3, oma = c(0,1,0,0))
      ## EDUCATION #############################################################
      {
        
        bp = barplot(c(nrow(subset(data, data$Abschluss == "(noch) ohne Abschluss")), nrow(subset(data, data$Abschluss == "Lehre oder schulischer Abschluss")), nrow(subset(data, data$Abschluss == "(Fach)Hochschulabschluss"))),
                border = "white",
                main = "(d) Höchster qualifizierender Abschluss",
                col = questionColors[2],
                ylab = "Häufigkeit",
                cex.names = 1.2,
                axes = FALSE,
                space = 0.1,
                ylim = c(0,300))
        axis(2, at = c(0,100,200,300))
        axis(1, at = bp, tick = FALSE, padj = 0.5, labels = c("ohne", "mittel", "hoch"))
        rm(bp)
        
      }
      
      ## REASON FOR USE #############################################################
      {
        
        bp = barplot(c(nrow(subset(data, data$Nutzung == "Erholung")), nrow(subset(data, data$Nutzung == "Verkehrsweg")), nrow(subset(data, data$Nutzung == "Arbeit"))),
                     border = "white",
                     main = "(e) Nutzungsgrund",
                     col = questionColors[2],
                     ylab = "Häufigkeit",
                     cex.names = 1.2,
                     axes = FALSE,
                     space = 0.1,
                     ylim = c(0,400))
        axis(2, at = c(0,200,400))
        axis(1, at = bp, tick = FALSE, padj = 0.5, labels = c("Erholung", "Verkehr", "andere"))
        rm(bp)
        
      }
      
      ## DISTANCE ##############################################################
      {

        bp = barplot(c(nrow(subset(data, data$Entfernung == "weniger als 5")), nrow(subset(data, data$Entfernung == "5 bis 15")), nrow(subset(data, data$Entfernung == "15 bis 30")), nrow(subset(data, data$Entfernung == "mehr als 30"))),
                border = "white",
                main = "(f) Entfernug zum Wohnort in Gehminuten",
                col = questionColors[2],
                ylab = "Häufigkeit",
                cex.names = 1.2,
                axes = FALSE,
                space = 0.1,
                ylim = c(0,300))
        axis(2, at = c(0,100,200,300))
        axis(1, at = bp, tick = FALSE, labels = c("< 5", "5 - 15", "15 - 30", "> 30"))
        rm(bp)
        
      }
      
      # export and finish
      dev.copy2pdf(file = paste0("output/", filename, "_row2.pdf"))
      dev.off()
      rm(filename, mat)
      cat(paste("\tDone!\n\n"))
      
    } # working
    
    
    ###########################
    ## DATA QUALITY PER AREA ##
    ###########################
    {
      
      ## print status
      cat(paste0("Creating data frame for data quality per area visualization..."))
      filename = "data_summary_per_area"
      df = data.frame(flaeche = vAreas,
                      n = NA,
                      gender = NA,
                      nationality = NA,
                      age_mean = NA,
                      qualification = NA,
                      distance = NA)
      
      for (i in 1:nrow(df))
      {
        
        tmp = subset(data, data$Flaeche == vAreas[i])
        df$n[i] = nrow(tmp)
        df$gender[i] = paste0(nrow(subset(tmp, tmp$Geschlecht == "w")),
                              ":",
                              nrow(subset(tmp, tmp$Geschlecht == "m")))
        df$nationality[i] = paste0(nrow(subset(tmp, tmp$Staatsangehoerigkeit == "deutsch")),
                                   ":",
                                   nrow(subset(tmp, tmp$Staatsangehoerigkeit != "deutsch")))
        df$age_mean[i] = mean(tmp$Alter)
        df$qualification[i] = paste0(nrow(subset(tmp, tmp$Abschluss == "(noch) ohne Abschluss")),
                                  ":",
                                  nrow(subset(tmp, tmp$Abschluss == "Lehre oder schulischer Abschluss")),
                                  ":",
                                  nrow(subset(tmp, tmp$Abschluss == "(Fach)Hochschulabschluss")))
        df$distance[i] = paste0(nrow(subset(tmp, tmp$Entfernung == "weniger als 5")),
                             ":",
                             nrow(subset(tmp, tmp$Entfernung == "5 bis 15")),
                             ":",
                             nrow(subset(tmp, tmp$Entfernung == "15 bis 30")),
                             ":",
                             nrow(subset(tmp, tmp$Entfernung == "mehr als 30")))
        
      }; rm(tmp, i)
      
      write.table(as.matrix(df),file=paste("output/", filename, "_matrix.csv", sep = ""),sep=";", col.names=TRUE, row.names=FALSE)
      rm(df, filename)
      
    }
    
  }
  
  ###################################################################################
  ## SCATTERPLOTS #########################################################
  ###############################################################
  {
    
    ##############################################
    ## EVALUATION VS EVALUATION (ALL QUESTIONS) ##
    ##############################################
    {
      
      # set up graphics device
      x11(1000,400)
      plot.new()
      par (mar = c (5,5,1,1), # margins
           mfrow = c(2,5),
           cex.lab = 2, # label font size
           cex.axis = 1.4) # axis font size
      
      # create data.frame
      df = data.frame(params = c("F1:F2", "F1:F3", "F1:F4", "F1:F5",
                      "F2:F3", "F2:F4", "F2:F5", "F3:F4", "F3:F5", "F4:F5"),
                      conf_level = NA, kendall_tau_b = NA, z_score = NA, p_value = NA)
      
      nPlots = 10
      iPlot = 0
    
      # loop through all questions
      for (i in 7:10)
      {
        
        headerX = colnames(data)[i]
        # do again, but skip correlations that have already been analyzed
        for (ii in (i+1):11)
        {
          
          iPlot = iPlot + 1
          headerY = colnames(data)[ii]
          filename = paste0("Scatterplot_", headerX, "_vs_", headerY)
          
          # print status
          cat(paste("Creating plot \"", filename, "\" "))
          cat(paste("with \"", headerX, "\" as independent variable"))
          cat(paste(" and \"", headerY, "\" as dependent variable.\n"))
          
          # test for correlation and write results to df
          for (iii in 1:nrow(df))
          {
            
            if (df$params[iii] == paste0("F", i-6, ":F", ii-6))
            {
              df$conf_level[iii] = 0.95
              
              # we can't compare values if one of them is NA, so we have to exclude
              # a participant in that case
              x <- vector(mode = "numeric", length = 0)
              y <- vector(mode = "numeric", length = 0)
              for (participant in 1:nrow(data))
              {
                
                if (is.na(data[participant,i]) || is.na(data[participant,ii])) next
                x <- c(x, data[participant,i])
                y <- c(y, data[participant,ii])
                
              }
              tau = cor.test(x,y, method = "kendall", conf.level = 0.95)
              df[iii,3:5] = round(tau$estimate, digits = 3)
              df$z_score[iii] = round(tau$statistic, digits = 3)
              df$p_value[iii] = tau$p.value
              rm(participant); rm(tau)
              
            }
            
          }; rm(iii)
          
          # create empty plot
          plot(x = c(x), y = c(y),
               type = 'n',
               axes = FALSE,
               ylab = paste0("F",ii-6),
               xlab = paste0("F",i-6),
               las = 2,
               ylim = c(0,11),
               xlim = c(0,11))
          
          # create y and x axis
          axis(side = 2,
               at = c(1,10),
               tick = FALSE,
               cex.axis = 1.7,
               line = FALSE)
          axis(side = 2,
               at = c(1:10),
               col = 'black',
               labels = FALSE)
          axis(side = 1,
               at = c(1,10),
               tick = FALSE,
               cex.axis = 1.7,
               line = FALSE)
          axis(side = 1,
               at = c(1:10),
               col = 'black',
               labels = FALSE)
          
          # add points of varying size
          points = data.frame(posX = c(x), posY = c(y), asString = paste0(c(x), c(y)), amount = 1)
          for (p in 1:nrow(points))
          {
            
            points$amount[p] = nrow(subset(points, points$asString == points$asString[p]))
            
          }
          points = unique(points)
          points(points$posX, points$posY, pch = 16, cex = 1 + points$amount/7, col = questionColors[2])
          rm(points); rm(p)
          
          # add regression line
          abline(lm(y~x), col = "black", lwd = 2)
          
          # add plot number
          text(1,10, paste0("(", letters[iPlot], ")"), cex = 2)
          
          # add legend
          model = subset(df, df$params == paste0("F", i-6, ":F", ii-6))
          legend ("bottom", inset = c(2, 0),
                  legend = bquote(paste("\u03c4"[b], " = ", .(model$kendall_tau_b), " (n = ", .(length(x)), ")")),
                  cex = 1.7,
                  bty = "n",
                  text.col = "black")
          rm(model)

        }
        
      }
      
      # print status
      cat(paste("Exporting to output directory..."))
      
      # export and finish
      dev.copy2pdf(file = paste0("output/", filename, ".pdf"))
      df = as.matrix(df)
      write.table(df,file=paste("output/", filename, "_matrix.csv", sep = ""),sep=";", col.names=TRUE, row.names=FALSE)
      
      # cleanup
      rm(filename, headerX, headerY, i, ii, x, y, nPlots, iPlot, df)
      dev.off()
      
    }
    
    ####################################
    ## ANSWERS VS AGE (ALL QUESTIONS) ##
    ####################################
    {
      
      # set up graphics device
      x11()
      plot.new()
      par (mar = c (5,5,2,2), # margins
           cex.lab = 1.4, # label font size
           cex.axis = 1.4) # axis font size
      
      # assign filename
      filename = "Scatterplot_Eval_vs_Age"
      
      # assign headers
      headerX = "Alter"
      headerY = "Bewertung"
      
      # print status
      cat(paste("Creating plot \"", filename, "\" "))
      cat(paste("with \"", headerX, "\" as independent variable"))
      cat(paste(" and \"", headerY, "\" as dependent variable.\n"))
      
      # create empty plot
      plot(x = 1, y = 1,
           type = 'n',
           axes = FALSE,
           ylab = "Antwortwert",
           xlab = "Alter",
           las = 2,
           ylim = c(0,11),
           xlim = c(10,100))
      
      # create y and x axis
      axis(side = 2,
           at = c(1:10),
           col = 'black')
      axis(side = 1,
           at = c(20,30,40,50,60,70,80,90,100),
           col = 'black')
      
      # create data.frame df
      df = data.frame(question = c("F1", "F2", "F3", "F4", "F5"),
                      n = NA,
                      mean = NA,
                      kendall_tau_b = NA)
      
      
      for (i in 1:nrow(df))
      {
        
        df$n[i] = length(data[,i+6])
        df$mean[i] = mean(data[,i+6], na.rm = TRUE)
        fit = lm(data[,i+6] ~ data$Alter)
        df$kendall_tau_b[i] = round(cor.test(data[,i+6], data$Alter, exact = TRUE)$estimate, digits = 3)
        
        # add points
        points(data[,i+6]~data$Alter,
               pch = 16, cex = 4.5 - (0.75 * i), col = questionColors[i])
        
        # add regression line
        abline(fit, col = questionColors[i], lwd = 2)
        
        # add legend
        text (90, 4-(i*0.66), # x and y
              labels = bquote(paste("F", .(df$question[i]), " (\u03c4"[b], " = ", .(df$kendall_tau_b[i]), ")")),
              cex = 1.2, col = questionColors[i])
        
      }
      
      # print status
      cat(paste("Exporting to output directory..."))
      
      # export and finish
      dev.copy2pdf(file = paste("output/", filename, ".pdf", sep = ""))
      df = as.matrix(df)
      write.table(df,file=paste("output/", filename, "_matrix.csv", sep = ""),sep=";", col.names=TRUE, row.names=FALSE)
      rm(filename, df, headerX, headerY, fit, i)
      dev.off()
      
      # print status
      cat(paste("\tDone!\n\n"))
      
    }
    
    #####################################
    ## ANSWERS VS TIME (ALL QUESTIONS) ##
    #####################################
    {
      
      # set up graphics device
      x11()
      plot.new()
      par (mar = c (5,5,2,2), # margins
           cex.lab = 1.4, # label font size
           cex.axis = 1.4) # axis font size
      
      # assign filename
      filename = "Scatterplot_Eval_vs_Time"
      
      # assign headers
      headerX = "Zeit"
      headerY = "Bewertung"
      
      # print status
      cat(paste("Creating plot \"", filename, "\" "))
      cat(paste("with \"", headerX, "\" as independent variable"))
      cat(paste(" and \"", headerY, "\" as dependent variable.\n"))
      
      # convert time to plottable format
      vTime = data$Zeit
      for (i in 1:length(vTime))
      {
        if (vTime[i]%%100 == 15) vTime[i] = vTime[i] - vTime[i]%%100 + 25;
        if (vTime[i]%%100 == 30) vTime[i] = vTime[i] - vTime[i]%%100 + 50;
        if (vTime[i]%%100 == 45) vTime[i] = vTime[i] - vTime[i]%%100 + 75;
      }
      
      # create empty plot
      plot(x = 1, y = 1,
           type = 'n',
           axes = FALSE,
           ylab = "Antwortwert",
           xlab = "Uhrzeit",
           las = 2,
           ylim = c(0,11),
           xlim = c(1000,1800))
      
      # create y and x axis
      axis(side = 2,
           at = c(1:10),
           col = 'black')
      axis(side = 1,
           at = c(1000,1100,1200,1300,1400,1500,1600,1700,1800),
           col = 'black')
      
      # create data.frame df
      df = data.frame(question = c("F1", "F2", "F3", "F4", "F5"),
                      n = NA,
                      mean = NA,
                      kendall_tau_b = NA)
      
      
      for (i in 1:nrow(df))
      {
        
        df$n[i] = length(data[,i+6])
        df$mean[i] = mean(data[,i+6], na.rm = TRUE)
        fit = lm(data[,i+6] ~ vTime)
        df$kendall_tau_b[i] = round(cor.test(data[,i+6], vTime, exact = TRUE)$estimate, digits = 3)
        
        # add points
        points(data[,i+6]~vTime,
               pch = 16, cex = 4.5 - (0.75 * i), col = questionColors[i])
        
        # add regression line
        abline(fit, col = questionColors[i], lwd = 2)
        
        # add legend
        text (1700, 4-(i*0.66), # x and y
              labels = bquote(paste("F", .(df$question[i]), " (\u03c4"[b], " = ", .(df$kendall_tau_b[i]), ")")),
              cex = 1.2, col = questionColors[i])
        
      }
      
      # print status
      cat(paste("Exporting to output directory..."))
      
      # export and finish
      dev.copy2pdf(file = paste("output/", filename, ".pdf", sep = ""))
      df = as.matrix(df)
      write.table(df,file=paste("output/", filename, "_matrix.csv", sep = ""),sep=";", col.names=TRUE, row.names=FALSE)
      rm(filename, df, headerX, headerY, fit, i)
      dev.off()
      
      # print status
      cat(paste("\tDone!\n\n"))
      
    }
    
  }
  
  # print status
  cat(paste0("All project code was run successfully. Warnings: ", length(warnings()), "\n"))
  
}