1
#just setting a list of colors for the aesthetic
burnLevelColors = c("#bf4026", "#d99116", "#149132")
burnColors = c("#bf4026", "#149132")


#list of all qualitative x variables
qualXvars = c("BurnLevel","Burn","Code")
basicVars = list("Date Time" = "datetime", "Drainage Area" = "DRNAREA",
     "Discharge" = "discharge_m3_sec", "Burn Level" = "BurnLevel",
     "Burned or not" = "Burn","Site Id" = "Code")
SCANVars = list("Turbidity" = "turbidity_FTU_SCAN","No3" = "NO3_mg_L_SCAN", 
     "TOC" = "TOC_mg_L_SCAN", "DOC" = "DOC_mg_L_SCAN")
YSIVars = list("Temperature" = "temp_C_YSI","Pressure" = "pressure_mmHg_YSI",
               "Dissolved Oxygen %" = "DO_perc_YSI","Disolved Oxygen mg/L" = "DO_mg_L_YSI", 
               "Conductivity" = "SPConductivity_uS_cm_YSI","pH" = "pH_YSI", 
               "ORP" = "ORP_mV_YSI","Chlorophyll" = "chl_ug_L_YSI", 
               "Chlorophyll RFU" = "chl_RFU_YSI", "PC" = "PC_ug_L_YSI")


#reads in data and returns a table with alldata
readInData <-function(filePathToData){ # read in data from Data Tiger and store it in a tibble 
  
  chemistry.alldata <- read_csv(("megafire_point_samples.csv"), col_names = TRUE) # reading in most recent data 
  #changing datetime column to actual dates
  chemistry.alldata$datetime = as.POSIXct(chemistry.alldata$datetime, format = "%m/%d/%y %H:%M")
  return(chemistry.alldata)
}


#returns a table created by filtering the entered table to only data from the siteID/IDs
createSiteTable <- function(table,siteIDList){
  return(table %>% filter(site_id %in% siteIDList))
}


#returns a table created by filtering the entered table to only data from between the two dates
createTimeTable <- function(table,startTime,endTime){
  startTime <- as.POSIXct(startTime)
  endTime <- as.POSIXct(endTime)
  return(table %>% filter(datetime > startTime & datetime < endTime))
}


#sets outlier limits for a single vector based on SD, takes in the column (as a vector) and sd to use
getLimitsSD <- function(col,SDtoUse=3){
  #the code below was an attempt to use IQR to find outliers, it worked better for one or two varibles but was way worse for eveyrthing else
  # median = median(chemistry.alldata$pH_YSI, na.rm=TRUE)
  # IQR = IQR(chemistry.alldata$pH_YSI, na.rm=TRUE)
  # 
  # upper = median + IQR/2 + 1.5*IQR
  #lower = median + IQR/2 - 1.5*IQR
  
  upper=mean(col,na.rm=TRUE)+SDtoUse*sd(col,na.rm=TRUE)
  lower=mean(col,na.rm=TRUE)-SDtoUse*sd(col,na.rm=TRUE)
  if(upper > max(col,na.rm=TRUE)){
    upper = max(col,na.rm=TRUE)
  }
  if(lower < min(col,na.rm=TRUE)){
    lower = min(col,na.rm=TRUE)
  }

  return(c(lower,upper))
}

 
#sets outlier limits for a single vector based on IQR, takes in the column (as a vector)
getLimitsIQR <- function(col){
  
  median = median(col, na.rm=TRUE)
  IQR = IQR(col, na.rm=TRUE)

  upper = median + IQR/2 + 1.5*IQR
  lower = median - IQR/2 - 1.5*IQR
  
  if(upper > max(col,na.rm=TRUE)){
    upper = max(col,na.rm=TRUE)
  }
  if(lower < min(col,na.rm=TRUE)){
    lower = min(col,na.rm=TRUE)
  }
  
  return(c(lower,upper))
}


#creates list of plots by cycling through the combinations of x and y variables
createScatterPlotList <- function(xVars, yVars, df=chemistry.alldata, xVarsLabels=xVars, yVarsLabels=yVars){
  
  #initializing empty plotList
  plotList = list()
  counter = 0
  
  #for loop that runs through the # of quantitative explanatory variables
  for(j in 1:length(xVars)){
    #for loop that runs the # of response variables present and 
    #plots each y variable vs the one x variable
    for(i in 1:length(yVars)){
      
      #creating variable limits
      xCol=pull(chemistry.alldata,xVars[j])
      yCol=pull(chemistry.alldata,yVars[i])
      xLims=NULL
      yLims=NULL
      if(is.numeric(xCol)){#double checks that the variables are numeric otherwise getLimits won't work
        xLims=getLimitsIQR(xCol)
      }
      if(is.numeric(yCol)){#double checks that the variables are numeric otherwise getLimits won't work
        yLims=getLimitsIQR(yCol)
      }
      
      #plotting with labels from the label lists
      plot <- df %>%
        ggplot(aes_string(xVars[j],yVars[i],color="BurnLevel")) + geom_point()+
        coord_cartesian(xlim = xLims, ylim = yLims)+
        scale_color_manual(values=burnLevelColors)+
        xlab(xVarsLabels[j])+ylab(yVarsLabels[i])+
        labs(title=paste(yVarsLabels[i], " explained by ",xVarsLabels[j],sep = ""))
      #incrementing
      counter = counter + 1
      #saving to plotList
      plotList[[counter]] <- plot
    }
  }
  return(plotList)
}


#creates list of plots by cycling through the combinations of x and y variables
createBoxPlotList <- function(xVars, yVars, df=chemistry.alldata, xVarsLabels=xVars, yVarsLabels=yVars){
  
  #initializing empty plotList
  plotList = list()
  counter = 0
  
  #for loop that runs through the # of qualitative explanatory variables
  for(j in 1:length(xVars)){
    #for loop that runs the # of response variables present and 
    #plots each y variable vs the one x variable
    for(i in 1:length(yVars)){
      
      #creating variable limits
      yCol=pull(chemistry.alldata,yVars[i])
      xCol=pull(chemistry.alldata,xVars[j])
      yLims=NULL
      if(is.numeric(yCol)){#double checks that the variables are numeric otherwise getLimits won't work
        yLims=getLimitsIQR(yCol)
      }
      
      #plotting with labels from the label lists
      plot <- df %>%
        filter(!is.na(xVars[j])) %>% #filtering out any rows with nas under the xVar
        ggplot(aes_string(xVars[j],yVars[i],fill=xVars[j])) + geom_boxplot() +
        coord_cartesian(ylim = yLims)+
        xlab(xVarsLabels[j])+ylab(yVarsLabels[i])+
        labs(title=paste(yVarsLabels[i], " explained by ",xVarsLabels[j],sep = "")) + 
        theme(axis.text.x = element_blank(),       #removing tick marks and tick labels (the key will be on the right )
              axis.ticks = element_blank())
      
      #checking to see if the x variable has to many categories for the color pallete (if so, it just assigns it a nice rainbow gradient)
      plot <- if(length(unique(xCol)) < 8){
        plot + scale_fill_brewer(palette="YlOrRd",direction = -1)
      }else{
        plot + scale_colour_gradientn(colours=rainbow(10))
      }

      #increment
      counter = counter + 1
      #saving to plotList
      plotList[[counter]] <- plot
    }
  }
  return(plotList)
}


#exports a list of plots (like  one returned by createPlotList) to a pdf
exportPlotList <- function(plotList,exportName){
  #opening pdf connection
  pdf(file=paste("~/Library/CloudStorage/Box-Box/Lee Megafire/Figures/plots/",exportName, sep = ""))
  #going through list of plots
  print(plotList)
  #closing connection
  dev.off()
}



getLabel <- function(variable){
  if(variable %in% SCANVars){
    return(names(SCANVars)[match(variable,SCANVars)])
  }
  else if(variable %in% basicVars){
    return(names(basicVars)[match(variable,basicVars)])
  }
  else if(variable %in% YSIVars){
    return(names(YSIVars)[match(variable,YSIVars)])
  }
  
}


