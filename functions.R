#Important equations:
#  RQ = CO2 eliminated/O2 consumed
#  EE = 0.06 * (3.941 * VO2 + 1.106 * VCO2)
#  
#  from 14.4 in Leighton book
#  LabDiet 5015 = (26.101/100).71+(19.752/100).83+.54148 = .8907387
#  LabDiet low fat 5015 = (22.8/100).71+(6.6/100).83+.706 = .92266


#import data function
bring_in_data <- function(data_file, Sex)
{
  data <- paste(path, data_file,sep="")
  raw <- read_csv(data,
                  col_types = cols(Animal = col_double(),
                                   deltaCO2 = col_double(), 
                                   deltaH2O = col_double(),
                                   H2Oml = col_double(),
                                   Deg_C = col_double(),
                                   VCO2 = col_double(),
                                   StartTime = col_time(format = "%H:%M:%S")))
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  raw <- raw %>% mutate(animal = round(Animal, digits=0)) %>% mutate(Animal = NULL) 
  raw <- raw %>% mutate(animal = ifelse(animal >= 4, animal-1, animal))
  
  raw <- raw %>% mutate(StartDate = as.Date(StartDate, format = "%m/%d/%Y"))
  
  raw <- raw %>% 
    mutate(EE_kJH = 4.1868*0.06*(3.941*VO2 + 1.106*VCO2)) %>% 
    mutate(RQ = VCO2/VO2) %>%
    mutate(Sex  = Sex) %>%
    mutate(StartDate, StartDate = as.POSIXlt(StartDate, format = "%d-%b-%y", tz="EST")) %>%
    unite("DateTime", StartDate:StartTime, remove = FALSE, sep =  " ") %>%
    mutate(weight_0 = 
             ifelse(animal == 0 & StartDate == startdate, cageweight00, 
                    ifelse(animal == 1 & StartDate == startdate, cageweight01,
                           ifelse(animal == 2 & StartDate == startdate, cageweight02,
                                  ifelse(animal == 3 & StartDate == startdate, cageweight03,
                                         ifelse(animal == 4& StartDate == startdate, cageweight04,
                                                ifelse(animal == 5& StartDate == startdate, cageweight05,NA
                                                       )))))))%>% 
    mutate(body_temp0 = 
             ifelse(animal == 0 & StartDate == startdate, bodytemp00, 
                    ifelse(animal == 1& StartDate == startdate, bodytemp01,
                           ifelse(animal == 2& StartDate == startdate, bodytemp02,
                                  ifelse(animal == 3& StartDate == startdate, bodytemp03,
                                         ifelse(animal == 4& StartDate == startdate, bodytemp04,
                                                ifelse(animal == 5& StartDate == startdate, bodytemp05,NA
                                                ))))))) %>%
    mutate(weight_1 = 
       ifelse(animal == 0 & StartDate == startdate +1, cageweight10, 
           ifelse(animal == 1& StartDate == startdate +1, cageweight11,
                  ifelse(animal == 2 & StartDate == startdate +1, cageweight12,
                         ifelse(animal == 3 & StartDate == startdate+1, cageweight13,
                                ifelse(animal == 4 & StartDate == startdate+1, cageweight14,
                                       ifelse(animal == 5 & StartDate == startdate+1, cageweight15,NA
                                              ))))))) %>% 
    mutate(body_temp1 = 
             ifelse(animal == 0& StartDate == startdate +1, bodytemp10, 
                    ifelse(animal == 1& StartDate == startdate +1, bodytemp11,
                           ifelse(animal == 2& StartDate == startdate +1, bodytemp12,
                                  ifelse(animal == 3& StartDate == startdate +1, bodytemp13,
                                         ifelse(animal == 4& StartDate == startdate +1, bodytemp14,
                                                ifelse(animal == 5& StartDate == startdate +1, bodytemp15,NA
                                                ))))))) %>%
    
    mutate(weight_2 = 
    ifelse(animal == 0& StartDate == startdate +2, cageweight20, 
           ifelse(animal == 1& StartDate == startdate +2, cageweight21,
                  ifelse(animal == 2& StartDate == startdate +2, cageweight22,
                         ifelse(animal == 3& StartDate == startdate +2, cageweight23,
                                ifelse(animal == 4& StartDate == startdate +2, cageweight24,
                                       ifelse(animal == 5& StartDate == startdate +2, cageweight25,NA
                                              ))))))) %>% 
    mutate(body_temp2 = 
             ifelse(animal == 0& StartDate == startdate +2, bodytemp20, 
                    ifelse(animal == 1& StartDate == startdate +2, bodytemp21,
                           ifelse(animal == 2& StartDate == startdate +2, bodytemp22,
                                  ifelse(animal == 3& StartDate == startdate +2, bodytemp23,
                                         ifelse(animal == 4& StartDate == startdate +2, bodytemp24,
                                                ifelse(animal == 5& StartDate == startdate +2, bodytemp25,NA
                                                ))))))) %>%
    mutate(weight_3 = 
    ifelse(animal == 0& StartDate == startdate +3, cageweight30, 
           ifelse(animal == 1& StartDate == startdate +3, cageweight31,
                  ifelse(animal == 2& StartDate == startdate +3, cageweight32,
                         ifelse(animal == 3& StartDate == startdate +3, cageweight33,
                                ifelse(animal == 4& StartDate == startdate +3, cageweight34,
                                       ifelse(animal == 5& StartDate == startdate +3, cageweight35,NA
                                              ))))))) %>% 
    mutate(body_temp3 = 
   ifelse(animal == 0& StartDate == startdate +3, bodytemp30, 
          ifelse(animal == 1& StartDate == startdate +3, bodytemp31,
                 ifelse(animal == 2& StartDate == startdate +3, bodytemp32,
                        ifelse(animal == 3& StartDate == startdate +3, bodytemp33,
                               ifelse(animal == 4& StartDate == startdate +3, bodytemp34,
                                      ifelse(animal == 5& StartDate == startdate +3, bodytemp35,NA
                                             ))))))) %>%
    mutate(Animal_ID = 
             ifelse(animal == 0, animalID0, 
                    ifelse(animal == 1, animalID1,
                           ifelse(animal == 2, animalID2,
                                  ifelse(animal == 3, animalID3,
                                         ifelse(animal == 4, animalID4,
                                                ifelse(animal == 5, animalID5,NA
                                                       ))))))) %>% 
    mutate(H2Omg_edit = 
             ifelse(hour(StartTime) == 8, H2Omg,
                    ifelse(hour(StartTime) == 7, H2Omg,
                           ifelse(hour(StartTime) == 9, H2Omg,
                                  ifelse(hour(StartTime) == 10, H2Omg,
                                         ifelse(hour(StartTime) == 19, H2Omg,
                                                ifelse(hour(StartTime) == 20, H2Omg,
                                                       ifelse(hour(StartTime) == 21, H2Omg,
                                                              ifelse(hour(StartTime) == 22, H2Omg,
                                                                     ifelse(hour(StartTime) %!in% c(7,8,9,10,20,21,22,19), H2Omg, NA)))))))))) %>% 
    mutate_at("H2Omg_edit", as.numeric)
  
  #metric <- "corEE"
  target <- c(0,1,2,3,4,5)
  cages <- raw %>% filter(animal %in% target)
  
  
  #start_time <- ymd_hms(subset[[5]][1])
  #begin_experiment <- start_time + dhours(2)
  #end_time <- begin_experiment + dhours(72)
  #filtered <- subset %>% filter(raw$DateTime >= begin_experiment & raw$DateTime <= end_time)
  
  
  return(cages)
}


#mouse id and weights function
#will add electrolytes....one day
weight_ID <- function(date)
{  
  subset <- electrolyte_data[which(electrolyte_data$experiment_date == date), names(electrolyte_data) %in% c("sex", "mouse_ID", "rep", "cage_ID", "weight0", "weight1", "weight2", "weight3", "body_temp_0", "body_temp_1", "body_temp_2", "body_temp_3", "Na", "K", "Cl", "TCO2", "BUN", "Crea", "Glu", "iCa", "AnGap", "Hct", "Hb*")]
  
  ids <- subset$mouse_ID
  weight0 <- as.double(subset$weight0)
  weight1 <- as.double(subset$weight1)
  weight2 <- as.double(subset$weight2)
  weight3 <- as.double(subset$weight3)
  body_temp0 <- as.double(subset$body_temp_0)
  body_temp1 <- as.double(subset$body_temp_1)
  body_temp2 <- as.double(subset$body_temp_2)
  body_temp3 <- as.double(subset$body_temp_3)
  
  x = 0
  
  for (i in 1:length(ids))
  {
    assign(paste("animalID", x, sep = ""), ids[i], envir = parent.frame())
    
    assign(paste("cageweight0", x, sep = ""), weight0[i], envir = parent.frame())
    assign(paste("cageweight1", x, sep = ""), weight1[i], envir = parent.frame())
    assign(paste("cageweight2", x, sep = ""), weight2[i], envir = parent.frame())
    assign(paste("cageweight3", x, sep = ""), weight3[i], envir = parent.frame())
    
    assign(paste("bodytemp0", x, sep = ""), body_temp0[i], envir = parent.frame())
    assign(paste("bodytemp1", x, sep = ""), body_temp1[i], envir = parent.frame())
    assign(paste("bodytemp2", x, sep = ""), body_temp2[i], envir = parent.frame())
    assign(paste("bodytemp3", x, sep = ""), body_temp3[i], envir = parent.frame())
    
    x = x + 1
  }
} 


#merge data and subset for 72 hours function
merge_data <- function(cage)
{
  start_time <- ymd_hms(cage[[1]][1])
  begin_experiment <- start_time + dhours(2)
  end_time <- begin_experiment + dhours(72)
  filtered <- cage %>% filter(DateTime >= begin_experiment & DateTime <= end_time)
  
  return(filtered)
}
  
  save <- function(name, fig_len, fig_width,plot_name)
  {
    library(Cairo)
    
    Cairo::Cairo(
      fig_len, #length
      fig_width, #width
      file = paste(name, ".png", sep = ""),
      type = "png", #tiff
      bg = "transparent", #white or transparent depending on your requirement 
      dpi = 300,
      units = "cm" #you can change to pixels etc 
    )
    plot(plot_name) #p is your graph object 
    dev.off()
  }
  
  