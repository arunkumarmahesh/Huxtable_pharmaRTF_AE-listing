
######################################
######### installing packages
#####################################

#install.packages("huxtable") 

#install.packages("pharmaRTF")

#install.packages("tidyverse")

#install.packages("here")

#install.packages("haven")

packages <- c("huxtable", "pharmaRTF", "tidyverse", "here", "haven","dplyr","magrittr", "grid", "gridExtra")

##############################
#### Calling packages
##############################

lapply (packages, library, character.only = TRUE)

###############################################
##### Reading Datasets (Huxtable Basics)
###############################################

adae <- read_sas("adae.sas7bdat") 


data <- function(data) {
  
  
   data <- subset (data, select = c(CCS, ASR, AESER, AETERM, AEDECOD, AEBODSYS, AESTDTC, AEENDTC, AETOXGR, AERELN,
                                   AEACNOTN, AEACNN, AEOUTN, SAFFL, ASTDY, AENDY))
  
  
  data$AESTDTC <- as.Date( data$AESTDTC , format = "%Y-%m-%d") #date formating
  
  data$AEENDTC <- as.Date( data$AEENDTC, format = "%Y-%m-%d") #date formating
  
  data$DURATION <- data$AEENDTC - data$AESTDTC  #calculating days (start date- end date)
  
  data$AESTDTC <- as.character( data$AESTDTC) # setting to character
  
  data$AEENDTC <- as.character( data$AEENDTC) # setting to character
  
  data$AEENDTC <- ifelse(is.na(data$AEENDTC), "", data$AEENDTC) # Setting end date fields to blank if there is NA
  
  data$AENDY <- ifelse(is.na(data$AENDY), "", data$AENDY) # Setting end day fields to blank if there is NA
  
  ############################
  ### Sorting data
  ############################
  
  sort <- c("CCS", "AESTDTC", "AEENDTC") ##### select parameter names to sort in the dataset
  
  data <- arrange_(data, .dots = sort)
  
  
  #########################
  #### Filter
  #########################
  
  fil_ter <- quote(SAFFL %in% "Y")  
  
  ## If we want to filter any observation in the parameters we can use this step, here SAFFL is the parameter and "Y" is the observations
  
  data <- filter_(data, .dots = fil_ter) 
  
  #################################
  #### Concatenating parameters
  #################################
  
  PTTERM <- "PTTERM" 
  
  data <- mutate (data, !!PTTERM := paste (AETERM, AEDECOD, AEBODSYS, sep ="/ "))
  
  start_date <- "AEST" 
  
  data <- mutate (data, !!start_date := paste (AESTDTC, ASTDY, sep ="/ "))
  
  End_date <- "AEEND" 
  
  data <- mutate (data, !!End_date := paste (AEENDTC, AENDY, sep ="/ "))
  
  
  ###############################
  ##### selecting variable names
  ################################
  
  column_names1 <- c ("CCS", "ASR", "AESER", "PTTERM" , "AEST", "AEEND", "DURATION", "AETOXGR", "AERELN",
                      "AEACNOTN", "AEACNN", "AEOUTN")
  
  ## In this example we have taken adverse event	as a dataset and passed required parameter names which need to be derived further				
  
  data <- select_ ( data, .dots = column_names1) #### In this step the dataset is build with specified colum names above
 
  
}


data_adae <- data(adae)


##############################################
####### Rading dataframe to Huxtable format
##############################################
  
ht <- as_hux( data_adae , add_colnames=TRUE)

########################################
##### Renaming column names in huxtable
#######################################

ht[1, 1] <- "Country/\\line Subject\\line Identifier"                                    #CCS
ht[1, 2] <- "Age/\\line Sex/\\line Race"                                                 #ASR
ht[1, 3] <- "Serious\\line event"                                                        #aeser
ht[1, 4] <- "Reported term\\line Preferred term\\line System organ class"                #aeterm9
ht[1, 5] <- "Start\\line date\\line time/\\line day"                                     #aestdt9              
ht[1, 6] <- "End date\\line time/\\line End rel.\\line to ref./\\line day"               #aeendt9
ht[1, 7] <- "Dur\\line ation\\line (day)"                                                #adurn
ht[1, 8] <- "Toxi\\line city\\line Grade"                                                #aetoxgrs
ht[1, 9] <- "Cau\\line sal\\line ity"                                                    #aereln
ht[1, 10] <- "Med\\line or \\line ther.\\line taken"                                     #aeacnotn
ht[1, 11] <- "Action\\line taken\\line with\\line med"                                   #aeacnn
ht[1, 12] <- "Out\\line come"                                                            #aeoutn

huxtable::escape_contents(ht) <- FALSE  # Set escape contents to false to remove special characters



ht <- ht %>%
  huxtable::set_bottom_border(1, 1:ncol(ht1), 1) %>%         #Adding border line to column names
 # huxtable::set_align(1:nrow(ht1),3, 'right') %>%
 # huxtable::set_align(1:nrow(ht1),6, 'right') %>%
  huxtable::set_align(1:nrow(ht1),10, 'right') %>%           # Setting variable to centre
  huxtable::set_align(1:nrow(ht1),11, 'right') %>%           # Setting Variable to centre
  huxtable::set_align(1:nrow(ht1),12, 'right') %>%           # Setting varibale to centre
  huxtable::set_col_width(c(rep(.27,each=3),.4,rep(.27,each=8)))  # setting column widths

ht1 <- as_hux(ht)   #creating new object ht1

doc <- rtf_doc(ht1, titles=list(hf_line("Adverse events ", "PAGE_FORMAT: Page %s of %s"))) %>%  # adding titles
  add_titles(hf_line("<analysis set>")) %>%
  add_titles(hf_line("Subset: <subset id name>",align = 'left')) %>%
  add_titles(hf_line("Actual Treatment: XXXXXXXXXX", align = 'left')) %>%
  add_footnotes(hf_line("- Day is relative to the reference start date.  Missing dates are based on imputed dates.", align = 'left')) %>%
  add_footnotes(hf_line("- Severity: MILD=Mild, MOD=Moderate, SEV=Severe.", align = 'left')) %>%
  add_footnotes(hf_line("- Relationship to study treatment (causality): 1=No, 2=Yes, 3= Yes, investigational treatment,", align = 'left')) %>%
  add_footnotes(hf_line("4= Yes, other study treatment (non-investigational), 5= Yes, both and/or indistinguishable.", align = 'left')) %>%
  add_footnotes(hf_line("- Action taken: 1=None, 2=Dose adjusted, 3=Temporarily interrupted, 4=Permanently discontinued, 997=Unknown, 999=NA.", align = 'left')) %>%
  add_footnotes(hf_line("- Outcome: 1=Not recovered/not resolved, 2=recovered/resolved, 3=recovering/resolving,", align = 'left')) %>%
  add_footnotes(hf_line("4=recovered/resolved with sequelae, 5=Fatal, 997=Unknown.", align = 'left')) %>%
  add_footnotes(hf_line("- Medication or therapies taken: 1=No concomitant medication or non-drug therapy, 10=Conc. Med. or non-drug therapy", align = 'left')) %>%
  add_footnotes(hf_line("- Time to AE = Time of AE onset minus time of the most recent dose.", align = 'left')) %>%
  add_footnotes(hf_line( "DATE_FORMAT: %Y-%m-%dT%H:%M:%S", align='left')) %>%
  set_pagesize(c(height=8.27, width=11.69)) %>% 
  set_font('Courier New') %>% 
  set_margins(c(top=1.18, bottom=0.58)) %>% 
  set_font_size(9) %>%
  set_header_rows(1) 
  #set_footer_height(2)
  #set_column_header_buffer(top=1, bottom = 1) 

write_rtf(doc, file='AEL001.rtf')
