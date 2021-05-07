#Label Maker
#Keleigh Reynolds
#7/20/2020

library(dplyr)
#first run the .RMD for creating the itineraries, it will generate a dataframe to start this with.

#source("RMD/Itinerary_Generator.RMD") #just unhash this to get it to run here

#the dataframe you will start with is new.df, which is your trip name.csv from the original RMD.
trip_name<-"ErieNiag_Screening"
infile<-paste(trip_name,".csv",sep = "")
hab_number_start<-484   #You also need to put in the NUBMER TO START THE HABS from
SDG.d<-"091420" #AND the sample delivery group



#read in the file
file.path.in=file.path(here::here(),
                       "RMD/RMD_outputs",
                       infile)
new.df<-read.csv(file.path.in,stringsAsFactors = FALSE)


#first create a folder for the trip
file.path.out=file.path(here::here(),
                        "outputs",
                        trip_name)

dir.create(file.path.out)

#create simple file for quick lookups for excel
new.df<-new.df %>% 
  filter(SH_SITE_ID!="HOME") %>% 
  group_by(team,date)

short<-new.df %>% 
  select(team,date,order,SH_SITE_ID,SH_LATITUDE,SH_LONGITUDE) %>% 
  filter(SH_SITE_ID!="HOME")

################################################################################################
#LABELS<-  BUG and HABS
bugs<-new.df %>% 
  select(SH_SITE_ID,date) %>%
  rename("SITE_ID"=SH_SITE_ID,
         "DATE"=date) %>% 
  mutate(REP="REP: 1",SURVEY=trip_name) %>% 
  mutate(SAMP="SAMP TYPE:")

bugs<-bugs[rep(seq_len(nrow(bugs)), each = 2), ]
bugs$DATE<-as.Date.character(bugs$DATE,"%m/%d/%Y")
bugs$DATE<-format(bugs$DATE,"%m/__/%Y")

#HAB labels
habr<-new.df %>% 
  select(SH_SITE_ID) %>% 
  filter(SH_SITE_ID!="HOME") %>% 
  rename("SiteInfo"=SH_SITE_ID)

ufi<-habr %>% 
  select(SiteInfo) %>% 
  mutate(Sample_Type=paste0("RAW               LAB:UFI"))

esf<-habr %>% 
  select(SiteInfo) %>% 
  mutate(Sample_Type=paste0("RAW               LAB:ESF"))

#rbind these two together instead of creating repeats (below) to get 2 lines per place
#habr<-habr[rep(seq_len(nrow(habr)), each = 2), ]
habr<-rbind(ufi,esf)

habr<-habr %>% 
  arrange(team,date,SiteInfo)

#get the sequential numbers in there
i=nrow(habr)-1
seq.hab=hab_number_start+i

habr<-habr %>% 
  ungroup() %>% 
  mutate(Project="STREAM HABS SAMPLE",
         SiteID="BAS-LOC-RM:",
         SampleID=paste0("- _  _  _  _-20-R",seq(hab_number_start,seq.hab,1), sep = ""))


#scrape labels
habs<-new.df %>% 
  select(SH_SITE_ID) %>% 
  filter(SH_SITE_ID!="HOME") %>% 
  rename("SiteInfo"=SH_SITE_ID)

i2=nrow(habs)-1
next.hab=seq.hab+1
scr.hab=next.hab+i2

habs<-habs %>% 
  ungroup() %>% 
  mutate(Project="STREAM HABS SAMPLE",SiteID="BAS-LOC-RM:",
         SampleID=paste0("- _  _  _  _-20-S",seq(next.hab,scr.hab), sep = ""),
         Sample_Type=paste("SCRAPE               LAB:ESF"))

habs.final<-rbind(habr,habs)

habs.final<-habs.final %>% 
  arrange(team,date,SiteInfo) %>% 
  rename("Sample Type"=Sample_Type)

################################################################################################
#Chemistry labels

chem<-new.df %>% 
  select(SH_SITE_ID,team,date)

chem$date<-as.Date.character(chem$date,"%m/%d/%Y")
chem$new.date<-format(chem$date,"%m__%y")
chem$bl.date<-format(chem$date,"%m__%Y")


chem<-chem %>% 
  ungroup() %>% 
  mutate(SITE="SITE:",Year_Matrix=paste(chem$bl.date,"-W",sep = ""),Survey=trip_name,TIME="_____:_____",
         SDG="SDG:",SDG_=paste(SDG.d,"CO__W",sep = ""))

#limit to the columns I want         
chem.f<-chem %>% 
    select(SH_SITE_ID,team,date,SITE,Year_Matrix,Survey,TIME,SDG,SDG_)

#make sure there are 6 labels per site
chem.f<-chem.f[rep(seq_len(nrow(chem.f)), each = 6), ]

#create the blanks and sequential duplicate IDs
dates<-chem %>% 
  select(team,date) %>% 
  distinct()#get the dates
#read in both files
file.path.eq<-file.path(here::here(),
                     "eq_labels_blank.csv")
file.path.ms<-file.path(here::here(),
                        "ms_msd_labels_blanks.csv")


eq<-read.csv(file.path.eq,stringsAsFactors = FALSE)
ms<-read.csv(file.path.ms,stringsAsFactors = FALSE)

#for each date duplictate it a number of times
#firstcombine them
eq<-merge(eq,dates)
eq$thing<-paste(eq$team,eq$date,sep="_")

#bugs<-bugs[rep(seq_len(nrow(bugs)), each = 2), ]

#for (i in seq_along(eq$thing)){
 # eq<-eq[rep(seq_len(nrow(eq)), each = 6), ] #this makes too many of them.
#}
eq<-eq[rep(seq_len(nrow(eq)), each = 6), ] #this makes 6 per sequence (date/team), which is what we want

#take the column out cuz we don't need it now
eq$thing<-NULL
eq$SURVEY<-paste0("SCR-EQP BL")

teams<-chem %>% 
  select(team) %>% 
  distinct()#get the dates

ms<-merge(ms,teams) %>% 
  mutate(date=NA)


extras<-rbind(eq,ms)


bl.date<-unique(chem$bl.date)#get the blank date value to fill in what we need
extras$YEAR.MATRIX<-paste(bl.date,"-W",sep = "")
extras$SDG._<-paste(SDG.d,"CO__W",sep = "")

#rename to match
extras<-extras %>% 
  rename("SH_SITE_ID"=SITE.ID,
         "Year_Matrix"=YEAR.MATRIX,
         "Survey"=SURVEY,
         "SDG_"=SDG._)
  

#ok, now we have everything we need, so bind and then group.

chem.final<-rbind(extras,chem.f)
chem.final<-chem.final %>% 
  arrange(team,date)

chem.final<-chem.final %>% 
  rename("SITE ID"=SH_SITE_ID,
         "YEAR-MATRIX"=Year_Matrix,
         "SDG _"=SDG_)

################################################################################################

#write everything to files<- there are 3 files, 
#simple_site_info(Out)
#HAB and bug labels (OUT2)
#Chem labels(OUT_CHEM)

# Create a blank workbook(s)
library(openxlsx)


############################################OUt
OUT <- createWorkbook()#for lookups
# Add some sheets to the workbook

addWorksheet(OUT,"simple_site_info")

# Write the data to the sheets

writeData(OUT, sheet = "simple_site_info", x = short)
xlfile.path<-file.path(here::here(),
                       "outputs",
                       trip_name,paste(trip_name,".xlsx",sep = ""))
# Export the files
saveWorkbook(OUT, xlfile.path)


############################################OUT2
OUT2<-createWorkbook()#for all other labels

addWorksheet(OUT2,"BUG_labels")
addWorksheet(OUT2,"HAB_labels")

# Write the data to the sheets

writeData(OUT2, sheet = "BUG_labels", x = bugs)
writeData(OUT2, sheet = "HAB_labels", x = habs.final)

xlfile.path.label<-file.path(here::here(),
                             "outputs",
                             trip_name,paste(trip_name,"BUG_AND_HAB_LABELS.xlsx",sep = ""))

# Export the files
saveWorkbook(OUT2,xlfile.path.label)


############################################OUT_Chem

OUT_Chem<-createWorkbook()#for chem labels
addWorksheet(OUT_Chem,"chem_labels_all")


# Write the data to the sheets

writeData(OUT_Chem, sheet = "chem_labels_all", x = chem.final)

xlfile.path.label.chem<-file.path(here::here(),
                                  "outputs",
                                  trip_name,paste(trip_name,"_CHEM_LABELS.xlsx",sep = ""))


# Reorder worksheets
#worksheetOrder(OUT) <- c(2,1,)

# Export the files

saveWorkbook(OUT_Chem,xlfile.path.label.chem)




