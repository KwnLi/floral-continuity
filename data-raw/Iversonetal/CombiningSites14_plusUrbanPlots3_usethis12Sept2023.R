#HERE'S THE WORKFLOW (2024 UPDATED). GO TO COMBININGSITES17_USETHIS19Aug2022.R AND CALCULATE THE VALUES FOR URBAN_FLRS (CURRENTLY LINE 120). THEN, SAVE AS CSV AND IMPORT INTO THIS CODE HERE. THEN RUN THE FILES WITH ALL THE UPDATED BLOOM TIME, FLOWERS, FLORAL AREA. CHANGE PARTS OF COMBININGSITES17 AND THIS CODE FOR INSECT-POLLINATED VS. ALL PLANTS. For aggregated resources around random points, go to random_pointsJul2022_subset_landscapes (for subset of 12 sites). For 100 random sites, use the file 'random_pointsJul2022'.
#for final curves (fig 3, 4 in paper) take output file="habitat_mean_WIP_29May2024.csv and import into 'habitat_plotsv2_KevinDec2022_v5.R'. use this to make curves. Make sure to separate IP vs. WIP in all datasets.
#get soy, apple, corn values from end of this file and input those into 'all_habs' file manually.('habitat_mean_WIP_Jun2024')
setwd("/Users/aiverson/OneDrive - St. Lawrence University/Simonin/Current spreadsheets to use")

#install.packages('dplyr')
library(dplyr)
library(readxl)
library(ggplot2)
library(magrittr)#part of dplyr package
library(RColorBrewer)

#READING-IN DATA 
#1) Floral density:
fl.density<-read.csv("Flowers_29May2024.csv", na.strings = "#DIV/0!", skip=1)  #This DIV/O addition changes div/o into NA values
fl.density$Genus<-as.factor(trimws(fl.density$Genus))#get rid of extra spaces after names
fl.density$species<-as.factor(trimws(fl.density$species))
#levels(fl.density$Genus)#2 less genera in this spreadsheet. Agrimonia eupatoria is extra, not in fl.density

#2) Floral area:
fl.area<-read.csv("FloralArea_29May2024.csv")
fl.area$Genus<-as.factor(trimws(fl.area$Genus))
fl.area$species<-as.factor(trimws(fl.area$species))
#levels(fl.area$Genus)
#3Bloom dates:
bloom.date<-read.csv("bloom_period_29May2024.csv", skip=1)
bloom.date$Genus<-as.factor(trimws(bloom.date$Genus))
bloom.date$species<-as.factor(trimws(bloom.date$species))
#levels(bloom.date$Genus)

#4) sex ratios
sex_ratios<-read.csv("Kass_flowers_33_ratios_v4.csv", skip=1)#updated Oct 25, 2022
sex_ratios$Genus<-as.factor(trimws(sex_ratios$Genus))
sex_ratios$species<-as.factor(trimws(sex_ratios$species))

#CREATING JULIAN DAY RANGE
Jdate <- 1:365

#READ EXCEL FILE OF SITES
#SitesExcel <- read_excel("Plots_Final_2016_26.xlsx")
#SitesExcel<-SitesExcel[,-c(29,31,33,35,37,39)]#added sept 2023
#need to delete first and last worksheets
SiteSheetList <- excel_sheets("Plots_Final_2016_27.xlsx")

#CREATING BLANK DATA FRAME FOR SUMMED CURVES AT EACH SITE
#each row is the site (column is called 'site' and in it is tab name)
site.summaries <- data.frame(site=SiteSheetList)
site.summaries$habitat<-substr(site.summaries$site, start=1, stop=regexpr("_| ", site.summaries$site)-1) #this takes the tab and names the sites based on the tab by stopping at the space or the _ in the tab text. regexpr indexes where the space is, we don't want to include the space, so we subtract 1. 
#site.summaries$habitat<-substr(site.summaries$site, start=1, stop=2)#this would give name of first 2 characters.


site.summaries[paste("d", 1:365, sep="")]<-NA #default is to add space, so sep is to get rid of space. 
site.summaries$richness<-NA#to make column which we will fill with richness data
#i<-SiteSheetList[115] #to get 3rd site in sitesheetlist
#
for (i in SiteSheetList){
  #read sheet
  thissite<-read_excel("Plots_Final_2016_27.xlsx", sheet=i, na=c("", 'tree', 'sap', 'Tree', 'Sap', '(sap)', '(tree)', 's', 't'), n_max=661)#some subplots have 'tree' or 'sap', etc instead of number (actually 0 value), n_max means only read first 661 rows
  #thissite<-thissite[,-c(29,31,33,35,37,39)]#added sept 2023 trying to remove columns from each spreadsheet, not just one.
  thissite<-thissite[,-grep(pattern="in.flower|flowers|X|Bloom_guide|unidentified|notes", names(thissite))]#gets rid of superfluous columns
  thissite$Genus<-as.factor(trimws(thissite$Genus))
  thissite$species<-as.factor(trimws(thissite$species))
  #levels(thissite$Genus)

#create column "avg_cov"
##I could use the following if I didn't want to double-count plots A-E if they were already counted in the subplots. This is read 'if (first chunk), then (second chunk), otherwise (third chunk)
  
 # test1<-ifelse(rowSums(thissite[c("subplot_1","subplot_2","sublplot_3","sublplot_4","sublplot_5","sublplot_6","sublplot_7","sublplot_8","sublplot_9","sublplot_10")], na.rm=TRUE)!=0, (rowSums(thissite[c("subplot_1","subplot_2","sublplot_3","sublplot_4","sublplot_5","sublplot_6","sublplot_7","sublplot_8","sublplot_9","sublplot_10")], na.rm=TRUE)/10/100*250*40), (rowSums(cbind(thissite$`Plot A`*0.0083*10000, thissite$`Plot B`*0.0059*10000, thissite$`Plot C`*0.0009*10000,thissite$D*0.0022*10000,thissite$E*0.0007*10000), na.rm=TRUE)))
  
#however, I do want to include A-E data even if it was recorded in subplots--so, this will weight the plants that are in subplots and A-E higher than if just in subplots. The code above would only count subplot data if it is found in a subplot.

  thissite$avg_cov<-rowSums(thissite[c("subplot_1","subplot_2","sublplot_3","sublplot_4","sublplot_5","sublplot_6","sublplot_7","sublplot_8","sublplot_9","sublplot_10")], na.rm=TRUE)/10/100*250*40 + rowSums(cbind(thissite$`Plot A`*0.0083*10000, thissite$`Plot B`*0.0059*10000, thissite$`Plot C`*0.0009*10000,thissite$D*0.0022*10000,thissite$E*0.0007*10000), na.rm=TRUE)  

  #left join with floral density and floral area and bloom date
    thissite2<-left_join(thissite,fl.density[c("Genus", "species", "Insect_pollinated", "Avg_Flrs_1m", "Avg_Flrs_Tree")], by=c("Genus"="Genus", "species"="species"))
    
  ##############
  ###HAD TO ADD THIS 1 OCT 2019 BC AVG_FLRS_1M AND AVG_FLRS_TREE WERE COMING UP AS FACTORS!
  thissite2$Avg_Flrs_1m<-as.numeric(as.character(thissite2$Avg_Flrs_1m))
  thissite2$Avg_Flrs_Tree<-as.numeric(as.character(thissite2$Avg_Flrs_Tree))
  ##############
  
  thissite3<-left_join(thissite2,fl.area[c("Genus", "species", "area.per.flower.mm2")], by=c("Genus"="Genus", "species"="species")) 
  
  thissite4<-left_join(thissite3,bloom.date[c("Genus", "species", "Start_final", "End_final")], by=c("Genus"="Genus", "species"="species")) 
  
  sum(thissite4$avg_cov>0)#count the number of trues

#adding in sex ratios  

  thissite4.5<-left_join(thissite4,sex_ratios[c("Genus", "species", "Mult_factor_final")], by=c("Genus"="Genus", "species"="species")) 
  
  thissite4.5$Avg_Flrs_Tree_Sexed<-thissite4.5$Avg_Flrs_Tree*thissite4.5$Mult_factor_final #this multiplies by the sex ratio multiplication factor
  
  #Calculate Curves 
  ###IMPORTANT--CHANGE BASED ON CALCULATING INSECT POLLINATED ONLY OR WIND AND INSECT POLLINATED
  #Add this if want just insect-pollinated
 # thissite5<-thissite4.5[thissite4.5$Insect_pollinated==1,]  
  #otherwise use this:
 thissite5<-thissite4.5  
  
  #thissite5$peakarea<-thissite5$Avg_Flrs_1m*thissite5$area.per.flower.mm2*thissite5$avg_cov #this is to use if no trees are included
  
  #use the following if including trees
  thissite5$peakarea.herb<-thissite5$Avg_Flrs_1m*thissite5$area.per.flower.mm2*thissite5$avg_cov#This results in mm2 floral area per ha 
  thissite5$peakarea.tree<-thissite5$No_canopy_trees*40*thissite5$area.per.flower.mm2*thissite5$Avg_Flrs_Tree_Sexed#multiply by 40 to get mm2 floral area per ha
  thissite5$peakarea<-rowSums(cbind(thissite5$peakarea.herb, thissite5$peakarea.tree), na.rm=TRUE)
  
  #the following 4 lines define the bloom curve for each species with a normal distribution across the growing season 
  thissite5$span<-thissite5$End_final-thissite5$Start_final
  thissite5$peakjday<-thissite5$Start_final+thissite5$span/2
  thissite5$periodsd<-thissite5$span/6
  thissite5$mult <- thissite5$peakarea/dnorm(thissite5$peakjday, mean=thissite5$peakjday, sd=thissite5$periodsd)
  
  thissite5[paste("d", 1:365, sep="")]<-NA #makes empty dataframe, the paste command puts a d (day) in front of 1 to 365 with no space (sep="")
  
#Sum curves, this takes each plot and sums the amount of resources per day per species
  for (j in 1:nrow(thissite5)){ #a for loop within a for loop
    thissite5[j,paste("d", 1:365, sep="")]<-as.list(dnorm(Jdate, mean=thissite5$peakjday[j], sd=thissite5$periodsd[j])*thissite5$mult[j])
  }#SEP 2023 HAD TO ADD THE 'AS.LIST' TO GET IT TO WORK
  site.summaries[which(site.summaries$site==i),paste("d", 1:365, sep="")]<-colSums(thissite5[,paste("d", 1:365, sep="")], na.rm=TRUE)/1000000#divide by 1 million to go from mm2 to m2 per ha
  #site.summaries now is each row is a different site and each column is a day
  site.summaries$richness[which(site.summaries$site==i)]<-sum(thissite4$avg_cov>0)
}


########################################

#trouble shooting dataset
#to see what the output is in a given cell(s). This works (jan 3, 2024)
#used to find why certain species didn't have resources--were they recorded in dataset?
#first for trees
results <- list()#make sure to run the anew each time you run the for loop
for (i in SiteSheetList){
  #read sheet
  thissite<-read_excel("Plots_Final_2016_27.xlsx", sheet=i, na=c("", 'tree', 'sap', 'Tree', 'Sap', '(sap)', '(tree)', 's', 't'), n_max=661)#some subplots have 'tree' or 'sap', etc instead of number (actually 0 value), n_max means only read first 661 rows
  thissite<-thissite[,-grep(pattern="in.flower|flowers|X|Bloom_guide|unidentified|notes", names(thissite))]#gets rid of superfluous columns
  thissite<-thissite[610,21]#col 21 is No_canopy trees. change the row column depending on the species I'm looking at.
  results <- append(results, thissite)
}
paste0(results,collapse=",")#condensed output

#used to find why certain species didn't have resources--were they recorded in dataset?
#for non-trees 
results <- list()#make sure to run the anew each time you run the for loop
for (i in SiteSheetList){
  #read sheet
  thissite<-read_excel("Plots_Final_2016_27.xlsx", sheet=i, na=c("", 'tree', 'sap', 'Tree', 'Sap', '(sap)', '(tree)', 's', 't'), n_max=661)#some subplots have 'tree' or 'sap', etc instead of number (actually 0 value), n_max means only read first 661 rows
  thissite<-thissite[,-grep(pattern="in.flower|flowers|X|Bloom_guide|unidentified|notes", names(thissite))]#gets rid of superfluous columns
  thissite<-thissite[620,c(6:20)]#col 21 is No_canopy trees. change the row column depending on the species I'm looking at. Columns 6-20 are where all the non-tree things were counted.
  results <- append(results, thissite)
}
paste0(results,collapse=",")#condensed output

#i #tells the index of the site where i stopped
#site.summaries$site[i]#this says where i stopped, so where there was first warning. Look up in the 'environment' table and see where there is 'chr' instead of 'num'. this means probably some word instead of number. 


#table(thissite$subplot_2) #this returns values for where the 'chr' was, can then see what needs to change.

#plotting each plot as separate line
#this builds an empty plot that we will later fill
plot(Jdate, site.summaries[1, paste("d", 1:365, sep="")], pch=NA, main= "Plot-level floral resource curves", ylim=c(0,50000), xlab="Day of year", ylab=expression(paste("Floral area, m"^"2"*"ha"^"-1")))
#can add this to fit everything within y limits: ylim=c(0, max(site.summaries[,-1:-2]
#this will fill in the above plot with lines, need to run plot first. Each species is plotted separately
for(i in which(!is.na(site.summaries$d1))){#which returns index number--which rows are not na for col d1 (d1 just bc if its in first row its in all)
  lines(Jdate, site.summaries[i, paste("d", 1:365, sep="")], type="l", col=i)
}



#site.summaries$site[site.summaries$d200>200] #this was to find which sites had floral resources above 200 at day 200
#write.csv(thissite5, file="thissite5_2Jan2024.csv")#checking for any plants that are not coming through as having resources

##########################################################
#Everything from here on we are averaging by habitat
#site.summaries.ordered<-site.summaries[c(),]#OK, if I want to group by ag vs. forest etc. in the legend, I can specify the order here...do this later
#habitat_mean<-summarize_each(group_by(site.summaries, habitat), funs(mean))#this is old code
habitat_mean<-site.summaries %>% group_by(habitat) %>%
  summarize(across(d1:richness, mean))
#habitat_mean<-summarize_each(group_by(site.summaries, habitat), funs(mean,se=sd(.)/sqrt(n())))#period means it will take the stndard dev of whatever you give it just notation. could put in std dev if want to and just replace all the se stuff with 'sd'

#taking out resource values from field crops after crops are in
habitat_mean[habitat_mean$habitat=='WinterFallow', paste("d", 152:365, sep="")]<-0
#number of warnings corresponds to number of habitats--name is not numeric bc can't aggregate site names

#MAKE SURE TO CHANGE TO IP OR WIP DEPENDING ON WHAT I'M CALCULATING
urban_flrs<-read.csv('urban_habitat_resource_byday14_IP.csv')#use if calculating insect-pollinated only. calculated in 'CombiningSites17_usethis19Aug2022.R' file. 
#urban_flrs<-read.csv('urban_habitat_resource_byday14_WIP.csv')#use if calculating wind and insect-pollinated 

urban_flrs_data<-urban_flrs[,-c(1:3)]
urban_flrs_data_ha<-urban_flrs_data*10000#to convert from floral area (m2) within one square meter to within one ha 

urban_flrs_ha<-cbind(urban_flrs[,c(2:3)], urban_flrs_data_ha)
#colnames(urban_flrs_ha)[1]<-'habitat'
all_habs<-bind_rows(habitat_mean, urban_flrs_ha)#this matches by column name
#write.csv(all_habs, file="habitat_mean_WIP_17Jun2024.csv")#THIS IS WHAT WE INPUT (IN CORRECT FORMAT 
#WITH A FEW EXTRA COLUMNS AT BEGINNING INTO KEVIN'S 'habitat_plotsv2_kevinDec2022')

#######Graphing all habitats######
plot(as.Date(Jdate, origin = "2016-01-01"), all_habs[1, paste("d", 1:365, sep="")], pch=NA, main= "Habitat floral resource curves",ylim=c(0,12000), xlab="", xlim=c(0, 365), ylab="", cex.axis=0.9) #or ylab=expression(paste("Floral area, m"^"2"*"ha"^"-1")). #use 33000 for y-lim with wind and 12000 for insect

#if put in log="y" this will put log scale in y axis to visualize smaller values, these are good limit values ylim=c(100,2000)
#mycol<-rainbow(nrow(habitat_mean))#gives nrow unique colors, can change this

#or, do mycol=c("dodgerblue", "red", ...) Look at website http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

#mycol2<-rainbow(nrow(habitat_mean)/2)#when even number
#mycol2<-rainbow(11)
mycol3<-c(brewer.pal(9, "Set1"), "black", "turquoise", "aquamarine4")  #use color brewer to get most distinct colors that the human eye can distinguish http://earlglynn.github.io/RNotes/package/RColorBrewer/index.htmlthis is saying do the first 9 of set1 (only has 9), then do black and turquoise
mycol4<-rep(mycol3, each=2)#repeats color twice before moving to next one
#mycol2<-rep(rainbow(11), each=2) #repeats color twice before moving to next one
mylty<-rep(c(2,1), times=12)  #1,2 correspond to the line type (normal(1) and dashed(2)), this repeats that sequence 11 times

nrow(all_habs)
#this will fill in the above plot with lines, need to run plot first. 
for(i in 1:nrow(all_habs)){#which returns index number--which rows are not na for col d1 (d1 just bc if its in first row its in all)
  lines(Jdate, all_habs[i, paste("d", 1:365, sep="")], type="l", col=mycol4[i], lty=mylty[i])
}

#legend(0,2550, habitat_mean$habitat, lty=mylty, col=mycol4, bty="n", cex=0.7, y.intersp=1.2) #this gives names exactly as they appear

legend(0,12000, legend= c("Hay (alfalfa)", "Perennial crop", "Conifer forest", "Row crop (winter cover)", "Lawn", "Roadside ditch", "Dry oak forest", "Forest edge", "Emergent wetland", "Old field", "Floodplain", "Hay (grass)", "N. hardwood forest (remnant)", "N. hardwood forest (post-ag)", "Pasture", "Shrub wetland", "Shrubland", "Swamp", "Hedgerow", "Mixed vegetable farm", "Row crop (winter fallow)", "Ornamental garden", "Urban tree"), lty=mylty, col=mycol4, bty="n", cex=0.5, y.intersp=1.1)

mtext(expression(paste("Floral area, m"^"2"*"ha"^"-1")), side=2, cex=1, line=2.5)#use this otherwise the y label gets cut off

#######Graphing all habitats just highlighting major ones per habitat category######
plot(as.Date(Jdate, origin = "2016-01-01", format = "%m/%d/%y"), all_habs[1, paste("d", 1:365, sep="")], pch=NA, main= "Habitat floral resource curves",ylim=c(0,12000), xlab="", xlim=c(105, 305), ylab="", cex.axis=0.9) #or ylab=expression(paste("Floral area, m"^"2"*"ha"^"-1")). #use 33000 for y-lim with wind and 12000 for insect

mycol1<-c('gray', 'gray', 'gray','gray', 'gray', 'gray','gray', 'gray', 'gray','orange', 'gray', 'gray','gray', 'gray', 'gray','pink', 'gray', 'gray','black', 'gray', 'gray','gray', 'aquamarine4')

mylty1<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2)

size<-c(.5,.5,.5,.5,.5,.5,.5,.5,.5,2,.5,.5,.5,.5,.5,2,.5,.5,2,.5,.5,.5,2)
nrow(all_habs)
#this will fill in the above plot with lines, need to run plot first. 
for(i in 1:nrow(all_habs)){#which returns index number--which rows are not na for col d1 (d1 just bc if its in first row its in all)
  lines(Jdate, all_habs[i, paste("d", 1:365, sep="")], type="l", col=mycol1[i], lty=mylty1[i], lwd=size[i])
}

#legend(0,2550, habitat_mean$habitat, lty=mylty, col=mycol4, bty="n", cex=0.7, y.intersp=1.2) #this gives names exactly as they appear

legend(200,10000, legend= c("Old field", "Shrub wetland", "Hedgerow", "Urban tree"),  lty=c(1,1,2,1), col=c('orange', 'pink', 'black', 'aquamarine'), bty="n", cex=0.9, y.intersp=1.1, lwd=2)

mtext(expression(paste("Floral area, m"^"2"*"ha"^"-1")), side=2, cex=1, line=2.5)#use this otherwise the y label gets cut off


##################################################################
#SUBSETTING HABITATS####
#CombiningSites8.R has previous version of color patterns for these graphs that was automatic (colors not spelled out)
###########################################


########################FORESTS###################################

#if I want to graph just a subset of the habitats, do the following:
forests<-subset(habitat_mean, habitat%in%c("Conifer", "DryOak", "Edge", "Flood", "MesicUpRem", "MesicUpSuc", "Shrubland", "Swamp","Treerow")) #%in% sign means if the value of habitat is one of these, include it. %%means remainder and %/%means how many times is it divisible in it

#calculating a mean forest resource curve
forests_mean<-forests %>% summarise_if(is.numeric, mean)
forests_mean<-forests_mean[,1:366]
forests_mean$habitat<-"Forest avg"
forests_mean<-forests_mean[,c(367,1:366)]

plot(as.Date(Jdate, origin = "2016-01-01", format = "%m/%d/%y"), forests[1, paste("d", 1:365, sep="")], pch=NA, main= "",ylim=c(0,11000), xlab="", xlim=c(100, 305), ylab="", cex.axis=0.9)

mycol3<-c('blue', 'purple', 'purple', 'chartreuse1', 'brown', 'brown','gray', 'gray', 'black')#originally chartreuse1 was 'yellow'
mylty<-c(2,2,1,2,2,1,2,1,2)

for(i in 1:nrow(forests)){#which returns index number--which rows are not na for col d1 (d1 just bc if its in first row its in all)
  lines(Jdate, forests[i, paste("d", 1:365, sep="")], type="l", col=mycol3[i], lty=mylty[i], lwd=1.5)
}

legend(200,11000, legend= c("Conifer forest", "Dry oak forest", "Forest edge", "Floodplain forest", "N. hardwood forest (remnant)", "N. hardwood forest (post-ag)", "Shrubland", "Swamp", "Hedgerow"), lty=mylty, col=mycol3, bty="n", cex=0.9, y.intersp=1.2, lwd=1.5)

mtext(expression(paste("Floral area, m"^"2"*"ha"^"-1")), side=2, cex=1, line=2.5)#use this otherwise the y label gets cut off

########################Agriculture##################################
#if I want to graph just a subset of the habitats, do the following:
agriculture<-subset(habitat_mean, habitat%in%c("Alf", "Apple", "Cover", "Field", "GrassHay", "Pasture", "Veg", "WinterFallow")) #%in% sign means if the value of habitat is one of these, include it. %%means remainder and %/%means how many times is it divisible in it

plot(as.Date(Jdate, origin = "2016-01-01"), agriculture[1, paste("d", 1:365, sep="")], pch=NA, main= "",ylim=c(0,3400), xlab="", xlim=c(1, 350), ylab="", cex.axis=0.9)

mycol3<-c('red', 'red', 'blue', 'dark orange', 'yellow', 'pink', 'black', 'turquoise')
mylty<-c(2,1,1,1,1,2,1,2)

for(i in 1:nrow(agriculture)){#which returns index number--which rows are not na for col d1 (d1 just bc if its in first row its in all)
  lines(Jdate, agriculture[i, paste("d", 1:365, sep="")], type="l", col=mycol3[i], lty=mylty[i])
}

legend(0,3400, legend= c("Hay (alfalfa)", "Perennial crop", "Row crop (winter cover)", "Old field", "Hay (grass)", "Pasture", "Mixed vegetable farm", "Row crop (winter fallow)"), lty=mylty, col=mycol3, bty="n", cex=0.7, y.intersp=1.2)

mtext(expression(paste("Floral area, m"^"2"*"ha"^"-1")), side=2, cex=1, line=2.5)#use this otherwise the y label gets cut off

##################################################################
#SUBSETTING JUST wetlands
##################################################################
#if I want to graph just a subset of the habitats, do the following:
wetlands<-subset(habitat_mean, habitat%in%c("Emer", "Flood", "Shrub", "Swamp")) #%in% sign means if the value of habitat is one of these, include it. %%means remainder and %/%means how many times is it divisible in it

plot(as.Date(Jdate, origin = "2016-01-01"), wetlands[1, paste("d", 1:365, sep="")], pch=NA, main= "",ylim=c(0,1700), xlab="", xlim=c(1, 350), ylab="", cex.axis=0.9)

mycol3<-c('dark orange', 'gold1', 'hot pink', 'dark gray')
mylty<-c(2,2,1,1)

for(i in 1:nrow(wetlands)){#which returns index number--which rows are not na for col d1 (d1 just bc if its in first row its in all)
  lines(Jdate, wetlands[i, paste("d", 1:365, sep="")], type="l", col=mycol3[i], lty=mylty[i])
}

legend(0,1700, legend= c("Emergent wetland", "Floodplain forest", "Shrub wetland", "Swamp"), lty=mylty, col=mycol3, bty="n", cex=0.7, y.intersp=1.2)

mtext(expression(paste("Floral area, m"^"2"*"ha"^"-1")), side=2, cex=1, line=2.5)#use this otherwise the y label gets cut off


#################
##graphing all developed
#################

#if I want to graph just a subset of the habitats, do the following:
urban<-subset(all_habs, habitat%in%c("Ditch", "Developed", "Garden_flrs", "Urban_tree")) #%in% sign means if the value of habitat is one of these, include it. %%means remainder and %/%means how many times is it divisible in it

plot(as.Date(Jdate, origin = "2016-01-01",format = "%m/%d/%y"), urban[1, paste("d", 1:365, sep="")], pch=NA, main= "",ylim=c(0,2000), xlab="", xlim=c(100, 305), ylab="", cex.axis=0.9)

mycol3<-c('springgreen4', 'springgreen4', 'turquoise', 'turquoise')
mylty<-c(1,2,1,2)

for(i in 1:nrow(urban)){#which returns index number--which rows are not na for col d1 (d1 just bc if its in first row its in all)
  lines(Jdate, urban[i, paste("d", 1:365, sep="")], type="l", col=mycol3[i], lty=mylty[i], lwd=1.5)
}

legend(230,1800, legend= c("Lawn", "Roadside ditch", "Urban garden", "Urban trees"), lty=mylty, col=mycol3, bty="n", cex=0.9, y.intersp=1.2, lwd=1.5)

mtext(expression(paste("Floral area, m"^"2"*"ha"^"-1")), side=2, cex=1, line=2.5)#use this otherwise the y label gets cut off

#######################################
#Adding in corn, soy, apple curves (note these i divide by 1000000 but the others in 
#thissite5 aren't yet because that happens downstream (in site.summaries). but 
#apple, corn, soy, strawberry just go right from thissite5 into graphic)
#these lines from thissite5 for the particular crop are copied into habitat resource curve
#dataset and ultimately into 'habitat_plots_mean_grp_Kevin2024xxxx_IP (or WIP).csv')
#######################################

#corn
thissite5$peakarea[660]<-2730450691/1000000#this value is from 'corn_soy_apple_density.xls' in m2 per ha
thissite5$span<-thissite5$End_final-thissite5$Start_final
thissite5$peakjday<-thissite5$Start_final+thissite5$span/2
thissite5$periodsd<-thissite5$span/6
thissite5$mult <- thissite5$peakarea/dnorm(thissite5$peakjday, mean=thissite5$peakjday, sd=thissite5$periodsd)
thissite5[paste("d", 1:365, sep="")]<-NA #makes empty dataframe, the paste command puts a d (day) in front of 1 to 365 with no space (sep="")
#Sum curves, this takes each plot and sums the amount of resources per day per species
for (j in 1:nrow(thissite5)){ #a for loop within a for loop
  thissite5[j,paste("d", 1:365, sep="")]<-as.list(dnorm(Jdate, mean=thissite5$peakjday[j], sd=thissite5$periodsd[j])*thissite5$mult[j])
}

write.csv(thissite5, file="corn_resource_curve2.csv")#Jun 17,2024--updated with full data on maize tassle size

#soy
thissite5$peakarea[284]<-339082403.3/1000000#this value is from 'corn_soy_apple_density.xls' in mm2 per ha
thissite5$span<-thissite5$End_final-thissite5$Start_final
thissite5$peakjday<-thissite5$Start_final+thissite5$span/2
thissite5$periodsd<-thissite5$span/6
thissite5$mult <- thissite5$peakarea/dnorm(thissite5$peakjday, mean=thissite5$peakjday, sd=thissite5$periodsd)

thissite5[paste("d", 1:365, sep="")]<-NA #makes empty dataframe, the paste command puts a d (day) in front of 1 to 365 with no space (sep="")

#Sum curves, this takes each plot and sums the amount of resources per day per species
for (j in 1:nrow(thissite5)){ #a for loop within a for loop
  thissite5[j,paste("d", 1:365, sep="")]<-as.list(dnorm(Jdate, mean=thissite5$peakjday[j], sd=thissite5$periodsd[j])*thissite5$mult[j])
}
write.csv(thissite5, file="soy_resource_curve.csv")

#apple
thissite5$peakarea[362]<-13631792323/1000000#this value is from 'corn_soy_apple_density.xls' in mm2 per ha
thissite5$span<-thissite5$End_final-thissite5$Start_final
thissite5$peakjday<-thissite5$Start_final+thissite5$span/2
thissite5$periodsd<-thissite5$span/6
thissite5$mult <- thissite5$peakarea/dnorm(thissite5$peakjday, mean=thissite5$peakjday, sd=thissite5$periodsd)

thissite5[paste("d", 1:365, sep="")]<-NA #makes empty dataframe, the paste command puts a d (day) in front of 1 to 365 with no space (sep="")

#Sum curves, this takes each plot and sums the amount of resources per day per species
for (j in 1:nrow(thissite5)){ #a for loop within a for loop
  thissite5[j,paste("d", 1:365, sep="")]<-as.list(dnorm(Jdate, mean=thissite5$peakjday[j], sd=thissite5$periodsd[j])*thissite5$mult[j])
}
write.csv(thissite5, file="apple_resource_curve.csv")#MAY 24,2024--UPDATED WITH CORRECT APPLE SPACING, SHOULD BE ACCURATE NOW


#strawberry
thissite5$peakarea[253]<-457712678.1/1000000#this value is from 'corn_soy_apple_density.xls' in mm2 per ha
thissite5$span<-thissite5$End_final-thissite5$Start_final
thissite5$peakjday<-thissite5$Start_final+thissite5$span/2
thissite5$periodsd<-thissite5$span/6
thissite5$mult <- thissite5$peakarea/dnorm(thissite5$peakjday, mean=thissite5$peakjday, sd=thissite5$periodsd)

thissite5[paste("d", 1:365, sep="")]<-NA #makes empty dataframe, the paste command puts a d (day) in front of 1 to 365 with no space (sep="")

#Sum curves, this takes each plot and sums the amount of resources per day per species
for (j in 1:nrow(thissite5)){ #a for loop within a for loop
  thissite5[j,paste("d", 1:365, sep="")]<-as.list(dnorm(Jdate, mean=thissite5$peakjday[j], sd=thissite5$periodsd[j])*thissite5$mult[j])
}
write.csv(thissite5, file="strawberry_resource_curve.csv")