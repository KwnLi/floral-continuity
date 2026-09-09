#use this to get the urban values (go to where it says 'stop here' ), currently line 130/131 (Sept 2026). then go to 'combining sites 14 plus urban plots3 use this 12Sept23')file
setwd("/Users/aiverson/OneDrive - St. Lawrence University/Simonin/Current spreadsheets to use")

#install.packages('dplyr')
library(dplyr)
library(readxl)
library(ggplot2)
library(magrittr)#part of dplyr package
library(RColorBrewer)


###urban sites----
#READING-IN DATA 
#1) Floral density:
fl.density<-read.csv("Flowers_29May2024.csv", na.strings = "#DIV/0!", skip=1)  #This DIV/O addition changes div/o into NA values
fl.density$Genus<-as.factor(trimws(fl.density$Genus))#get rid of extra spaces after names
fl.density$species<-as.factor(trimws(fl.density$species))
fl.density$binomial<-paste(fl.density$Genus, fl.density$species)
fl.density$binomial<-as.factor(fl.density$binomial)

#levels(fl.density$Genus)#2 less genera in this spreadsheet. Agrimonia eupatoria is extra, not in fl.density

#2) Floral area:
fl.area<-read.csv("FloralArea_29May2024.csv")
fl.area$Genus<-as.factor(trimws(fl.area$Genus))
fl.area$species<-as.factor(trimws(fl.area$species))
fl.area$binomial<-paste(fl.area$Genus, fl.area$species)
#levels(fl.area$Genus)
#Bloom dates:
bloom.date<-read.csv("bloom_period_29May2024.csv", skip=1)
bloom.date$Genus<-as.factor(trimws(bloom.date$Genus))
bloom.date$species<-as.factor(trimws(bloom.date$species))
bloom.date$binomial<-paste(bloom.date$Genus, bloom.date$species)#not sure this is necessary
#levels(bloom.date$Genus)

#3. Urban plant composition
urb_comp<-read.csv("urban_comp8Aug19.csv", skip=1)
urb_comp$Genus<-as.factor(trimws(urb_comp$Genus))
urb_comp$species<-as.factor(trimws(urb_comp$species))

#4) sex ratios
sex_ratios<-read.csv("Kass_flowers_33_ratios_v4.csv", skip=1)#UPDATED Oct25, 2022
sex_ratios$Genus<-as.factor(trimws(sex_ratios$Genus))
sex_ratios$species<-as.factor(trimws(sex_ratios$species))
sex_ratios$binomial<-paste(sex_ratios$Genus, sex_ratios$species)

#CREATING JULIAN DAY RANGE
Jdate <- 1:365

#sum the total area covered by each species
urb_comp_sum<-urb_comp %>%
  select(binomial, cover_area) %>%
  group_by(binomial) %>%
  summarise(cover_area_sum = sum(cover_area)) 
#%>%  merge(.,unique(urb_comp[,c(1,3)]), by='Site')#this would be to add in factors (e.g. region)

#urb_comp_sum2<-aggregate(urb_comp$cover_area, by=list(binomial=urb_comp$binomial), FUN=sum)#this was used when above code wasn't working

#mean bed area 
bed_area_mean<-urb_comp %>%
  select(Region, Site, Bed_num, bed_area) %>%
  group_by(Site, Bed_num) %>%
  summarise(bed_area_mn = mean(bed_area))  
# %>%merge(.,unique(urb_comp[,1]), by=c(Site, Bed_num))#this would be to add in factors (e.g. region) 

#total bed area across all surveys
bed_area_sum<-sum(bed_area_mean$bed_area_mn)
bed_area_sum #925.66 total bed area

#calculate average cover of each species per m2 of total bed area 
urb_comp_sum$avg_cov_urb<-urb_comp_sum$cover_area_sum/925.66#this converts to coverage per meter

#then i added in trees manually. this document contains urban comp sum calcs above, so I can just use this now (and not run above calcs again). 
urb_comp_sum_trees<-read.csv('urb_comp_sum_trees6.csv')# this file summarizes everything from above, so I don't need the above code (thankfully, bc it's no longer working). Basically I summed the total cover of each species ('cover_area_sum'), then divided each by the total bed area (925.66m2), which gives me 'avg_cov_urb' values, or the average cover per m2.
urb_comp_sum_trees<-urb_comp_sum_trees[,2:5]

#left join with floral density and floral area and bloom date
urb1<-left_join(urb_comp_sum_trees,fl.density[c("binomial", "Insect_pollinated", "Avg_Flrs_1m", "Avg_Flrs_Tree")], by=c("binomial"))

urb2<-left_join(urb1,fl.area[c("binomial", "area.per.flower.mm2")], by=c("binomial")) 

#adding in sex ratios (some species flower counts include both male and female flowers when monoecious/dioecious and sometimes only male matters to pollinators)  
urb2.5<-left_join(urb2,sex_ratios[c("binomial", "Mult_factor_final")], by=c("binomial")) 
urb2.5$Avg_Flrs_Tree<-as.numeric(as.character(urb2.5$Avg_Flrs_Tree))#as.character just ranked the numbers
urb2.5$Avg_Flrs_Tree_Sexed<-urb2.5$Avg_Flrs_Tree*urb2.5$Mult_factor_final 

urb3<-left_join(urb2.5,bloom.date[c("binomial", "Start_final", "End_final")], by=c("binomial")) 

#######################
#Calculate Curves 
##IMPORTANT!
#Add this if want just insect-pollinated, skip if you want insect and wind pollinated
urb3<-subset(urb3, urb3$Insect_pollinated!='NA')
urb3<-urb3[urb3$Insect_pollinated==1,]
################################

#calculate peak area
urb3$Avg_Flrs_1m<-as.numeric(as.character(urb3$Avg_Flrs_1m))
urb3$peakarea_urb_herb<-urb3$Avg_Flrs_1m*urb3$area.per.flower.mm2/1000000*urb3$avg_cov_urb#this gives total floral area per species per m2 (the summed area of all beds). divide by a million to get to m2 of floral area.But this is per m2 not per hectare. 

urb3$Avg_Flrs_Tree_Sexed<-as.numeric(as.character(urb3$Avg_Flrs_Tree_Sexed))
urb3$peakarea.tree<-(urb3$num_trees*urb3$Avg_Flrs_Tree_Sexed*(urb3$area.per.flower.mm2/1000000))/27161#divide by 27161 to get num of trees per m2 (odd way of thinking about it, I know). There was a total of 27161 m2 of sampled urban green habitat...so this is the average area of flowers of each tree per m2 of habitat [don't worry, i convert to per ha by multiplying by 10000 in the code where i make floral resource curves]

urb3$peakarea_urb<-rowSums(cbind(urb3$peakarea_urb_herb, urb3$peakarea.tree), na.rm=TRUE)


#the following 4 lines define the bloom curve for each species with a normal distribution across the growing season 
urb3$span<-urb3$End_final-urb3$Start_final
urb3$peakjday<-urb3$Start_final+urb3$span/2
urb3$periodsd<-urb3$span/6
urb3$mult <- urb3$peakarea_urb/dnorm(urb3$peakjday, mean=urb3$peakjday, sd=urb3$periodsd)

urb3[paste("d", 1:365, sep="")]<-NA #makes empty dataframe, the paste command puts a d (day) in front of 1 to 365 with no space (sep="")

#Sum curves, this takes each plot and sums the amount of resources per day per species
for (j in 1:nrow(urb3)){ #a for loop within a for loop
  urb3[j,paste("d", 1:365, sep="")]<-dnorm(Jdate, mean=urb3$peakjday[j], sd=urb3$periodsd[j])*urb3$mult[j]
}
#write.csv(urb3, file="urban_resources_IP_29May2024_v2.csv")

urb4.garden<-colSums(urb3[is.na(urb3$num_trees),paste("d", 1:365, sep="")], na.rm=T)
#urb4.garden<-as.data.frame(urb4.garden)
urb4.trees<-colSums(urb3[!is.na(urb3$num_trees),paste("d", 1:365, sep="")], na.rm=T)
#urb4.trees<-as.data.frame(urb4.trees)
#urban_flrs<-cbind(data.frame(habitat=c('Garden_flrs', 'Urban_tree'), CDL_reclass=c('Garden_flrs', 'Urban_tree')), rbind(urb4.garden, urb4.trees))

urban_flrs<-cbind(data.frame(habitat=c('Garden_flrs', 'Urban_tree'), site=NA), rbind(urb4.garden, urb4.trees))

#USE THIS CSV FILE IN THE MAIN CODE TO CALCULATE ALL HABITAT FLORAL RESOURCE CURVES (CombingSites14_plusUrbanPlots3_usethis_12Sept2023.R)
#write.csv(urban_flrs, file='urban_habitat_resource_byday14_IP.csv')#WIP=wind and insect pollinated, IP=just insect pollinated
write.csv(urban_flrs, file='urban_habitat_resource_byday14_WIP.csv')#WIP=wind and insect pollinated, IP=just insect pollinated


###STOP HERE FOR ANALYSIS INCLUDING URBAN FLOWERS (1 OCT 2019). IMPORTED INTO COMBININGSITE14_PLUSURBANPLOTS2









#READ EXCEL FILE OF SITES----
SitesExcel <- read_excel("Plots_Final_2016_26.xlsx")
#need to delete first and last worksheets
SiteSheetList <- excel_sheets("Plots_Final_2016_26.xlsx")

#CREATING BLANK DATA FRAME FOR SUMMED CURVES AT EACH SITE
#each row is the site (column is called 'site' and in it is tab name)
site.summaries <- data.frame(site=SiteSheetList)
site.summaries$habitat<-substr(site.summaries$site, start=1, stop=regexpr("_| ", site.summaries$site)-1) #this takes the tab and names the sites based on the tab by stopping at the space or the _ in the tab text. regexpr indexes where the space is, we don't want to include the space, so we subtract 1. 
#site.summaries$habitat<-substr(site.summaries$site, start=1, stop=2)#this would give name of first 2 characters.
#table(site.summaries$habitat)#to see the N of each habitat category

site.summaries[paste("d", 1:365, sep="")]<-NA #default is to add space, so sep is to get rid of space. 
site.summaries$richness<-NA#to make column which we will fill with richness data
#i<-SiteSheetList[115] #to get 3rd site in sitesheetlist
#
for (i in SiteSheetList){
  #read sheet
  thissite<-read_excel("Plots_Final_2016_26.xlsx", sheet=i, na=c("", 'tree', 'sap', 'Tree', 'Sap', '(sap)', '(tree)', 's', 't'), n_max=661)#some subplots have 'tree' or 'sap', etc instead of number (actually 0 value), n_max means only read first 661 rows
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
  
  thissite3<-left_join(thissite2,fl.area[c("Genus", "species", "area.per.flower.mm2")], by=c("Genus"="Genus", "species"="species")) 
  
  thissite4<-left_join(thissite3,bloom.date[c("Genus", "species", "Start_final", "End_final")], by=c("Genus"="Genus", "species"="species")) 
  
  sum(thissite4$avg_cov>0)#count the number of trues
  
  #Calculate Curves 
  #Add this if want just insect-pollinated
  #thissite5<-thissite4[thissite4$Insect_pollinated==1,]  
  #otherwise use this:
  thissite5<-thissite4  
  
  #thissite5$peakarea<-thissite5$Avg_Flrs_1m*thissite5$area.per.flower.mm2*thissite5$avg_cov #this is to use if no trees are included
  
  #use the following if including trees
  thissite5$peakarea.herb<-thissite5$Avg_Flrs_1m*thissite5$area.per.flower.mm2*thissite5$avg_cov#This results in mm2 floral area per ha 
  thissite5$peakarea.tree<-thissite5$No_canopy_trees*40*thissite5$area.per.flower.mm2*thissite5$Avg_Flrs_Tree#multiply by 40 to get mm2 floral area per ha
  thissite5$peakarea<-rowSums(cbind(thissite5$peakarea.herb, thissite5$peakarea.tree), na.rm=TRUE)
  
  #the following 4 lines define the bloom curve for each species with a normal distribution across the growing season 
  thissite5$span<-thissite5$End_final-thissite5$Start_final
  thissite5$peakjday<-thissite5$Start_final+thissite5$span/2
  thissite5$periodsd<-thissite5$span/6
  thissite5$mult <- thissite5$peakarea/dnorm(thissite5$peakjday, mean=thissite5$peakjday, sd=thissite5$periodsd)
  
  thissite5[paste("d", 1:365, sep="")]<-NA #makes empty dataframe, the paste command puts a d (day) in front of 1 to 365 with no space (sep="")
  
#Sum curves, this takes each plot and sums the amount of resources per day per species
  for (j in 1:nrow(thissite5)){ #a for loop within a for loop
    thissite5[j,paste("d", 1:365, sep="")]<-dnorm(Jdate, mean=thissite5$peakjday[j], sd=thissite5$periodsd[j])*thissite5$mult[j]
  }
  site.summaries[which(site.summaries$site==i),paste("d", 1:365, sep="")]<-colSums(thissite5[,paste("d", 1:365, sep="")], na.rm=TRUE)/1000000#divide by 1 million to go from mm2 to m2 per ha
  #site.summaries now is each row is a different site and each column is a day
  site.summaries$richness[which(site.summaries$site==i)]<-sum(thissite4$avg_cov>0)
}

#trouble shooting dataset

#i #tells the index of the site where i stopped
#site.summaries$site[i]#this says where i stopped, so where there was first warning. Look up in the 'environment' table and see where there is 'chr' instead of 'num'. this means probably some word instead of number. 

#table(thissite$subplot_2) #this returns values for where the 'chr' was, can then see what needs to change.

#this builds an empty plot that we will later fill
plot(Jdate, site.summaries[1, paste("d", 1:365, sep="")], pch=NA, main= "Plot-level floral resource curves", ylim=c(0,1000), xlab="Day of year", ylab=expression(paste("Floral area, m"^"2"*"ha"^"-1")))
#can add this to fit everything within y limits: ylim=c(0, max(site.summaries[,-1:-2]
#this will fill in the above plot with lines, need to run plot first. Each species is plotted separately
for(i in which(!is.na(site.summaries$d1))){#which returns index number--which rows are not na for col d1 (d1 just bc if its in first row its in all)
  lines(Jdate, site.summaries[i, paste("d", 1:365, sep="")], type="l", col=i)
}

#this is exactly the same but without which! that omits those sites with NAs

#site.summaries$site[site.summaries$d200>200] #this was to find which sites had floral resources above 200 at day 200


##########################################################
#Everything from here on we are averaging by habitat
#site.summaries.ordered<-site.summaries[c(),]#OK, if I want to group by ag vs. forest etc. in the legend, I can specify the order here...do this later
habitat_mean<-summarize_each(group_by(site.summaries, habitat), funs(mean))

#habitat_mean<-summarize_each(group_by(site.summaries, habitat), funs(mean,se=sd(.)/sqrt(n())))#period means it will take the stndard dev of whatever you give it just notation. could put in std dev if want to and just replace all the se stuff with 'sd'

#taking out resource values from field crops after crops are in
habitat_mean[habitat_mean$habitat=='WinterFallow', paste("d", 152:365, sep="")]<-0
#number of warnings corresponds to number of habitats--name is not numeric bc can't aggregate site names

all_habs<-rbind(habitat_mean[,-368], urban_flrs)

#first plot an empty frame
#plot(Jdate, habitat_mean[1, paste("d", 1:365, sep="")], pch=NA, main= "Habitat floral resource curves",ylim=c(0,2700), xlab="Day of year", ylab="") #or ylab=expression(paste("Floral area, m"^"2"*"ha"^"-1"))#this is just julian date

#setting up the plotting for multiple in one graph

labels <- c("A. All habitats", "B. Forest", "C. Agriculture", "Wind- and insect-pollinated")

par(mfrow = c(2, 2),# 2x2 layout
oma = c(2, 2, 0, 0), #2 rows of text at the outer left and bottom margin
mar = c(1, 1, 1, 1),#space for 1 row of text at ticks & to separate plots
mgp = c(2, 1, 0),# axis label at 2 rows distance, tick labels at 1 row
xpd = NA)    # allow content to protrude into outer margin (and beyond)

for(i in 1:4) {
  plot(i)
  mtext(labels[i], side = 3, adj = 0.05, line = -1.3)
}

plot(as.Date(Jdate, origin = "2016-01-01"), habitat_mean[1, paste("d", 1:365, sep="")], pch=NA, main= "",ylim=c(0,1200), xlab="", xlim=c(1, 350), ylab="", cex.axis=0.9,
mtext(labels[i], side = 3, adj = 0.05, line = -1.3))
#or ylab=expression(paste("Floral area, m"^"2"*"ha"^"-1"))

#if put in log="y" this will put log scale in y axis to visualize smaller values, these are good limit values ylim=c(100,2000)
#mycol<-rainbow(nrow(habitat_mean))#gives nrow unique colors, can change this

#or, do mycol=c("dodgerblue", "red", ...) Look at website http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

#mycol2<-rainbow(nrow(habitat_mean)/2)#when even number
#mycol2<-rainbow(11)
mycol3<-c(brewer.pal(9, "Set1"), "black", "turquoise")  #use color brewer to get most distinct colors that the human eye can distinguish http://earlglynn.github.io/RNotes/package/RColorBrewer/index.htmlthis is saying do the first 9 of set1 (only has 9), then do black and turquoise
mycol4<-rep(mycol3, each=2)#repeats color twice before moving to next one
#mycol2<-rep(rainbow(11), each=2) #repeats color twice before moving to next one
mylty<-rep(c(2,1), times=12)  #1,2 correspond to the line type (normal(1) and dashed(2)), this repeats that sequence 11 times

nrow(all_habs)
#this will fill in the above plot with lines, need to run plot first. 
for(i in 1:nrow(all_habs)){#which returns index number--which rows are not na for col d1 (d1 just bc if its in first row its in all)
  lines(Jdate, all_habs[i, paste("d", 1:365, sep="")], type="l", col=mycol4[i], lty=mylty[i])
}

#legend(0,2550, habitat_mean$habitat, lty=mylty, col=mycol4, bty="n", cex=0.7, y.intersp=1.2) #this gives names exactly as they appear

legend(0,1000, legend= c("Hay (alfalfa)", "Perennial crop", "Conifer forest", "Row crop (winter cover)", "Lawn", "Roadside ditch", "Dry oak forest", "Forest edge", "Emergent wetland", "Old field", "Floodplain", "Hay (grass)", "N. hardwood forest (remnant)", "N. hardwood forest (post-ag)", "Pasture", "Shrub wetland", "Shrubland", "Swamp", "Hedgerow", "Mixed vegetable farm", "Row crop (winter fallow)", "Urban_orn", "Urban_tree"), lty=mylty, col=mycol4, bty="n", cex=0.4, y.intersp=1)

mtext(expression(paste("Floral area, m"^"2"*"ha"^"-1")), side=2, cex=1, line=2.5)#use this otherwise the y label gets cut off

##################################################################
#SUBSETTING HABITATS####
#CombiningSites8.R has previous version of color patterns for these graphs that was automatic (colors not spelled out)
########################FORESTS###################################
#if I want to graph just a subset of the habitats, do the following:
forests<-subset(habitat_mean, habitat%in%c("Conifer", "DryOak", "Edge", "Flood", "MesicUpRem", "MesicUpSuc", "Swamp","Treerow")) #%in% sign means if the value of habitat is one of these, include it. %%means remainder and %/%means how many times is it divisible in it

plot(as.Date(Jdate, origin = "2016-01-01"), forests[1, paste("d", 1:365, sep="")], pch=NA, main= "",ylim=c(0,12000), xlab="", xlim=c(1, 350), ylab="", cex.axis=0.9)

mycol3<-c('blue', 'purple', 'purple', 'yellow', 'brown', 'brown','gray', 'black')
mylty<-c(2,2,1,2,2,1,1,2)

for(i in 1:nrow(forests)){#which returns index number--which rows are not na for col d1 (d1 just bc if its in first row its in all)
  lines(Jdate, forests[i, paste("d", 1:365, sep="")], type="l", col=mycol3[i], lty=mylty[i])
}

legend(0,16000, legend= c("Conifer forest", "Dry oak forest", "Forest edge", "Floodplain forest", "N. hardwood forest (remnant)", "N. hardwood forest (post-ag)", "Swamp", "Hedgerow"), lty=mylty, col=mycol3, bty="n", cex=0.7, y.intersp=1.2)

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

plot(as.Date(Jdate, origin = "2016-01-01"), wetlands[1, paste("d", 1:365, sep="")], pch=NA, main= "",ylim=c(0,1000), xlab="", xlim=c(1, 350), ylab="", cex.axis=0.9)

mycol3<-c('dark orange', 'yellow', 'hot pink', 'dark gray')
mylty<-c(2,2,1,1)

for(i in 1:nrow(wetlands)){#which returns index number--which rows are not na for col d1 (d1 just bc if its in first row its in all)
  lines(Jdate, wetlands[i, paste("d", 1:365, sep="")], type="l", col=mycol3[i], lty=mylty[i])
}

legend(0,1000, legend= c("Emergent wetland", "Floodplain forest", "Shrub wetland", "Swamp"), lty=mylty, col=mycol3, bty="n", cex=0.7, y.intersp=1.2)

mtext(expression(paste("Floral area, m"^"2"*"ha"^"-1")), side=2, cex=1, line=2.5)#use this otherwise the y label gets cut off

########################JUST DRY OAK FORESTS###################################
#if I want to graph just a subset of the habitats, do the following:
DryOakFor<-subset(habitat_mean, habitat%in%c("DryOak")) #%in% sign means if the value of habitat is one of these, include it. %%means remainder and %/%means how many times is it divisible in it

plot(as.Date(Jdate, origin = "2016-01-01"), DryOakFor[1, paste("d", 1:365, sep="")], pch=NA, main= "Dry Oak floral resource curves",ylim=c(0,5000), xlab="", xlim=c(1, 350), ylab="", cex.axis=0.9)

mycol3<-c('blue', 'purple', 'purple', 'yellow', 'brown', 'brown','gray', 'black')
mylty<-c(2,2,1,2,2,1,1,2)

for(i in 1:nrow(DryOakFor)){#which returns index number--which rows are not na for col d1 (d1 just bc if its in first row its in all)
  lines(Jdate, DryOakFor[i, paste("d", 1:365, sep="")], type="l", col=mycol3[i], lty=1)
}

legend(0,16000, legend= , lty=mylty, col=mycol3, bty="n", cex=0.7, y.intersp=1.2)

mtext(expression(paste("Floral area, m"^"2"*"ha"^"-1")), side=2, cex=1, line=2.5)#use this otherwise the y label gets cut off

########################JUST DRY OAK FORESTS###################################
#if I want to graph just a subset of the habitats, do the following:
OldFields<-subset(habitat_mean, habitat%in%c("Field")) #%in% sign means if the value of habitat is one of these, include it. %%means remainder and %/%means how many times is it divisible in it

plot(as.Date(Jdate, origin = "2016-01-01"), OldFields[1, paste("d", 1:365, sep="")], pch=NA, main= "Old Field floral resource curves",ylim=c(0,5000), xlab="", xlim=c(1, 350), ylab="", cex.axis=0.9)

mycol3<-c('blue', 'purple', 'purple', 'yellow', 'brown', 'brown','gray', 'black')
mylty<-c(2,2,1,2,2,1,1,2)

for(i in 1:nrow(OldFields)){#which returns index number--which rows are not na for col d1 (d1 just bc if its in first row its in all)
  lines(Jdate, OldFields[i, paste("d", 1:365, sep="")], type="l", col=mycol3[i], lty=1)
}

legend(0,16000, legend= , lty=mylty, col=mycol3, bty="n", cex=0.7, y.intersp=1.2)

mtext(expression(paste("Floral area, m"^"2"*"ha"^"-1")), side=2, cex=1, line=2.5)#use this otherwise the y label gets cut off



#Dec 15, 2017 SUBSETTING JUST pertinent habitats with Lythrum and Centaurea (for Scott)####
##################################################################
#Lythrum
Lythrum<-subset(habitat_mean, habitat%in%c("Ditch", "Emer", "Swamp")) #%in% sign means if the value of habitat is one of these, include it. %%means remainder and %/%means how many times is it divisible in it

plot(as.Date(Jdate, origin = "2016-01-01"), Lythrum[1, paste("d", 1:365, sep="")], pch=NA, main= "Habitats with loosestrife",ylim=c(0,2100), xlab="", xlim=c(1, 350), ylab="", cex.axis=0.9)

mycol3<-c('dark green', 'dark orange', 'dark gray')
mylty<-c(1,2,1)

for(i in 1:nrow(Lythrum)){#which returns index number--which rows are not na for col d1 (d1 just bc if its in first row its in all)
  lines(Jdate, Lythrum[i, paste("d", 1:365, sep="")], type="l", col=mycol3[i], lty=mylty[i])
}

legend(0,1900, legend= c("Roadside ditch", "Emergent wetland","Swamp"), lty=mylty, col=mycol3, bty="n", cex=0.8, y.intersp=1.2)

mtext(expression(paste("Floral area, m"^"2"*"ha"^"-1")), side=2, cex=1, line=2.5)#use this otherwise the y label gets cut off
###############################################
#Centaurea
Centaurea<-subset(habitat_mean, habitat%in%c("Ditch", "Flood", "Pasture", "Shrubland", "Veg")) #%in% sign means if the value of habitat is one of these, include it. %%means remainder and %/%means how many times is it divisible in it

plot(as.Date(Jdate, origin = "2016-01-01"), Centaurea[1, paste("d", 1:365, sep="")], pch=NA, main= "Habitats with knapweed",ylim=c(0,2100), xlab="", xlim=c(1, 350), ylab="", cex.axis=0.9)

mycol3<-c('dark green', 'pink', 'dark gray', 'yellow2', 'black')
mylty<-c(1,2,2,2,1)

for(i in 1:nrow(Centaurea)){#which returns index number--which rows are not na for col d1 (d1 just bc if its in first row its in all)
  lines(Jdate, Centaurea[i, paste("d", 1:365, sep="")], type="l", col=mycol3[i], lty=mylty[i])
}

legend(0,1900, legend= c("Roadside ditch", "Pasture", "Shrubland", "Floodplain", "Mixed vegetable farm"), lty=mylty, col=mycol3, bty="n", cex=0.8, y.intersp=1.2)

mtext(expression(paste("Floral area, m"^"2"*"ha"^"-1")), side=2, cex=1, line=2.5)#use this otherwise the y label gets cut off

#####################################
####Phenology Shift####
#I want to calculate average shift, excluding 'used PA' data
#Replace missing values in Cayuga with Mundy where available, where not available

setwd("~/Dropbox/Simonin/Current spreadsheets to use")
detach(shift_data)


shift_data<-read.csv("bloom_period9.csv", skip=1)
#shift_data<-subset(shift_data, Notes_Cayuga!="used PA"& Notes_Cayuga2!="used PA") #this is to get rid of the values where I used PA data, since this will artificially make the phenology shift wonky
#filling back in NAs where 'used PA' is present
shift_data$Start_PA[shift_data$Start_PA=="not reported"] <- NA#some values here were listed at 'not reported' in the Start_PA column which caused heck later on
shift_data$Start_PA <- as.numeric(as.character(shift_data$Start_PA))#notes from Erika: We have to change the factor to a number with as.numeric(as.character(FACTOR)) because we have to convert it to the character representation of the number first, before making it numeric.  Otherwise it will copy over the factor level rank rather than the actual number. This is an example of a failing in R, in my opinion. 
shift_data$Start_Cayuga[shift_data$Notes_Cayuga=='used PA'|shift_data$Notes_Cayuga2=='used PA'|shift_data$Notes_Cayuga2=='delete cayuga']<-NA
shift_data$End_Cayuga[shift_data$Notes_Cayuga=='used PA'|shift_data$Notes_Cayuga2=='used PA']<-NA#do this instead of how I did it above bc doing a subset made it hard to get the omitted species back into the list at the end.

attach(shift_data)
plot(Start_Cornell~Start_Cayuga, xlim=c(50,300), ylim=c(50,300))
#plotting just the first flower dates
shift_mod<-lm(Start_Cornell~Start_Cayuga, na.action=na.exclude)#We don't want to use NA species in the model but we want to retain NAs in dataset for later use when making final dataframe.
summary(shift_mod) #R^2=0.5277
#plot(shift_mod)#this gives the fit/residual data
abline(shift_mod, col="red")#fits a line to the data
abline(0,1)#this plots the 1:1 line (0=intercept, 1=slope)

#graphing log of start_cornell in case it's a better fit than linear fit
#log_mod<-lm(log(Start_Cornell)~Start_Cayuga, data=shift_data)
#summary(log_mod)#nope, slightly worse fit R^2=0.5095


#Make a dataframe with the variables we want
#pred_data<-data.frame(Genus, species, Start_Cayuga, Start_Cornell)
#pred_data$pred1<-round(predict(shift_mod, newdata=pred_data))#this gives a prediction based on the shift_mod regression---so it takes into account time of year (e.g. shift at beginning of year compared to end) because it is based on the actual regression line (which is flatter than the 1:1 line---so at the beginning of the year it shifts later and at end of year it shifts earlier)

#verifying a different way to graph the data
#plot(I(Start_Cornell-Start_Cayuga)~Start_Cayuga)
#mod2<-lm(I(Start_Cornell-Start_Cayuga)~Start_Cayuga)#this is the same as shift_mod
#summary(mod2)
#plot(mod2)
#plot(I(Start_Cornell-Start_Cayuga)~Genus)#this is to look at the differences in the start dates based on Genus. There is a lot of spread within a given genus-doesnt make sense to generalize by genus.
#plot(I(Start_Cornell-Start_Cayuga)~family)#this is to look at the differences in the start dates based on family. This may be helpful to use, but it might make incorrect assumptions for families where there is only one data point. 

#pred_data$pred2<-pred_data$Start_Cayuga + predict(mod2, newdata=pred_data)#this gives exact same result, so can just use data from above (no need to use this)

#now I need to write a code to use the cornell data except for when no data are present, then use cayuga plus shift.This code is now outdated---didnt' take into account the 'usedPA' data. use next set of code instead.

#shift_data$start_use<-pred_data$pred1
# shift_data$start_use[!is.na(pred_data$Start_Cornell)]<-pred_data$Start_Cornell[!is.na(pred_data$Start_Cornell)]

# shift_data$end_use<-shift_data$End_Cornell
# shift_data$end_use[is.na(shift_data$End_Cornell)]<-shift_data$start_use[is.na(shift_data$End_Cornell)]+(shift_data$End_Cayuga[is.na(shift_data$End_Cornell)]-shift_data$Start_Cayuga[is.na(shift_data$End_Cornell)])

#write.csv(shift_data, file="bloom_period_shifted_minusPA.csv")#this refers to 30 or so plants that are not included in this because they were extracted from the list (where 'used PA' was listed)

#filling in final dataframe
shift_data$pred1<-round(predict(shift_mod, newdata=shift_data))#this fills in all values where there is cayuga data based on the shift_mod regression
shift_data$start_use<-shift_data$Start_Cornell#for the actual dates to use, if Cornell data are there, use that
shift_data$start_use[is.na(shift_data$start_use)]<-shift_data$pred1[is.na(shift_data$start_use)]#where there are NAs in 'start_use' bc no cornell data is present, use pred1 prediction based on regression.
shift_data$start_use[is.na(shift_data$start_use)]<-shift_data$Start_PA[is.na(shift_data$start_use)]#where start_use is still NA (because there is no start_cayuga data =  where PA data was used), use the start date from PA
View(shift_data[c("Genus", "species", "Start_PA", "Start_Cayuga", "Notes_Cayuga", "Notes_Cayuga2", "pred1", "Start_Cornell", "start_use")])

#use this if we want to have the end_cornell data be the one we use for the end_use
# shift_data$end_use<-shift_data$End_Cornell 
# shift_data$end_use[is.na(shift_data$End_Cornell)]<-shift_data$start_use[is.na(shift_data$End_Cornell)]+(shift_data$End_Cayuga[is.na(shift_data$End_Cornell)]-shift_data$Start_Cayuga[is.na(shift_data$End_Cornell)])
# shift_data$end_use[is.na(shift_data$end_use)]<-shift_data$End_PA[is.na(shift_data$end_use)]

#use this if want the end_use date to be the same range as cayuga flora for all plants (including cornell plants)
shift_data$end_use<-NA
shift_data$end_use<-shift_data$start_use+(shift_data$End_Cayuga-shift_data$Start_Cayuga)#this takes the start date (whether mundy observation, predicted from cayuga) and adds the range that is published in Cayuga flora)
shift_data$end_use[is.na(shift_data$end_use)]<-shift_data$End_Cornell[is.na(shift_data$end_use)]#if no cayuga data but there is cornell data, use that for the end date
shift_data$end_use[is.na(shift_data$end_use)]<-shift_data$End_PA[is.na(shift_data$end_use)]#if it is using PA bloom start, we keep it as the PA end date, too.
shift_data$end_use[is.na(shift_data$end_use)]<-shift_data$End_Cornell[is.na(shift_data$end_use)]
shift_data$start_use_date<-as.Date(shift_data$start_use, origin = "2018-01-01")
shift_data$end_use_date<-as.Date(shift_data$end_use, origin = "2018-01-01")

View(shift_data[c("Start_PA", "End_PA", "Start_Cayuga", "End_Cayuga", "Notes_Cayuga", "Notes_Cayuga2", "pred1", "Start_Cornell","End_Cornell", "start_use", "end_use")])
write.csv(shift_data, file="bloom_period_shifted23May2018.csv")

#checking what values are in a given column
levels(shift_data$Notes_Cayuga)
table(shift_data$Notes_Cayuga)

#some extra code

#i had this before, probably don't need:
#urb3$Avg_Flrs_Tree<-as.numeric(urb3$Avg_Flrs_Tree)
#urb3$peakarea.tree<-urb3$num_trees/27161*urb3$area.per.flower.mm2/1000000*urb3$Avg_Flrs_Tree#divide by 27161 to get num of trees per m2 (odd way of thinking about it, I know)


#to do Dec 2017
#set order of dataset so legend is in a specific order

#April25, 2018
#calculate what % of floral resources are represented by the mundy data so can put this number in the pub---maybe only 50% of species but could be 90% of resources

#ISSUES/tasks Sept 5, 2018
#pull out top 10 resource plants in each habitat
#figure out how to pull out which site had a particular plant---e.g. searching for where Geum macrophyllum was found
#pull out species richness per habitat type (for paper)
#Is 'mean_area_per_species.csv' the mean peak area? What about mean area under curve?
#some flower bloom ranges are still missing--quercus, salix, etc. 

#NEED TO DO SANITY CHECK TO MAKE SURE RESOURCES ON THAT DAY OF YEAR MAKES SENSE (EG BLOOMING IN JAN)
#group legend by ag, developed, forest, wetland, etc. 
#NEED TO ADD IN APPLE TREES TO APPLE PLOTS. ACTUALLY, MAYBE NOT SINCE THESE ARE MEANT TO BE PERENNIAL AG. 
#DELETE THE MISIDENTIFIED PLANTS IN ALL DATASETS AND REASSIGN TO APPROPRIATE PLANT IN PLOT-LEVEL DATA


#write.csv(habitat_mean, file="habitat_mean_WI_29Jan2019.csv")
#write.csv(habitat_mean, file="habitat_mean_I_29Jan2019.csv")
