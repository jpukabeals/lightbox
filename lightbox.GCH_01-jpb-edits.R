if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               readxl,
               dplyr,
               jpeg,
               randomForest,
               stringr,
               ggplot2,
               cowplot,
               emmeans,
               EBImage)

require(EBImage) #this needs to be downloaded from (https://www.bioconductor.org/packages/release/bioc/html/EBImage.html)***
# OR use these lines
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("EBImage")

options(scipen=999) #reduce scientific notation 
options(digits=4)   # only print 4 sig figs
theme_set(theme_bw)



ImgOverlay<- function(original.image, mask) #function for overlaying an RGB image onto a binary mask***
{
  replace<-which(mask==0)
  red<-original.image[,,1] 
  blue<-original.image[,,2]
  green<-original.image[,,3]
  red[replace]<- 0
  blue[replace]<- 0
  green[replace]<- 0
  return(array(c(red, blue, green), dim=dim(original.image)))
}

ImgToDataframe<- function(image.path) #function for vecotizing an RGB image and maintaining its matrix coordinates***
{
  image.dat<- readImage(image.path)
  coor<- as.data.frame.table(image.dat[,,1])[1:2]
  red<- 255*as.data.frame.table(image.dat[,,1])[3] #to obtain an RGB color space multiply by 255***
  green<- 255*as.data.frame.table(image.dat[,,2])[3]
  blue<- 255*as.data.frame.table(image.dat[,,3])[3]
  image.dat<- bind_cols(coor, red, green, blue)
  colnames(image.dat)<- c("y","x","red","green","blue")
  return(image.dat)
}

























getwd()
setwd("C:/Users/pukab001/Downloads")

img_IWG_seed.demo.1.30.21<- "C:\\Users\\pukab001\\Pictures\\OLYMPUS Capture\\IWG_img_output"
# "/Volumes/GoogleDrive/My Drive/Image Analysis/IWG_YieldComponents/ComputerVision_R/UMNPuka_SCS.2"

## Creating folders to store all image output
#NOTE: the folder "original_img" is where the images you want to process need to be***
folders <- c("training data", 
             "results", 
             "original_img", 
             "crop_img",
             "chaff_01",
             "chaff_02",
             "chaff_03",
             "dehulled seed_01",
             "dehulled seed_02",
             "dehulled seed_03",
             "hulled seed_01",
             "hulled seed_02",
             "hulled seed_03",
             "ergot_01",
             "ergot_02",
             "ergot_03"
) #adding in the correct folder list*** 
for (i in 1:length(folders))  { 
  dir.create(paste(img_IWG_seed.demo.1.30.21,folders[i], sep="/")) 
}






















IWG_seed.dat<- read.csv(paste(img_IWG_seed.demo.1.30.21, "results", "practice.files.csv",
                              sep = "/"))[1:3,] %>%
  mutate(tally=as.factor(tally))






















original_img.path<- list.files(path=paste(img_IWG_seed.demo.1.30.21, "original_img",sep = "/"), full.names = T)
original_img.name<- list.files(path=paste(img_IWG_seed.demo.1.30.21, "original_img",sep = "/"), full.names = F)
folder_crop_img<-   paste(img_IWG_seed.demo.1.30.21,"crop_img",sep = "/")
## This portions will reduce the number of pixels of the image through a resizing function
for(i in 1:length(original_img.path)){
  temp1<- readImage(original_img.path[i])
  temp2<- EBImage::resize(temp1, 
                          2000) #select your pixel width here***
  writeImage(temp2, paste(folder_crop_img, "/", original_img.name[i], sep = ""), quality = 100)
}






















palette_directory_IWG.seed<- paste(img_IWG_seed.demo.1.30.21, "training data",sep = "/") #file path where mixes are saved***

mixes_names<- list.files(path=palette_directory_IWG.seed,pattern="*.csv",full.names = FALSE) #name directory for what is in the palette folder***
mixes_path<- list.files(path=palette_directory_IWG.seed, pattern="*.csv", full.names = TRUE) #path directory for what is in the palette folder***
training.palette_IWG.seed<- data.frame()
#this for() loop will systematically rearrange and condense each mix file in the training palette folder***
#the reason I am doing this is to allow the script to update itself upon adding additional mixes***
for (i in 1:length(mixes_path)){
  temp_mix<- read.csv(mixes_path[i])
  temp_mix$band<- NA
  temp_mix$band[1:which(temp_mix$Label == "Red")] <- "Red"
  temp_mix$band[(which(temp_mix$Label == "Red")+1):which(temp_mix$Label == "Green")] <- "Green"
  temp_mix$band[(which(temp_mix$Label == "Green")+1):which(temp_mix$Label == "Blue")] <- "Blue"
  temp<- split(temp_mix, temp_mix$band) # creates a list
  temp2<- do.call("cbind", split(temp_mix, temp_mix$band))
  image<- temp2$Blue.Label[i]
  mix<- mixes_names[i]
  temp3<- data.frame(mix, image, x=temp2[5]$Blue.X, y=temp2[6]$Blue.Y, red=temp2[18]$Red.Mean, green=temp2[11]$Green.Mean, blue=temp2[4]$Blue.Mean) #this is where the hard coding is causing me problems
  training.palette_IWG.seed<- rbind(training.palette_IWG.seed, temp3) 
}
summary(training.palette_IWG.seed) #summarizing the training palette***
count(training.palette_IWG.seed, mix) #counting observations in each mix of the training palette*** 























palette_selection_categorical<- filter(training.palette_IWG.seed, grepl("Cat", mix)) #creating an object with training data, here filtering can be done for different training mixes***
palette_selection_binary<- filter(training.palette_IWG.seed, grepl("Bin", mix))

palette_selection_categorical<- palette_selection_categorical %>%
  mutate(classification = case_when(grepl("null", mix) ~ 0, #re coding each mix type (null ergot...) with a numeric integer to help make the model***
                                    grepl("chaff", mix) ~ 1,
                                    grepl("de", mix) ~ 2,
                                    grepl("Hseed", mix) ~ 3,
                                    grepl("ergot", mix) ~ 4))

# 'Error in na.fail.default(list(classification = c(0, 0, 0, 0, 0, 0, 0,  : missing values in object'
#  ^^^COULD BE ISSUE DUE TO SCREW FILE, MAY NEED TO ADD

# palette_selection_categorical %>% group_by(mix) %>% summarise(avg=mean(classification)) %>% View() #viewing the classisifications for this training palette***
rfm_IWG_categorical<- randomForest(factor(classification)~(red+green+blue), #NOTE the factor() before the response***
                                   data=palette_selection_categorical, 
                                   ntree=50, #it is best to keep trees under 100***
                                   mtry = 1, #with so few predictors 1 sample per tree is best***
                                   importance=TRUE)
print(rfm_IWG_categorical)
plot(rfm_IWG_categorical)
importance(rfm_IWG_categorical) 























#************************#
palette_selection_chaff<- palette_selection_categorical %>%
  mutate(classification = case_when(grepl("null", mix) ~ 0,
                                    grepl("chaff", mix) ~ 1,
                                    grepl("de", mix) ~ 0,
                                    grepl("Hseed", mix) ~ 0,
                                    grepl("ergot", mix) ~ 0)) 

chaff.rfm<- randomForest(classification~(red+green+blue),
                         data=palette_selection_chaff, 
                         ntree=30,
                         mtry = 1,
                         importance=TRUE)
print(chaff.rfm)
plot(chaff.rfm)
importance(chaff.rfm)
#************************#
palette_selection_ergot<- palette_selection_categorical %>%
  mutate(classification = case_when(grepl("null", mix) ~ 0,
                                    grepl("chaff", mix) ~ 0,
                                    grepl("de", mix) ~ 0,
                                    grepl("Hseed", mix) ~ 0,
                                    grepl("ergot", mix) ~ 1)) 

ergot.rfm<- randomForest(classification~(red+green+blue),
                         data=palette_selection_ergot, 
                         ntree=30,
                         mtry = 1,
                         importance=TRUE)
print(ergot.rfm)
plot(ergot.rfm)
importance(ergot.rfm)
#************************#
palette_selection_dehulled<- palette_selection_categorical %>%
  mutate(classification = case_when(grepl("null", mix) ~ 0,
                                    grepl("chaff", mix) ~ 0,
                                    grepl("de", mix) ~ 1,
                                    grepl("Hseed", mix) ~ 0,
                                    grepl("ergot", mix) ~ 0)) 

dehulled.seed.rfm<- randomForest(classification~(red+green+blue),
                         data=palette_selection_dehulled, 
                         ntree=30,
                         mtry = 1,
                         importance=TRUE)
print(dehulled.seed.rfm)
plot(dehulled.seed.rfm)
importance(dehulled.seed.rfm)
#************************#
palette_selection_hulled<- palette_selection_categorical %>%
  mutate(classification = case_when(grepl("null", mix) ~ 0,
                                    grepl("chaff", mix) ~ 0,
                                    grepl("de", mix) ~ 0,
                                    grepl("Hseed", mix) ~ 1,
                                    grepl("ergot", mix) ~ 0)) 

hulled.seed.rfm<- randomForest(classification~(red+green+blue),
                            data=palette_selection_hulled, 
                            ntree=30,
                            mtry = 1,
                            importance=TRUE)
print(hulled.seed.rfm)
plot(hulled.seed.rfm)
importance(hulled.seed.rfm)























## File paths for cropped images
paths_cropped_IWG.seed<- list.files(path=folder_crop_img,full.names = TRUE)
names_cropped_IWG.seed<- list.files(path=folder_crop_img,full.names = FALSE) 
#************************#
## Folders to export images from each feature (ex. hulled seeds)
## Note that these match the folders created in the beginning of the script
folder_crop_img<-  (paste(img_IWG_seed.demo.1.30.21,"crop_img",sep = "/"))
folder_chaff_01<-  (paste(img_IWG_seed.demo.1.30.21,"chaff_01",sep = "/"))
folder_chaff_02<-  (paste(img_IWG_seed.demo.1.30.21,"chaff_02",sep = "/"))
folder_chaff_03<-  (paste(img_IWG_seed.demo.1.30.21,"chaff_03",sep = "/"))
folder_dehulled.seed_01<-  (paste(img_IWG_seed.demo.1.30.21,"dehulled seed_01",sep = "/"))
folder_dehulled.seed_02<-  (paste(img_IWG_seed.demo.1.30.21,"dehulled seed_02",sep = "/"))
folder_dehulled.seed_03<-  (paste(img_IWG_seed.demo.1.30.21,"dehulled seed_03",sep = "/"))
folder_hulled.seed_01<-  (paste(img_IWG_seed.demo.1.30.21,"hulled seed_01",sep = "/"))
folder_hulled.seed_02<-  (paste(img_IWG_seed.demo.1.30.21,"hulled seed_02",sep = "/"))
folder_hulled.seed_03<-  (paste(img_IWG_seed.demo.1.30.21,"hulled seed_03",sep = "/"))
folder_ergot_01<-  (paste(img_IWG_seed.demo.1.30.21,"ergot_01",sep = "/"))
folder_ergot_02<-  (paste(img_IWG_seed.demo.1.30.21,"ergot_02",sep = "/"))
folder_ergot_03<-  (paste(img_IWG_seed.demo.1.30.21,"ergot_03",sep = "/"))
#************************#
## Empty data frame to fill with image data
img.stats_IWG_seed.demo.1.30.21<- data.frame()






















start<- Sys.time() #tracking time to completion***
for (i in 1:length(paths_cropped_IWG.seed)) {
  #************************#
  #************************#
  ##### Step 1 
  img.crop<- readImage(paths_cropped_IWG.seed[i]) #read in the first image to use for its dimensions later on***
  img.dat.crop<- ImgToDataframe(paths_cropped_IWG.seed[i]) #vectorize the RBG array***
  img.dat.crop$classify<- predict(rfm_IWG_categorical, img.dat.crop) #use the randomforest to predict a category for each pixel***
  img.dat.categorical<- img.dat.crop %>% #organize the output so there is a column for each categorical feature (ergot, seeds...)***
    mutate(chaff =       case_when(classify %in% c(0,2,3,4) ~ 0,
                                   classify == 1 ~ 1),
           dehulled.seed=case_when(classify %in%  c(0,1,3,4) ~ 0,
                                   classify == 2 ~ 1),
           hulled.seed  =case_when(classify %in% c(0,1,2,4) ~ 0,
                                   classify == 3 ~ 1),
           ergot        =case_when(classify %in%  c(0,1,2,3) ~ 0,
                                   classify == 4 ~ 1)
    )
  #************************#
  #************************#
  ##### Step 2
  ## creating a mask or grey scale image form the predictions in the random forest model
  img.chaff.1<- matrix(img.dat.categorical$chaff, nrow=nrow(img.crop), ncol=ncol(img.crop)) #mask for chaff***
  img.dehulled.seed.1<- matrix(img.dat.categorical$dehulled.seed, nrow=nrow(img.crop), ncol=ncol(img.crop)) #mask for dehulled seed***
  img.hulled.seed.1<- matrix(img.dat.categorical$hulled.seed, nrow=nrow(img.crop), ncol=ncol(img.crop)) #mask for hulled seed***
  img.ergot.1<- matrix(img.dat.categorical$ergot, nrow=nrow(img.crop), ncol=ncol(img.crop)) #mask for ergot seed***
  #************************#
  #************************#
  ##### Step 3
  ## Conduct morphological operations on chaff
  disc = makeBrush(19, "diamond") #setting the shape for adaptive thresholding***
  disc = disc / sum(disc) #setting the shape for adaptive thesholding***
  img.chaff.2<- filter2( img.chaff.1, disc ) > .3 #change the proportion after the ">" the change the global thresholding, .3 seems to work best*** 
  img.chaff.overlay<- ImgOverlay(img.crop, img.chaff.2) #overlaying the original image with the mask***
  img.chaff.paint<- paintObjects(img.chaff.2, img.crop, #drawing a line around the features in the image***
                                 col='#ffff00', 
                                 thick = T)
   ## ****NOTE**** use the function display(img.chaff.paint) to see the images being produced outside the loop***
  writeJPEG(img.chaff.overlay, paste(folder_chaff_01, "/", names_cropped_IWG.seed[i],"_chaff_01.jpeg" ,sep=""),
            quality = 100) #write the image to the hard drive***
  writeImage(img.chaff.paint, paste(folder_chaff_02, "/", names_cropped_IWG.seed[i],"_chaff_02.jpeg" ,sep=""),
             quality = 100)
  
  ## Conduct morphological operations on dehulled seeds
  img.dehulled.seed.2<- fillHull(img.dehulled.seed.1) #the dilate function will expand the masked area around the current prediction***
  img.dehulled.seed.3<- dilate(gblur(img.dehulled.seed.2, sigma = 2) > .6) #gblur will fill holes in object features then a global threshold (the proportion) will remove some low probability pixels from the mask***
  img.dehulled.seed.overlay<- ImgOverlay(img.crop,img.dehulled.seed.3)
  img.dehulled.seed.paint<- paintObjects(img.dehulled.seed.3, img.crop, col = '#000000', thick = T)
  writeJPEG(img.dehulled.seed.overlay, paste(folder_dehulled.seed_01, "/", names_cropped_IWG.seed[i],"_dehulled_01.jpeg" ,sep=""), 
            quality = 100)
  writeImage(img.dehulled.seed.paint, paste(folder_dehulled.seed_02, "/", names_cropped_IWG.seed[i],"_dehulled_02.jpeg" ,sep=""), 
             quality = 100)
  
  
  ## Conduct morphological operations on hulled seeds
  img.hulled.seed.2<- gblur(img.hulled.seed.1, sigma = 5) > .4
  img.hulled.seed.overlay<- ImgOverlay(img.crop, img.hulled.seed.2)
  img.hulled.seed.paint<- paintObjects(img.hulled.seed.2, img.crop, col = '#4169e1', thick = T)
    writeJPEG(img.hulled.seed.overlay, paste(folder_hulled.seed_01, "/", names_cropped_IWG.seed[i],"_hulled_01.jpeg" ,sep=""),
            quality = 100)
  writeImage(img.hulled.seed.paint, paste(folder_hulled.seed_02, "/", names_cropped_IWG.seed[i],"_hulled_02.jpeg" ,sep=""),
             quality = 100)
  
  ## Conduct morphological operations on ergot
  img.ergot.2<- dilate(gblur(img.ergot.1, sigma = 2) > .5)
  img.ergot.3<- dilate(gblur(img.ergot.2, sigma = 2) > .1)
  img.ergot.overlay<- ImgOverlay(img.crop,img.ergot.3)
  img.ergot.paint<- paintObjects(img.ergot.3, img.crop, col = "#000080", thick = T, closed = T)
  writeJPEG(img.ergot.overlay, paste(folder_ergot_01, "/", names_cropped_IWG.seed[i],"_ergot_01.jpeg" ,sep=""), 
            quality = 100)
  writeImage(img.ergot.paint, paste(folder_ergot_02, "/", names_cropped_IWG.seed[i],"_ergot_02.jpeg" ,sep=""), 
             quality = 100)
} 
end<- Sys.time() #tracking time to completion***
section_I<- end-start;section_I






















start<- Sys.time() #tracking time to completion***
for (i in 1:length(paths_cropped_IWG.seed)) {
  img.crop<-flop(readImage(paths_cropped_IWG.seed[i]))
  #************************#
  #************************#
  chaff.dat<- ImgToDataframe(list.files(path=folder_chaff_01,full.names = TRUE)[i])
  dehulled.seed.dat<- ImgToDataframe(list.files(path=folder_dehulled.seed_01,full.names = TRUE)[i])
  hulled.seed.dat<- ImgToDataframe(list.files(path=folder_hulled.seed_01,full.names = TRUE)[i])
  ergot.dat<- ImgToDataframe(list.files(path=folder_ergot_01,full.names = TRUE)[i])
  #************************#
  #************************#
  chaff.dat$classify<- predict(chaff.rfm, chaff.dat)
  chaff.dat$thresh<- ifelse(chaff.dat$classify>0.10, #all pixels above 0.1 probability of chaff, global thresholding is usually a pretty good idea***
                            chaff.dat$classify, 0)
  dehulled.seed.dat$classify<- predict(dehulled.seed.rfm, dehulled.seed.dat)
  dehulled.seed.dat$thresh<- ifelse(dehulled.seed.dat$classify>0.20, #all pixels above 0.3 probability of dehulled seed***
                                    dehulled.seed.dat$classify, 0)
  hulled.seed.dat$classify<- predict(hulled.seed.rfm, hulled.seed.dat)
  hulled.seed.dat$thresh<- ifelse(hulled.seed.dat$classify>0.2, #all pixels above 0.1 probability of hulled seed***
                                  hulled.seed.dat$classify, 0)
  ergot.dat$classify<- predict(ergot.rfm, ergot.dat)
  ergot.dat$thresh<- ifelse(ergot.dat$classify>0.8, #all pixels above 0.1 probability of ergot***
                            ergot.dat$classify, 0)
  #************************#
  #************************#
  img.chaff<- matrix(chaff.dat$thresh, nrow=nrow(img.crop), ncol=ncol(img.crop)) ##NOTE good*********
  wtrshed.chaff<- flop(rotate(watershed(img.chaff, 
                                 ext = 15), angle = 90))
  writeImage(colorLabels(wtrshed.chaff), paste(folder_chaff_03, "/", names_cropped_IWG.seed[i],"_chaff_03.jpeg" ,sep=""), 
             quality = 100)
  mesur.chaff.dat<- data.frame(computeFeatures.shape(wtrshed.chaff, ref=img.crop)) %>%
    filter(s.area > 30)
  #************************#
  img.dehulled.seed<- matrix(dehulled.seed.dat$thresh, nrow=nrow(img.crop), ncol=ncol(img.crop)) ##NOTE more training needed*************
  wtrshed.dehulled.seed<- flop(rotate(watershed(distmap(img.dehulled.seed), 
                                         ext = 1,
                                         tolerance = 1), angle = 90))
  writeImage(colorLabels(wtrshed.dehulled.seed), paste(folder_dehulled.seed_03, "/", names_cropped_IWG.seed[i],"_dehulled.seed_03.jpeg" ,sep=""), 
             quality = 100)
  mesur.dehulled.seed.dat<- data.frame(computeFeatures.shape(wtrshed.dehulled.seed, ref=img.crop)) %>%
    filter(s.area > 100)
  #************************#
  img.hulled.seed<- matrix(hulled.seed.dat$thresh, nrow=nrow(img.crop), ncol=ncol(img.crop)) ##NOTE more training needed*************
  wtrshed.hulled.seed<- flop(rotate(watershed(distmap(img.hulled.seed),
                                       ext = 3, tolerance = 3), angle = 90))
  writeImage(colorLabels(wtrshed.hulled.seed), paste(folder_hulled.seed_03, "/", names_cropped_IWG.seed[i],"_hulled.seed_03.jpeg" ,sep=""),
             quality = 100)
  mesur.hulled.seed.dat<- data.frame(computeFeatures.shape(wtrshed.hulled.seed, ref=img.crop)) %>% #issue that df doesn't have s.area
    filter(s.area > 200)
  #************************#
  img.ergot<- matrix(ergot.dat$thresh, nrow=nrow(img.crop), ncol=ncol(img.crop)) ##NOTE good*********
  wtrshed.ergot<- flop(watershed(img.ergot, 
                                 ext = 10)) 
  writeImage(colorLabels(wtrshed.ergot), paste(folder_ergot_03, "/", names_cropped_IWG.seed[i],"_ergot_03.jpeg" ,sep=""), 
             quality = 100)
  mesur.ergot.dat<- data.frame(computeFeatures.shape(wtrshed.ergot, ref=img.crop)) %>%
    filter(s.area > 10)
  #************************#
  write.stats<- data.frame(img.ID=                names_cropped_IWG.seed[i], #writting the data frame with measured features***
                           chaff.total.pixels=    sum(mesur.chaff.dat$s.area),
                           chaff.total.objects=   length(mesur.chaff.dat$s.area),
                           chaff.avg.size=        mean(mesur.chaff.dat$s.area),
                           dehulled.total.pixels= sum(mesur.dehulled.seed.dat$s.area),
                           dehulled.total.objects=length(mesur.dehulled.seed.dat$s.area),
                           dehulled.avg.size=     mean(mesur.dehulled.seed.dat$s.area),
                           dehulled.avg.length=   2*mean(mesur.dehulled.seed.dat$s.radius.max),
                           hulled.total.pixels=   sum(mesur.hulled.seed.dat$s.area),
                           hulled.total.objects=  length(mesur.hulled.seed.dat$s.area),
                           hulled.avg.size=       mean(mesur.hulled.seed.dat$s.area),
                           hulled.avg.length=     2*mean(mesur.hulled.seed.dat$s.radius.max),
                           ergot.total.object=    length(mesur.ergot.dat$s.area),
                           ergot.avg.size=        mean(mesur.ergot.dat$s.area))
  
  img.stats_IWG_seed.demo.1.30.21<-rbind(img.stats_IWG_seed.demo.1.30.21, write.stats) 
}
end<- Sys.time() #tracking time to completion***
section_II<- end-start;section_II 
totaltime<-section_I+section_II;totaltime





























img.stats<- img.stats_IWG_seed.demo.1.30.21 %>%
  mutate(filename = img.ID) 

dat<-left_join(img.stats, IWG_seed.dat);head(dat)

View(dat)

dat %>% 
  write.csv("garrett.parameters.2.csv")


