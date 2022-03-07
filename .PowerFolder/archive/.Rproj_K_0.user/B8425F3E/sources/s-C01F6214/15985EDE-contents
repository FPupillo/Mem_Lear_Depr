#------------------------------------------------------------------------------#
#
# script to select the images for the task
#
# created: "Tue Nov  9 10:31:42 2021"
#
#------------------------------------------------------------------------------#
rm(list=ls())
#------------------------------------------------------------------------------#
Pcong<-0.70   # 75 for the conguent ones, the preferred category

Pincong<-(1-Pcong)/3
#------------------------------------------------------------------------------#
# source the helper functions
source("helper_functions/checkCont.R")
source("helper_functions/getSum.R")


# load the libraries
library("readxl")
library(gtools) # for permutations
library(dplyr)

# retrieve info of the category
category<-read_excel("SI1.xlsx", sheet = "Sheet2")

# check how many categories
# change fourth and fifth name
names(category)[4:5]<-c("modal_categ", "cat_agreement")

# get the number of objects per category
table<-category %>% 
  group_by(modal_categ) %>% 
  tally()

# order by modal categ and name agreement
table<-table[order(table$n, decreasing=T),]

head(table, n=50)

# select the categories
selCat<-c( "Outdoor activity & sport item", 
           "Kitchen & utensil","Electronic device & accessory", "Hand labour tool & accessory")

# create a dataset with only the categories
dataSel<-category[category$modal_categ %in% selCat, ]

# select the the first 60 images with the highest category agreement level 
# for each category, 
# assign them to the relative dataset
for (cat in 1:length(selCat)){
  # subset the database
  datatask<- dataSel[dataSel$modal_categ==selCat[cat],]
  # order it
  datatask<-datatask[order(datatask$cat_agreement, decreasing = T),]
  # select first 40 
  datatasksel<-datatask[1:60,]
  # assign to a dataset
  assign(paste(selCat[cat],"_task",sep=""), datatasksel)
  
}

# merget the datasets 
data_task<-rbind(`Electronic device & accessory_task`, `Hand labour tool & accessory_task`, 
                 `Kitchen & utensil_task`, `Outdoor activity & sport item_task`)

# create a list of characters 
characters<-c("m_black", "m_orange")
categ<-selCat

set.seed(12345)
# for the categories, to have alle the possible permutations of category 
# and position we need 4 factorial (4*3*2*1) =24
categPermut<-permutations(n=4,r=4,v=categ,repeats.allowed=F)

# now we need to have 60 per character and repeat them twice (two characters)
# then repeat the combinations two times (character).
# In total, 96 trials
allcombin<-cbind(character=rep(characters, each=nrow(categPermut)*3))

# now bind the characters with the categories
allList<-data.frame(cbind(allcombin, do.call(rbind, replicate(6, categPermut,
                                                              simplify=FALSE))))

names(allList)[2:5]<-c("left_categ", "centleft_categ", "centright_categ",
                       "right_categ")

#------------------------------------------------------------------------------#
# create the correct choice for the first character
# initialize the variable


# as for the schematics, the we need for each character two different contingencies
# After that, we can counterbalance the taught vs learned contingencies
# across participants

# contingencies
# we need two different set of contingencies. Each one of them will be repeated
# twice, with inverted taught vs learned condition

cont1<-list(c(Pcong, Pincong, Pincong,Pincong), 
            c(Pincong, Pincong, Pincong, Pcong))

cont2<-list(c(Pincong, Pcong, Pincong,Pincong), 
            c(Pincong, Pincong, Pcong, Pincong))

# counter for the list

counter<-1

for (c in 1:2){ # contingencies loop
  
  for ( j in 1: 2) { #for each contingency, loop it twice to counterbalance
    
    # initialize the vector
    AllCharList<-vector()
    for (n in 1:length(characters)){ # character loop
      
      # subset the first character's trials
      charactList<-allList[(allList$character==characters[n] ),]  
      
      set.seed(12345)
      
      # take 60only per character randomly
      charactList<-sample_n(charactList, 60)
      
      # select the contingency
      cont<-get(paste0("cont", c))
      
      # sample the correct answer
      corr_ans<-sample(categ,nrow(charactList), prob = c(cont[[n]]), replace = T)
      
      # check how many times it appears
      probs<-vector()
      for (f in 1:length(categ)){
        probs[f]<-(length(corr_ans[corr_ans==categ[f]]) / length(corr_ans))
      }
      
      print(probs)
      
      # function to redistribute the correct responses for butterfly
      while (max(probs) != 0.70  | any(probs[! probs %in% max(probs)]<0.10)){ 
        #set.seed(sample(seq(1:1000), 1))
        corr_ans<-sample(categ,nrow(charactList), prob = c(cont[[n]]), replace = T)
        probs<-vector()
        for (f in 1:length(categ)){
          probs[f]<-(length(corr_ans[corr_ans==categ[f]]) / length(corr_ans))
        }
        
        print(probs)
        
      }
      
      # we need a variable to indicate whether it is the congruent or not
      charactList$type<-as.numeric(corr_ans==categ[which(probs==max(probs))])
      
      # bind it to the final dataset
      charactList$corr_ans<- corr_ans
      
      # create taught vs learned condition
      if (j==1){                                 # depending on the condition,
        charactList$condition<-ifelse(n==1,1,0)  # we are assigning different characters
      } else {                                   # to the taught cvs learned condition
        charactList$condition<-ifelse(n==1,0,1)
      }
      
      AllCharList<-rbind(AllCharList,charactList )
      
    }
    
    # save the list
    assign(paste0("AllCharList_", counter), AllCharList)
    
    # update the counter
    counter<-counter+1
  }
}

# now we need to indicate which number is the correct character
for (i in 1:4){
  # retrieve list
  AllCharList<-get(paste0("AllCharList_",i ))
  
  AllCharList$corr_ans_num<-NA
  for (j in 1: nrow(AllCharList)){
    AllCharList$corr_ans_num[j]<- which(AllCharList[j,2:5]==AllCharList$corr_ans[j])
  }
  
  # assign list
  assign(paste0("AllCharList_",i ), AllCharList)
}

#------------------------------------------------------------------------------#
# check that the number across the characters is okay

for (l in 1:4){
  
  print(paste("list", l))  
  
  AllCharList<-get(paste0("AllCharList_",l ))
  
  print(  
    checkCont(AllCharList, 60)
  )
  
}

#------------------------------------------------------------------------------#
# now that we have created the list, we can shuffle the order
# shuffle the list : create random numbers of the length of the list
set.seed(12345)

for (l in 1:4){
  
  print(paste("list", l))  
  
  AllCharList<-get(paste0("AllCharList_",l ))

# sample from the list created to create the practice list
task_list<-sample_n(AllCharList, nrow(AllCharList))

# we don't want three consecutive incongruent trials for each 
# character
add<-getSum(task_list)

while(any(add<1, na.rm=T)){
  task_list<-sample_n(AllCharList, nrow(AllCharList))
  
  add <-getSum(task_list)
}

# check contingencies again
print(
checkCont(task_list, 60)
)

# assign list
assign(paste0("task_list_",l ), task_list)

}

#------------------------------------------------------------------------------#
# select the objects
#------------------------------------------------------------------------------#

# images that were repeated
imagestodelete<-c("djmixer01","cleaver02","fork07b","jar03","mug05",
                  "pot02a", "strainer02", "muffintray02",
                  "sandpaper", "paintscraper", "musicalwoodenspoons",
                  "icescraper","hosenozzle", "carbattery",
                  "bagtie", "backfloat" )


dataSel<-dataSel[!dataSel$Filename %in% imagestodelete, ]
# serving spoon is servingspoon01
dataSel$Filename[dataSel$Filename=="servingspoon"]<-"servingspoon01"
# dvdcase is dvdcase01
dataSel$Filename[dataSel$Filename=="dvdcase"]<-"dvdcase01"
# bowl 
dataSel$Filename[dataSel$Filename=="bowl01"]<-"bowl02a"

# how many objects remaining?
# get the number of objects per category
table<-dataSel %>% 
  group_by(modal_categ) %>% 
  tally()

# order by modal categ and name agreement
table<-table[order(table$n, decreasing=T),]

head(table, n=11)


for (l in 1:4){
  
  print(paste("list", l))  
  
  AllCharList<-get(paste0("AllCharList_",l))

  # count the trials by category
  trialN<-AllCharList %>%
    count ( corr_ans) %>%
    mutate(prop = prop.table(n))
  
  # for each category we need the number of the list for the recognition, 
  # and half of the number as new items
  # we need to split among task objects, new objects for recognition, and fillers
  
  set.seed(12345)
  #process<-function(){
   
    Imagelist<-vector()
    for (cat in 1:length(selCat)){
      # subset the database
      datatask<- dataSel[dataSel$modal_categ==selCat[cat],]
      
      # get the number of trial for that category
      trialNcat<-trialN[trialN$corr_ans==selCat[cat], "n"]
      
      # select object encoding 
      datatasksel<-datatask[sample(nrow(datatask), size = trialNcat, replace =F),] 
      
      datatasksel$kind<-"encoding"
      
      # delete those objects
      datatask<-datatask[!datatask$Filename %in% datatasksel$Filename,]
      
      # now the recognition (new objects)
      datarec<-datatask[sample(nrow(datatask), size = trialNcat/2, replace =F),]
      
      datarec$kind<-"recognition"
      
      # bind encoding and recognition
      curr_list<-rbind(datatasksel, datarec)
      
      Imagelist<-rbind(Imagelist, curr_list)
      
      # delete those objects
      datatask<-datatask[!datatask$Filename %in% datarec$Filename,]

      
    }
  
  # assign the list 
    assign(paste0("ImageList_",l), Imagelist)
}

# check if there are repeated images
for (list in 1:4){
  currlist<-get(paste0("ImageList_", list))
for (l in 1:length(currlist)){
  images<-currlist$Filename
  # exclude words that have 0 in them
  imagesNum<-images[grep('0', images)]
  # delete from the images
  images<-images[!images %in% imagesNum]
  
  # delete the double
  # first mark the one that are doubled
  imagesdoubled<-vector()
  counter<-1
  for (i in 1:length(images)){
    times<-length(images[images==images[i]])
    if ((times)>1){
      for (t in 1:(times-1)){
        imagesdoubled[counter]<-ImagSelect$Filename[ImagSelect$Modal.name==modalname[i]][t]
        counter<-counter+1
      }
    }
  }
  
  sort(imagesNum)
}
print(
imagesdoubled
)
}


# now select the images
lists<-ls(pattern = "^task_list_")

set.seed(12345)
for (l in 1:length(lists)){ # loop between lists
  
  currlist<-get(paste0("task_list_",l))
  objects<-get(paste0("ImageList_", l))
  
  # select only encoding 
  objects<-objects[objects$kind=="encoding",]
  # loop within lists
  currlist$img<-NA
  for (n in 1:nrow(currlist)){ 
    # sample the object
    # subset the data keeping only the ones with the same object cateogory
    currobjects<-objects[objects$modal_categ==currlist$corr_ans[n],]
    # sample one of them
    objectsel<-sample(currobjects$Filename, 1)
    # assign to the data
    currlist$img[n]<-paste("stimuli/objects/",objectsel, ".jpg", sep="")
    # delete that object from the objectPract
    objects<-objects[objects$Filename!=objectsel,]
  }

  currlist$character<-paste("stimuli/", currlist$character, ".jpg", sep="")

  # convert the categories into images
  categories<-c("left_categ", "centleft_categ", "centright_categ", "right_categ")
  for (c in 1:length(categories)){
    currlist[[categories[c]]]<-as.character(currlist[[categories[c]]])
  }
  
  for (n in 1:nrow(currlist)){
    for (c in 1:length(categories)){
      currlist[n, categories[c]]<-paste("stimuli/", 
                        as.character(currlist[n, categories[c]]), ".png", sep="")
    }
  }
  
  # assign the current list to the data
  assign(lists[l], currlist)
  
  # print the list
  write.table(as.data.frame(eval(parse(text = lists[l]))), 
              paste("lists/", lists[l], ".csv", sep=""),
              col.names = T,row.names = F, quote=F, sep=",")
}

#------------------------------------------------------------------------------#
# write the objects
ImagesEncoding<-unique(c(ImageList_1$Filename[ImageList_1$kind=="encoding"],
                         ImageList_2$Filename[ImageList_2$kind=="encoding"], 
                       ImageList_3$Filename[ImageList_3$kind=="encoding"],
                       ImageList_4$Filename[ImageList_4$kind=="encoding"]))

ImagesRec<-unique(c(ImageList_1$Filename[ImageList_1$kind=="recognition"],
                    ImageList_2$Filename[ImageList_2$kind=="recognition"], 
                    ImageList_3$Filename[ImageList_3$kind=="recognition"],
                    ImageList_4$Filename[ImageList_4$kind=="recognition"]))

ImagesEncoding<-paste0(ImagesEncoding, ".jpg")

ImagesRec<-paste0(ImagesRec, ".jpg")

write.table(ImagesEncoding, "lists/Images_encoding.txt", row.names=F, 
            col.names = F, quote=F)


write.table(c(ImagesEncoding, ImagesRec), "lists/Images_recognition.txt",
            row.names=F, col.names = F, quote=F)

#------------------------------------------------------------------------------#
# create lists for recognition
set.seed(12345)
for (list in 1:4){
  
  # get the list
  currlist<-get(paste0("ImageList_", list))

  # create old/new
  currlist$type<-ifelse(currlist$kind=="encoding", "old", "new")
  
  # correct answer
  currlist$corr_ans<-ifelse(currlist$type=="old", "left", "right")
  
  # select only the variables of interest
  imagesforRecog<-currlist[, c("Filename","modal_categ", "type", "corr_ans")]
  
  names(imagesforRecog)[1]<-"images"
  
  imagesforRecog$images<-paste("stimuli/objects/", imagesforRecog$images,sep="" )
  
  # shuffle
  imagesforRecog$order<-sample(seq(1:length(imagesforRecog$images)))
  imagesforRecog<-imagesforRecog[order(imagesforRecog$order),]
  
  # attach jpg
  imagesforRecog$images<-paste0(imagesforRecog$images, ".jpg")
  
  # divide into two blocks
  imagesforRecog1<-imagesforRecog[1:90,]
  imagesforRecog2<-imagesforRecog[91:180,]
  
  
  # print
  write.table(imagesforRecog1, paste("lists/ImagesforRecog", list, "_1", 
                 ".csv", sep=""),col.names = T,row.names = F, quote=F, sep=",")
  write.table(imagesforRecog2, paste("lists/ImagesforRecog", list, "_2",  
                 ".csv", sep=""),col.names = T,row.names = F, quote=F, sep=",")
  
  }

#-----------------------------end----------------------------------------------#
