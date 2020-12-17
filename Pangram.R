rm(list =ls())
setwd("C:/Users/rui.lee/Dropbox/Summit/Projects/Pangram/")

vowels      <- c("a", "e", "i", "o", "u")
words       <- read.table("words2.txt", sep = "\t",  comment.char = "")[ ,1]
wordsList   <- strsplit(paste(as.character(words)), "")
wordsList   <- wordsList[-which(duplicated(wordsList))]
importance  <- lapply(wordsList, FUN = function(x) length(x) - length(unique(x)))


temp                <- wordsList[which(importance == 0)]
occurance           <- sort(table(unlist(wordsList)))
occurance[vowels]   <- max(occurance)*1.5
while(T){
        new_temp            <- temp 
        selectedAlphWeights <- unlist(lapply(temp, FUN = function(x) sum(occurance[x])/length(x)))
        alphaFinite         <- which(is.infinite(selectedAlphWeights))
        if (length(alphaFinite) >0){
          new_temp[which(alphaFinite)] <- NULL
          selectedAlphWeights <- selectedAlphWeights[-alphaFinite]
        }
        temp_ranked         <- temp[order(selectedAlphWeights)]
        selectedAlphWeights <- sort(selectedAlphWeights)
        alphaOrdered  <- names(occurance)
        occurance_new <- occurance
        words         <- vector()
        characters    <- vector()
        
        while (T){
          
            chosen_occ <- occurance_new[which(!alphaOrdered %in% characters)]
            if (length(chosen_occ) == 0) break
            
            weighting  <- as.vector(chosen_occ/max(chosen_occ))
            pos_occ    <- runif(1, min(weighting), weighting[min(length(weighting),5)])
            n          <- names(tail(chosen_occ[which(weighting <= pos_occ)],1))
            
            
            if (n %in% characters) break
            
            slected_Ind     <- unlist(lapply(temp_ranked, FUN= function(x) n %in% x))
            slected_Words   <- temp_ranked[slected_Ind]
            
            
            slected_Weights <- selectedAlphWeights[slected_Ind]
            slected_Prob    <- slected_Weights/max(slected_Weights)
            
            if (sum(slected_Ind) <=2) break
            
            while(T){
                uni_sample    <- runif(1, min(slected_Prob), max(slected_Prob))
                sample        <- runif(1,min(slected_Prob), min(min(slected_Prob)*1.5, max(slected_Prob)))
                #print(paste( uni_sample, sample))
                if (uni_sample <= sample) {
                  slected_Pos  <- max(which(slected_Prob < uni_sample))
                  break
                }
            }
            theWord         <- slected_Words[[slected_Pos]]
            words           <- paste(words,paste0(theWord, collapse = ""))
            characters      <- c(characters, theWord)
            theWordnotV     <- vowels[table(characters)[vowels]>2]
            theWordnotV     <- theWordnotV[which(!is.na(theWordnotV))]
            occurance_new[theWord]      <- occurance_new[theWord] + max(occurance_new)
            occurance_new               <- sort(occurance_new)
            alphaOrdered                <- names(occurance_new)   
            delete_Ind_1                <- unlist(lapply(new_temp, FUN = function(x) paste0(x, collapse = "") == paste0(theWord, collapse = "")))
            delete_Ind_2                <- unlist(lapply(new_temp, FUN = function(x) any(theWordnotV %in% x) ))
            new_temp[which(delete_Ind_1 |delete_Ind_2)] <-NULL
            selectedAlphWeights         <- unlist(lapply(new_temp, FUN = function(x) sum(occurance_new[x])/length(x)))
            temp_ranked                 <- new_temp[order(selectedAlphWeights)]
            selectedAlphWeights         <- sort(selectedAlphWeights)  
            #print(c(length(characters), words))
            #print(length(new_temp))
        
        }
        
        if (length(unique(characters) ==26)){
            write.table(matrix(c(length(characters), words), nrow = 1), file = "Pangram.csv", append = T, col.names = F, row.names = F, sep = ",")
            print(c(length(characters), words))
        }

}

