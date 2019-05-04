read_txt <- function(file_name, file_loca){
  current_file_name <- sub(".txt","",file_name)
  current_ground_truth <- readLines(paste(file_loca,current_file_name,".txt",sep=""), encoding="UTF-8",warn=FALSE)
  return(current_ground_truth)
}

dictionary <- function(bigr_lib){
  corpus<- VCorpus(VectorSource(bigr_lib))%>%
    tm_map(content_transformer(tolower))%>%
    tm_map(removePunctuation)%>%
    tm_map(removeNumbers)%>%
    tm_map(removeWords, character(0))%>%
    tm_map(stripWhitespace)
  
  dict <- tidy(corpus) %>% select(text)  
  completed <- dict %>%
    unnest_tokens(dictionary, text) %>%
    anti_join(stop_words,by = c("dictionary" = "word")) 
  
  list <- completed$dictionary %>% paste(., collapse = " ")
  
  return(list)
}

biletter <- function(token){
  ## Split into characterss:
  w <- strsplit(token, "")[[1]]
  ## Word tri-grams pasted together:
  return(vapply(ngrams(w, 2L), paste, "", collapse = ""))
}


biletter_list <- function(corpus){
  letter <- list()
  for(i in 1:length(corpus)){
    letter[[i]]<- corpus[i] %>% as.character() %>% biletter()
  }
  letter <- unlist(letter)
  return(letter)
}


into_dataframe <- function(list){
  df <- list %>% as.character() %>% strsplit(.,"") %>% unlist() %>% matrix(.,ncol = 2, byrow = T) %>% data.frame()
  df$biletter <- list
  return(df)
}

train.ind <- function(bigr_lib){
  ind <- sample(1:length(bigr_lib), floor(length(bigr_lib)*0.7), replace = F)
  return(ind)
}

density <- function(reference){
  trans.mat=matrix(0,27,27)
  rownames(trans.mat)=colnames(trans.mat)=c(tolower(letters),"")
  lastletter=""
  
  for (ln in 1:length(reference)) {
    if (ln %% 1000 ==0) {cat("Line",ln,"\n")}
    for (pos in 1:nchar(reference[ln])) {
      curletter=substring(reference[ln],pos,pos)
      
      if (curletter %in% tolower(letters)) {
        trans.mat[rownames(trans.mat)==lastletter,
                  colnames(trans.mat)==curletter]=
          trans.mat[rownames(trans.mat)==lastletter,
                    colnames(trans.mat)==curletter]+1
        lastletter=curletter
      } else {
        
        if (lastletter!="") {
          trans.mat[rownames(trans.mat)==lastletter,27]=
            trans.mat[rownames(trans.mat)==lastletter,27]+1
          lastletter=""
        }
      }
    }
    curletter=""
    
    if (lastletter!="") {
      trans.mat[rownames(trans.mat)==lastletter,27]=
        trans.mat[rownames(trans.mat)==lastletter,27]+1
    }
    lastletter=""
  }
  
  trans.prob.mat <- (trans.mat+1) / sum(trans.mat)
  all_letters <- expand.grid(colnames(trans.prob.mat),rownames(trans.prob.mat)) %>%
    unite(., letters, c(Var1, Var2), remove=T, sep = "") %>% as.matrix() %>% as.vector()
  freq <- as.vector(trans.prob.mat)
  names(freq) <- all_letters
  return(freq)
}


extract.features <- function(bigr_lib,k = NA){
  features <- matrix(NA, nrow = length(bigr_lib), ncol = 729)
  for(i in 1:length(bigr_lib)){
    features[i,] <- bigr_lib[[i]] %>% dictionary() %>% density()
    colnames(features) <- names(b)
  }
  labels <- rep(k, length(bigr_lib))
  return(list(features = features, labels = labels))
}



KLD.label <- function(file){
  
  kld_busi <- KLD(busi_train,file)$sum.KLD.px.py/KLD(busi_train,rep(0,729))$sum.KLD.px.py
  kld_politics <- KLD(politics_train,file)$sum.KLD.px.py/KLD(politics_train,rep(0,729))$sum.KLD.px.py
  kld_entertain <- KLD(entertain_train,file)$sum.KLD.px.py/KLD(entertain_train,rep(0,729))$sum.KLD.px.py
  kld_sport <- KLD(sport_train,file)$sum.KLD.px.py/KLD(sport_train,rep(0,729))$sum.KLD.px.py
  kld_tech <- KLD(tech_train,file)$sum.KLD.px.py/KLD(tech_train,rep(0,729))$sum.KLD.px.py
  
  kld.dist <- cbind(kld_busi, kld_politics, kld_entertain, kld_sport, kld_tech)
  labels <- c("business", "politics", "entertainment","sport","technology")
  
  ind <- which.min(kld.dist)
  return(labels[ind])
}


KLD.pred <- function(train_feat){
 
   kld_train_pred <- rep(NA, nrow(train_feat$features))
  for(i in 1:length(train_feat$labels)){
    kld_train_pred[i] <-   KLD.label(train_feat$features[i,])
  }
  
  return(kld_train_pred)
}




