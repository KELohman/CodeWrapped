
# function to get just the raw recap numbers
#arg1 = Region 1
#arg2 = Region 2
#arg3 = df of encoutners 
raw_index <- function(arg1,arg2, arg3){
  raw_list <- c()   # set up empty list for function output
  R1 = arg1
  reglist = arg2
  interchange_encounters = arg3
  for (i in 1:(length(reglist))){
    R2 = reglist[i]
    if (R1 == R2){
      within_regionRecaps <- interchange_encounters %>%
        filter(Region == R1) %>% 
        group_by(GeneticID) %>% # group by region, indiv
        tally() %>%
        filter(n > 1) 
      out <- nrow(within_regionRecaps)
    }  
    else{
      # individuals in R1
      R1_indivs <- interchange_encounters %>% 
        filter(Region == R1) %>% 
        distinct(GeneticID,.keep_all = TRUE)
      # individuals in R2
      R2_indivs <- interchange_encounters %>% 
        filter(Region == R2) %>% 
        distinct(GeneticID,.keep_all = TRUE)
      # recpatured individuals
      recaptures <- R1_indivs %>% 
        filter(GeneticID %in% R2_indivs$GeneticID)# this makes a tiny df of individuals sighted in R1 and R2
      out <- nrow(recaptures)
    }
    raw_list[[i]] <- out
  }
  return(raw_list)
}

# function to get table of interchange indices. NA on diagonals.
#arg1 = Region 1
#arg2 = Region 2
interchange_index <- function(arg1,arg2,arg3){
  index_list <- c()   # set up empty list for function output
  R1 = arg1
  reglist = arg2
  interchange_encounters = arg3
  for (i in 1:(length(reglist))){
    R2 = reglist[i]
    if (R1 == R2){
      index <- "-"
    }  
    else{
      # individuals in R1
      R1_indivs <- interchange_encounters %>% 
        filter(Region == R1) %>% 
        distinct(GeneticID,.keep_all = TRUE)
      # individuals in R2
      R2_indivs <- interchange_encounters %>% 
        filter(Region == R2) %>% 
        distinct(GeneticID,.keep_all = TRUE)
      # recpatured individuals
      recaptures <- R1_indivs %>% 
        filter(GeneticID %in% R2_indivs$GeneticID) # this makes a tiny df of individuals sighted in R1 and R2
      # index calc
      index <- ( (length(unique(recaptures$GeneticID))) /((length(unique(R1_indivs$GeneticID))) * (length(unique(R2_indivs$GeneticID))) )*1000)
      index <- round(index,2)
    }
    index_list[[i]] <- index
  }
  return(index_list)
}

# list of regions for function to fiter on....
regionlist <- c("NBC","SBC-WA","OR","CA-North","CA-Central","CA-South")

# wrapper interchange function - spits out split matrix w/ssizes in first column
# var1 = regions list
#var2 = subset dfs.
interchange_wrapper <- function(var1,var2){
  regions = var1
  data = var2
  
  # iter through the reg list of possibilities for interchange index
  test <- lapply(regions, interchange_index, arg2=regions, arg3 = data)
  # r smash together with a lil do.call and rbind
  test <- do.call(rbind,test)
  # iter through list for raw recap numbers
  t <- lapply(regions, raw_index, arg2=regions,arg3=data)
  t <- do.call(rbind,t)
  
  # this pops out a lower tri df of the interchange values w/upper tri of sample sizes
  test[upper.tri(test)] <- t[upper.tri(t)]
  test <- as.data.frame(test)
  colnames(test) <- regions
  rownames(test) <- regions
  test <- rownames_to_column(test, "Region")
  
  # add sample size to the row names....egh
  n <- data %>%
    group_by(Region) %>% 
    distinct(GeneticID) %>% 
    tally() %>% 
    mutate(size = (paste0(Region," (n=",n,")")))
  
  final <- left_join(test,n,by="Region") %>% 
    select(-Region,-n) %>% 
    rename(Region = size) %>% 
    select(Region,1:6)
  return(final)
}

# feeding ground interchange table
peak_enc <- encounters %>% 
  dplyr::filter(between(Month, 4,11)) %>% 
  rename(Region="TrueRegion") %>% 
  interchange_wrapper(regionlist, .)




### recapture table

## funtion here
# function to get just the raw recap numbers
#arg1 = Region 1
#arg2 = Region 2
#arg3 = df of encoutners 
resight_index <- function(arg1,arg2,arg3){
  raw_resight <- c()   # set up empty list for function output
  reglist = arg1
  interchange_encounters = arg2
  for (i in 1:(length(reglist))){
    R1 = reglist[i]
      within_regionRecaps <- interchange_encounters %>%
        filter(Region == R1) %>% 
        distinct(GeneticID,Date) %>% #just to be sure
        group_by(GeneticID) %>% # group by region, indiv
        tally() %>%
        filter(n > 1) 
      
      NR <- nrow(within_regionRecaps) # number of recapts
      
      N_A <- interchange_encounters %>%
        filter(Region == R1) %>% 
        distinct(GeneticID) %>%  # number of individuals
        nrow(.)
      
      Resight <- ((NR/(N_A^2))*1000) # calculate index
      Resight <- round(Resight,digits=2)
    raw_resight[[i]] <- c(R1,NR,N_A,Resight)
  }
  
  return(raw_resight)
}

# feeding ground interchange table
peak_resight <- encounters %>% 
  dplyr::filter(between(Month, 5,10)) %>% 
  rename(Region="TrueRegion") %>% 
  resight_index(regionlist, .) %>% 
  do.call(rbind, .)
peak_resight <- as.data.frame(peak_resight) 

# peak_resight$V4 <- as.numeric(peak_resight$V4)
# 
# peak_resight %<>% 
#   mutate_if(is.numeric, round, digits=3)


  