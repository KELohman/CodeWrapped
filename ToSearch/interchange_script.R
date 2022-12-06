# work through w/bg encounters

# first find individuals w/recapture events and link to second obs....
interchange_encounters <- bg_encounters %>%
  drop_na(Date) %>% # remove encounters w/out date. just a few of em.
  filter(!Region %in% wnp) #%>% #buh bye wnp.
#  dplyr::filter(between(Month, 02,03)) # filter to right time frame hur [COME BACK TO MAKE THIS AN ARG IN A BABY FUNCTION LATERS BABE]

# start easy - number of individual recaps w/in regions and between regions.
# within_regionRecaps <- interchange_encounters %>%
#   group_by(Region, GeneticID) %>% # group by region, indiv
#   tally() %>%
#   filter(n > 1) %>%
#   summarise(n=n())

  
# function to get just the raw recap numbers
#arg1 = Region 1
#arg2 = Region 2
raw_index <- function(arg1,arg2){
  raw_list <- c()   # set up empty list for function output
  R1 = arg1
  reglist = arg2
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
interchange_index <- function(arg1,arg2){
  index_list <- c()   # set up empty list for function output
  R1 = arg1
  reglist = arg2
  for (i in 1:(length(reglist))){
    R2 = reglist[i]
    if (R1 == R2){
    index <- NA
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
    }
    index_list[[i]] <- index
  }
  return(index_list)
}


# list of regions for function to fiter on....
regions <- c("CentAm","SMex","MX-ML","MX-BC","MX-AR","Hawaii")

# iter through the reg list of possibilities for interchange index
test <- lapply(regions, interchange_index, arg2=regions)
# r smash together with a lil do.call and rbind
test <- do.call(rbind,test)

# iter through list for raw recap numbers
t <- lapply(regions, raw_index, arg2=regions)
t <- do.call(rbind,t)

test[upper.tri(test)] <- t[upper.tri(t)]
test <- as.data.frame(test)
colnames(test) <- regions
rownames(test) <- regions
test # this pops out a lower tri df of the interchange values.



#kable(exchanges, caption="Below the diagonal: Interchange index between geographic strata. High values indicate high probability of individual recapture in both populations (or a small population). Low values indicate low probability of individual recapture between regions due to population size or low rates of movement between sampling areas. Above the diagonal: raw number of individual recaptures between compared regions.")