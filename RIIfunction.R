## RII function
require(reshape2)
require(tidyverse)

rii.fun <- function(data, compare, treat, control){

## describe variables
dataset <- data
shrub <- enquo(treat)
open <- enquo(control)

## Determine the columns that are site identifiers (e.g. rep, site name, year) and the columns that RII should apply to - i.e. all numeric values
site <- dataset[!unlist(lapply(dataset, is.numeric))] ## Site values
nums <- dataset[unlist(lapply(dataset, is.numeric))] ## numeric values for RII
site.nocompare <- site[,!names(site)==compare]   ## drop column for comparison

## First generate a column with unique identfiers for the dataset
site.name <- names(site.nocompare) ## site names
nums.name <- names(nums) ## column names

no.compare <- unique(site.nocompare) ## find unique instances
no.compare[,"uniqueID"] <- seq(1,nrow(no.compare),1) ## add unique identifier column
unique.site <- merge(dataset, no.compare, by=c(site.name)) ## join unique identifier back to original dataset



rii <- function(x){
  f1 <- as.formula(paste("uniqueID", "~", compare))## formula for dcast
  unique.site %>% data.table::dcast(data=., f1, mean, value.var=x) %>% mutate(RII=(!!shrub-!!open)/(!!shrub+!!open)) %>%  select(RII)
}

rii.data <- data.frame(lapply(nums.name, rii))
colnames(rii.data) <- nums.name
rii.data <- cbind(site.nocompare[!duplicated(site.nocompare), ],rii.data)
rii.data[is.na(rii.data)] <- 0 ## convert NA to zeros

return(rii.data)
}
