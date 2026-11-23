# Teste sintético das quotas LC; requer apenas base R.
# Prefere a implementação proposta; usa cat.R se já tiver sido substituído.
cat_file <- if (file.exists("cat_updated.R")) "cat_updated.R" else "cat.R"
source(cat_file)
set.seed(81)
bank <- data.frame(item_id=paste0("q",1:50),
  item_group=c(rep("common",40),rep("english",5),rep("spanish",5)),
  a=runif(50,.7,1.8),b=runif(50,-2,2),c=runif(50,.1,.25))
for(language in c("english","spanish")){
  quotas<-c(common=40L,setNames(5L,language))
  z<-run_comparison(bank,n_persons=4,max_items=45,target_sd=.6,grid_points=51,
    seed=100+match(language,c("english","spanish")),quotas=quotas,group_col="item_group")
  stopifnot(nrow(z)==8L,all(z$n_items==45L),all(z$n_common==40L),
            all(z[[paste0("n_",language)]]==5L),all(z$structure_valid))
}
cat("LC structure checks passed for English and Spanish alternatives.\n")
