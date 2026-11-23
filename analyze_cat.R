#!/usr/bin/env Rscript
# Monte Carlo ENEM CAT; LC constrained to 40 common + 5 items of one language.
a<-commandArgs(TRUE);opt<-function(k,d){h<-a[startsWith(a,paste0("--",k,"="))];if(length(h))sub(paste0("--",k,"="),"",h[1],fixed=TRUE)else d};s<-grep("^--file=",commandArgs(),value=TRUE);root<-dirname(normalizePath(if(length(s))sub("^--file=","",s[1])else"analyze_cat.R",mustWork=FALSE));source(file.path(root,"cat.R"));if(!requireNamespace("arrow",quietly=TRUE))stop("Install arrow.")
n<-as.integer(opt("n","50"));reps<-as.integer(opt("replications","100"));seed<-as.integer(opt("seed","2025"));if(is.na(n)||n<1||is.na(reps)||reps<2)stop("Require --n>=1 and --replications>=2.")
areas<-c(LC="Linguagens, Códigos e suas Tecnologias",CH="Ciências Humanas e suas Tecnologias",CN="Ciências da Natureza e suas Tecnologias",MT="Matemática e suas Tecnologias");indir<-file.path(root,"data","processed");od<-file.path(root,"results");dir.create(od,recursive=TRUE,showWarnings=FALSE)
read_bank<-function(f){if(!file.exists(f))stop("Missing item bank: ",f);validate_bank(as.data.frame(arrow::read_parquet(f)))}
allp<-list();k<-1L
for(ar in names(areas))for(lang in if(ar=="LC")c("english","spanish")else NA_character_){
 f<-if(ar=="LC")file.path(indir,paste0("enem_lc_",lang,"_item_bank.parquet"))else file.path(indir,paste0("enem_",tolower(ar),"_item_bank.parquet"));bank<-read_bank(f)
 if(ar=="LC"){
  quotas<-c(common=40L);quotas<-c(quotas,setNames(5L,lang));group_col<-"item_group";avail<-table(factor(as.character(bank$item_group),levels=names(quotas)))
  if(any(avail<quotas))stop("LC ",lang," cannot form exact 40+5 bank after exclusions. Available: ",paste(names(avail),avail,sep="=",collapse=", "),". No substitution is allowed.")
  max_items<-45L
 }else{quotas<-NULL;group_col<-NULL;max_items<-nrow(bank)}
 for(r in seq_len(reps)){
  this_seed<-seed+100000L*match(ar,names(areas))+1000L*ifelse(is.na(lang),0L,match(lang,c("english","spanish")))+r
  z<-run_comparison(bank,n_persons=n,max_items=max_items,target_sd=.35,grid_points=301,seed=this_seed,quotas=quotas,group_col=group_col)
  for(v in c("n_common","n_english","n_spanish"))if(!v%in%names(z))z[[v]]<-NA_integer_
  if(!"structure_valid"%in%names(z))z$structure_valid<-NA
  if(ar=="LC"&&!all(z$n_items==45L&z$n_common==40L&z[[paste0("n_",lang)]]==5L&z$structure_valid))stop("LC test composition validation failed for ",lang,".")
  z$AREA<-ar;z$area_name<-unname(areas[ar]);z$language<-lang;z$replication<-r;z$eligible_pool_items<-nrow(bank);z$language_alternatives_in_test<-if(ar=="LC")1L else NA_integer_;allp[[k]]<-z;k<-k+1L
 }
}
person<-do.call(rbind,allp);person$language_group<-ifelse(is.na(person$language),"none",person$language);write.csv(person,file.path(od,"enem_simulation_person_level.csv"),row.names=FALSE)
grp<-split(person,interaction(person$AREA,person$language_group,person$replication,person$method,drop=TRUE));rs<-do.call(rbind,lapply(grp,function(z)data.frame(AREA=z$AREA[1],area_name=z$area_name[1],language=z$language[1],replication=z$replication[1],method=z$method[1],eligible_pool_items=z$eligible_pool_items[1],rmse=sqrt(mean(z$rmse_error^2)),mae=mean(abs(z$rmse_error)),bias=mean(z$rmse_error),mean_items=mean(z$n_items),mean_posterior_sd=mean(z$posterior_sd),target_attainment=mean(z$posterior_sd<=.35+1e-10),full_length_rate=mean(z$n_items==if(z$AREA[1]=="LC")45L else z$eligible_pool_items[1]),structure_valid=if(all(is.na(z$structure_valid)))NA else all(z$structure_valid))))
write.csv(rs,file.path(od,"enem_simulation_replication_summaries.csv"),row.names=FALSE)
sg<-split(rs,interaction(rs$AREA,ifelse(is.na(rs$language),"none",rs$language),rs$method,drop=TRUE));su<-do.call(rbind,lapply(sg,function(z)data.frame(AREA=z$AREA[1],area_name=z$area_name[1],language=z$language[1],method=z$method[1],eligible_pool_items=z$eligible_pool_items[1],replications=nrow(z),rmse=mean(z$rmse),mae=mean(z$mae),bias=mean(z$bias),mean_items=mean(z$mean_items),mean_posterior_sd=mean(z$mean_posterior_sd),target_attainment=mean(z$target_attainment),full_length_rate=mean(z$full_length_rate),structure_valid=if(all(is.na(z$structure_valid)))NA else all(z$structure_valid))))
write.csv(su,file.path(od,"enem_simulation_summary.csv"),row.names=FALSE)
metrics<-c("rmse","mae","bias","mean_items","mean_posterior_sd","target_attainment","full_length_rate");pd<-list();j<-1L
for(ar in names(areas))for(lang in if(ar=="LC")c("english","spanish")else NA_character_)for(met in metrics){sel<-rs$AREA==ar&if(is.na(lang))is.na(rs$language)else rs$language==lang;z<-rs[sel,c("replication","method",met),drop=FALSE];w<-reshape(z,idvar="replication",timevar="method",direction="wide");dlt<-w[[paste0(met,".entropy")]]-w[[paste0(met,".fisher")]];pd[[j]]<-data.frame(AREA=ar,area_name=unname(areas[ar]),language=lang,metric=met,contrast="EPE - Fisher",mean_difference=mean(dlt),median_difference=median(dlt),lower_95_MC=unname(quantile(dlt,.025)),upper_95_MC=unname(quantile(dlt,.975)),replications=length(dlt));j<-j+1L}
write.csv(do.call(rbind,pd),file.path(od,"enem_paired_method_differences.csv"),row.names=FALSE)
cat("Simulations complete. CH/CN/MT retain SD<=0.35 or bank exhaustion. LC tests are fixed at 45 items (40 common + 5 English OR Spanish); SD<=0.35 is measured at test end, not used for early termination.\n")
