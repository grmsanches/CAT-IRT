# CAT bayesiano 3PL: Fisher e entropia posterior esperada.
# Item parameters are externally calibrated and must be on the theta scale.
validate_bank <- function(bank) {
  required <- c("a","b","c"); missing <- setdiff(required,names(bank))
  if(length(missing)) stop("Item bank is missing columns: ",paste(missing,collapse=", "))
  bank<-as.data.frame(bank);if(!"item_id"%in%names(bank))bank$item_id<-sprintf("item_%03d",seq_len(nrow(bank)))
  for(v in required)bank[[v]]<-as.numeric(bank[[v]])
  if(!nrow(bank)||any(!is.finite(as.matrix(bank[,required]))))stop("Item bank is empty or parameters are not finite.")
  if(any(bank$a<=0)||any(bank$c<0|bank$c>=1))stop("Require a > 0 and 0 <= c < 1.")
  if(anyDuplicated(bank$item_id))stop("item_id values must be unique.")
  bank
}
prob_correct<-function(theta,a,b,c){z<-pmax(-35,pmin(35,1.7*a*(theta-b)));pmax(1e-12,pmin(1-1e-12,c+(1-c)/(1+exp(-z))))}
fisher_information<-function(theta,a,b,c){p<-prob_correct(theta,a,b,c);(1.7*a)^2*(p-c)^2*(1-p)/(p*(1-c)^2)}
normalize_log_weights<-function(x){z<-exp(x-max(x));z/sum(z)}
posterior_weights<-function(theta_grid,bank,administered,responses){lw<-dnorm(theta_grid,log=TRUE);for(k in seq_along(administered)){j<-administered[k];p<-prob_correct(theta_grid,bank$a[j],bank$b[j],bank$c[j]);lw<-lw+if(responses[k]==1)log(p)else log1p(-p)};normalize_log_weights(lw)}
posterior_summary<-function(theta_grid,w){m<-sum(theta_grid*w);c(mean=m,sd=sqrt(max(0,sum((theta_grid-m)^2*w))))}
posterior_entropy<-function(w)-sum(w*log(pmax(w,1e-300)))
choose_fisher<-function(theta_grid,w,bank,available){th<-sum(theta_grid*w);available[which.max(fisher_information(th,bank$a[available],bank$b[available],bank$c[available]))]}
choose_entropy<-function(theta_grid,w,bank,available){h<-posterior_entropy(w);g<-vapply(available,function(j){p<-prob_correct(theta_grid,bank$a[j],bank$b[j],bank$c[j]);p1<-sum(w*p);w1<-normalize_log_weights(log(pmax(w,1e-300))+log(p));w0<-normalize_log_weights(log(pmax(w,1e-300))+log1p(-p));h-p1*posterior_entropy(w1)-(1-p1)*posterior_entropy(w0)},numeric(1));available[which.max(g)]}
validate_quotas<-function(bank,quotas,group_col){if(is.null(names(quotas))||!group_col%in%names(bank))stop("Quotas need names and a matching item group column.");if(any(quotas<0)||any(quotas!=as.integer(quotas)))stop("Quotas must be nonnegative integers.");counts<-table(factor(as.character(bank[[group_col]]),levels=names(quotas)));short<-names(quotas)[counts<quotas];if(length(short))stop("Not enough eligible items for required group quota(s): ",paste(short,collapse=", "));sum(quotas)}
simulate_one<-function(theta_true,bank,method,theta_grid,max_items,target_sd,quotas=NULL,group_col=NULL){
 administered<-integer();responses<-integer();
 repeat{
  w<-posterior_weights(theta_grid,bank,administered,responses);est<-posterior_summary(theta_grid,w)
  if(is.null(quotas)&&est["sd"]<=target_sd)break
  if(length(administered)>=max_items)break
  available<-setdiff(seq_len(nrow(bank)),administered)
  if(!is.null(quotas)){
   used<-if(length(administered))table(factor(as.character(bank[[group_col]][administered]),levels=names(quotas)))else setNames(rep(0L,length(quotas)),names(quotas))
   open_groups<-names(quotas)[used<quotas]
   available<-available[as.character(bank[[group_col]][available])%in%open_groups]
  }
  if(!length(available))break
  j<-if(method=="fisher")choose_fisher(theta_grid,w,bank,available)else if(method=="entropy")choose_entropy(theta_grid,w,bank,available)else stop("Unknown method: ",method)
  p<-prob_correct(theta_true,bank$a[j],bank$b[j],bank$c[j]);administered<-c(administered,j);responses<-c(responses,as.integer(runif(1)<p))
 }
 w<-posterior_weights(theta_grid,bank,administered,responses);est<-posterior_summary(theta_grid,w)
 out<-data.frame(theta_true=theta_true,theta_hat=unname(est["mean"]),posterior_sd=unname(est["sd"]),n_items=length(administered),rmse_error=unname(est["mean"]-theta_true))
 if(!is.null(quotas)){actual<-table(factor(as.character(bank[[group_col]][administered]),levels=names(quotas)));for(g in names(quotas))out[[paste0("n_",g)]]<-as.integer(actual[g]);out$structure_valid<-length(administered)==sum(quotas)&&all(as.integer(actual)==as.integer(quotas))}
 out
}
run_comparison<-function(bank,n_persons=50,max_items=nrow(bank),target_sd=0.35,grid_points=301,seed=2025,quotas=NULL,group_col=NULL){
 bank<-validate_bank(bank);if(n_persons<1||max_items<1||max_items>nrow(bank)||target_sd<=0||grid_points<51)stop("Invalid CAT configuration.")
 if(!is.null(quotas)){needed<-validate_quotas(bank,quotas,group_col);if(max_items!=needed)stop("For a quota-constrained test, max_items must equal the sum of quotas.")}
 set.seed(seed);truth<-rnorm(n_persons);grid<-seq(-4,4,length.out=grid_points);out<-vector("list",2*n_persons);k<-1L
 for(i in seq_len(n_persons))for(m in c("fisher","entropy")){z<-simulate_one(truth[i],bank,m,grid,max_items,target_sd,quotas,group_col);z$person<-i;z$method<-m;out[[k]]<-z;k<-k+1L}
 result<-do.call(rbind,out);if(!is.null(quotas)&&!all(result$structure_valid))stop("At least one simulated test violated the required item composition.");result[,c("person","method",setdiff(names(result),c("person","method")))]
}
summarize_comparison<-function(x){do.call(rbind,lapply(unique(x$method),function(m){z<-x[x$method==m,];data.frame(method=m,n=nrow(z),rmse=sqrt(mean(z$rmse_error^2)),mae=mean(abs(z$rmse_error)),bias=mean(z$rmse_error),mean_items=mean(z$n_items),median_items=median(z$n_items),mean_posterior_sd=mean(z$posterior_sd),target_attainment=mean(z$posterior_sd<=.6+1e-10))}))}
make_demo_bank<-function(n_items=45,seed=2025){set.seed(seed);data.frame(item_id=sprintf("demo_%03d",seq_len(n_items)),a=pmin(2.5,pmax(.45,rlnorm(n_items,log(1.1),.28))),b=sort(runif(n_items,-2.5,2.5)),c=runif(n_items,.12,.25))}
