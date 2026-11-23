#!/usr/bin/env Rscript
# ENEM 2023: preparação por área, exclusões auditáveis e alternativas LC.
suppressPackageStartupMessages({
        for(p in c("data.table","arrow"))
                if(!requireNamespace(p, quietly=TRUE))
                        stop("Instale ",p,".")
        library(data.table)
})
a<-commandArgs(TRUE);opt<-function(k,d){h<-a[startsWith(a,paste0("--",k,"="))];if(length(h))sub(paste0("--",k,"="),"",h[1],fixed=TRUE)else d};sf<-grep("^--file=",commandArgs(),value=TRUE);root<-dirname(normalizePath(if(length(sf))sub("^--file=","",sf[1])else"prepare_ENEM.R",mustWork=FALSE));d0<-file.path(root,"microdados_enem_2023","DADOS");src<-normalizePath(opt("data-dir",if(dir.exists(d0))d0 else file.path(root,"..","microdados_enem_2023","DADOS")),mustWork=FALSE);out<-file.path(root,"data","processed");dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(!file.exists(file.path(src,"MICRODADOS_ENEM_2023.csv"))||!file.exists(file.path(src,"ITENS_PROVA_2023.csv")))stop("Microdados/ITENS_PROVA não encontrados em ",src)
areas<-c(CH="Ciências Humanas e suas Tecnologias",LC="Linguagens, Códigos e suas Tecnologias",CN="Ciências da Natureza e suas Tecnologias",MT="Matemática e suas Tecnologias");
log_progress <- function(...) {
  cat(
    sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")),
    paste0(..., collapse = ""),
    "\n",
    sep = ""
  )
  flush.console()
}

log_progress("Preparation started.");forms<-list(LC=as.character(1201:1204),CH=as.character(1191:1194),CN=as.character(1221:1224),MT=as.character(1211:1214))
cc<-c("NU_INSCRICAO","NU_ANO","TP_FAIXA_ETARIA","TP_SEXO","TP_COR_RACA","TP_ESCOLA","SG_UF_ESC","TP_ST_CONCLUSAO","IN_TREINEIRO","TP_LINGUA",unlist(lapply(names(areas),function(x)c(paste0("TP_PRESENCA_",x),paste0("CO_PROVA_",x),paste0("NU_NOTA_",x),paste0("TX_RESPOSTAS_",x),paste0("TX_GABARITO_",x)))))
cand<-data.table::fread(file.path(src,"MICRODADOS_ENEM_2023.csv"),select=cc,na.strings=c("","NA"));if(length(setdiff(cc,names(cand))))stop("Variáveis de candidatos ausentes: ",paste(setdiff(cc,names(cand)),collapse=", "))
ic<-c("CO_POSICAO","SG_AREA","CO_ITEM","TX_GABARITO","CO_HABILIDADE","IN_ITEM_ABAN","TX_MOTIVO_ABAN","NU_PARAM_A","NU_PARAM_B","NU_PARAM_C","TX_COR","CO_PROVA","TP_LINGUA","IN_ITEM_ADAPTADO");ip<-file.path(src,"ITENS_PROVA_2023.csv");ih<-names(data.table::fread(ip,nrows=0,encoding="Latin-1"));it<-data.table::fread(ip,select=intersect(ic,ih),na.strings=c("","NA"),encoding="Latin-1");must<-c("CO_POSICAO","SG_AREA","CO_ITEM","TX_GABARITO","CO_PROVA");if(length(setdiff(must,names(it))))stop("Campos essenciais ausentes no arquivo de itens.");if(!"TP_LINGUA"%in%names(it))stop("O dicionário lista TP_LINGUA no arquivo de itens, mas o campo não consta no CSV; classificação LC não será inferida.");for(v in setdiff(ic,names(it)))it[,(v):=NA]
it[,`:=`(CO_PROVA=as.character(CO_PROVA),SG_AREA=as.character(SG_AREA),CO_ITEM=as.character(CO_ITEM),CO_POSICAO=suppressWarnings(as.integer(CO_POSICAO)),TX_GABARITO=toupper(trimws(as.character(TX_GABARITO))),TP_LINGUA=suppressWarnings(as.integer(as.character(TP_LINGUA))))]
# Respostas: 1 correta, 0 incorreta, -1 branco, -2 dupla, -3 chave X/anulada,
# -4 ausente/inválida, -5 posição 9 (item não apresentado em LC).
score<-function(r,k){r<-toupper(r);k<-toupper(k);o<-rep(-4L,length(r));o[!is.na(k)&k=="X"]<--3L;b<-!is.na(r)&r=="."&!is.na(k)&k%in%LETTERS[1:5];m<-!is.na(r)&r=="*"&!is.na(k)&k%in%LETTERS[1:5];v<-!is.na(r)&r%in%LETTERS[1:5]&!is.na(k)&k%in%LETTERS[1:5];o[b]<--1L;o[m]<--2L;o[v]<-as.integer(r[v]==k[v]);o[!is.na(r)&r=="9"]<--5L;o}
lc_keypos<-function(lang){if(is.na(lang)||!lang%in%c(0L,1L))stop("TP_LINGUA deve ser 0 (inglês) ou 1 (espanhol).");z<-6:50;z[1:5]<-if(lang==0L)1:5 else 6:10;z}
all_audit<-list();counts<-list()
for(ar in names(areas)){
  log_progress("Starting area: ", ar);
  fs<-forms[[ar]];z<-it[SG_AREA==ar&CO_PROVA%chin%fs];if(!nrow(z))stop("Sem itens da área ",ar," nos códigos padrão do dicionário.");if(anyNA(z$CO_ITEM)||anyNA(z$CO_POSICAO))stop("Identificador/posição ausente em ",ar)
  for(fm in fs){q<-z[CO_PROVA==fm];if(anyDuplicated(q$CO_ITEM))stop("CO_ITEM duplicado dentro de forma ",fm)
    if(ar=="LC"){
      if(nrow(q)!=50L||data.table::uniqueN(q$CO_POSICAO)!=45L||!setequal(unique(q$CO_POSICAO),1:45))stop("LC ",fm,": requer 50 registros, 45 posições distintas 1:45; observado ",nrow(q),"/",data.table::uniqueN(q$CO_POSICAO))
      if(any(!is.na(q$TP_LINGUA)&!(q$TP_LINGUA%in%c(0L,1L))))stop("TP_LINGUA LC fora dos valores documentados 0/1/NA.")
      ne<-sum(q$TP_LINGUA==0L,na.rm=TRUE);ns<-sum(q$TP_LINGUA==1L,na.rm=TRUE);nc<-sum(is.na(q$TP_LINGUA));pc<-q[,.(n=.N,nc=sum(is.na(TP_LINGUA)),ll=paste(sort(na.omit(TP_LINGUA)),collapse=",")),by=CO_POSICAO]
      if(ne!=5L||ns!=5L||nc!=40L||any(!((pc$n==1L&pc$nc==1L)|(pc$n==2L&pc$nc==0L&pc$ll=="0,1"))))stop("LC ",fm,": TP_LINGUA/CO_POSICAO não confirma 40 comuns e duas alternativas de cinco; conferir fonte.");  log_progress(ar, ": item bank saved.")
    }else if(nrow(q)!=45L||data.table::uniqueN(q$CO_POSICAO)!=45L||anyDuplicated(q$CO_POSICAO))stop(ar," ",fm,": esperado 45 itens/45 posições.")
  };log_progress(ar, ": item records validated.")
  if(ar=="LC")z[,ITEM_GROUP:=ifelse(is.na(TP_LINGUA),"common",ifelse(TP_LINGUA==0L,"english","spanish"))]else z[,ITEM_GROUP:="common"]
  pc<-paste0("TP_PRESENCA_",ar);fc<-paste0("CO_PROVA_",ar);rc<-paste0("TX_RESPOSTAS_",ar);gc<-paste0("TX_GABARITO_",ar);cand[[fc]]<-as.character(cand[[fc]]);ppl<-cand[get(pc)==1&get(fc)%chin%fs];log_progress(ar, ": ", nrow(ppl), " candidates selected.");data.table::setnames(ppl,fc,"CO_PROVA")
  if(ar=="LC"&&any(is.na(ppl$TP_LINGUA)|!ppl$TP_LINGUA%in%c(0,1)))stop("Candidato LC presente com TP_LINGUA ausente/inválido.")
  # Verifica gabarito individual contra uma única questão do mapa por língua/posição.
  checked<-mismatch<-0L
  
  # Verifica gabarito individual contra uma única questão do mapa por língua/posição.
  # Otimização: processa grupos de candidatos e posições em operações vetorizadas.
  checked <- 0L
  mismatch <- 0L
  
  lang_values <- if (ar == "LC") c(0L, 1L) else NA_integer_
  
  pos_offset <- switch(
    ar,
    LC = 0L,
    CH = 45L,
    CN = 90L,
    MT = 135L
  )
  
  for (fm in fs) {
    q <- z[CO_PROVA == fm]
    
    for (lang in lang_values) {
      if (ar == "LC") {
        ix <- which(ppl$CO_PROVA == fm & ppl$TP_LINGUA == lang)
      } else {
        ix <- which(ppl$CO_PROVA == fm)
      }
      
      if (!length(ix)) next
      
      # Verificação dos gabaritos dos candidatos contra o mapa de itens.
      for (pos in 1:45) {
        map_pos <- pos + pos_offset
        
        mi <- if (ar == "LC") {
          q[CO_POSICAO == map_pos &
              (is.na(TP_LINGUA) | TP_LINGUA == lang)]
        } else {
          q[CO_POSICAO == map_pos]
        }
        
        if (nrow(mi) != 1L) {
          cat("\n--- MAPPING DIAGNOSTIC ---\n")
          cat("Area:", ar, "\n")
          cat("Form:", fm, "\n")
          cat("Position:", pos, "\n")
          cat("Matching rows:", nrow(mi), "\n")
          
          cat("\nPosition counts in this form:\n")
          print(q[, .N, by = CO_POSICAO][order(CO_POSICAO)])
          
          cat("\nRecords at the requested position:\n")
          print(q[CO_POSICAO == pos])
          
          stop("Ambiguous item mapping.")
        }
        
        kp <- if (ar == "LC") lc_keypos(lang)[pos] else pos
        ik <- mi$TX_GABARITO[1]
        
        if (!is.na(ik) && nzchar(ik) && ik != "X") {
          ck <- toupper(substr(ppl[[gc]][ix], kp, kp))
          valid <- !is.na(ck) & nzchar(ck)
          
          checked <- checked + sum(valid)
          mismatch <- mismatch + sum(ck[valid] != ik)
        }
      }
    }
  }
  
  if (mismatch) {
    warning(mismatch, " de ", checked,
            " gabaritos divergentes em ", ar,
            "; verificar correspondência.")
  }
  
  # Calcula os escores das respostas.
  # A função score() e seus códigos de resposta permanecem inalterados.
  for (pos in 1:45) {
    nm <- sprintf("R%02d", pos)
    vv <- rep(-4L, nrow(ppl))
    
    for (fm in fs) {
      for (lang in lang_values) {
        if (ar == "LC") {
          ix <- which(ppl$CO_PROVA == fm & ppl$TP_LINGUA == lang)
        } else {
          ix <- which(ppl$CO_PROVA == fm)
        }
        
        if (!length(ix)) next
        
        kp <- if (ar == "LC") lc_keypos(lang)[pos] else pos
        
        vv[ix] <- score(
          substr(ppl[[rc]][ix], pos, pos),
          substr(ppl[[gc]][ix], kp, kp)
        )
      }
    }
    
    ppl[[nm]] <- vv
  }
  ;ppl[,c(rc,gc):=NULL];arrow::write_parquet(as.data.frame(ppl),file.path(out,paste0("enem_",tolower(ar),"_candidates.parquet")),compression="zstd");  log_progress(ar, ": candidate responses scored and saved.")
  ids<-unique(z$CO_ITEM);au<-data.table::rbindlist(lapply(ids,function(id){q<-z[CO_ITEM==id];pp<-as.data.frame(q[,.(NU_PARAM_A,NU_PARAM_B,NU_PARAM_C)]);pp[]<-lapply(pp,function(v)suppressWarnings(as.numeric(as.character(v))));complete<-complete.cases(pp);valid<-complete&is.finite(pp$NU_PARAM_A)&is.finite(pp$NU_PARAM_B)&is.finite(pp$NU_PARAM_C)&pp$NU_PARAM_A>0&pp$NU_PARAM_C>=0&pp$NU_PARAM_C<1;consistent<-nrow(unique(pp))==1L;st<-suppressWarnings(as.numeric(as.character(q$IN_ITEM_ABAN)));status_consistent<-length(unique(st[!is.na(st)]))<=1L&&!(anyNA(st)&&any(!is.na(st)));ann<-any(st==1,na.rm=TRUE)||any(q$TX_GABARITO=="X",na.rm=TRUE);eligible_status<-status_consistent&&length(st)>0&&all(!is.na(st)&st==0);groups<-unique(q$ITEM_GROUP);group_ok<-length(groups)==1L;reason<-if(ann)"annulled: IN_ITEM_ABAN=1 or TX_GABARITO=X"else if(!eligible_status)"status absent/inconsistent/not eligible"else if(!consistent)"A/B/C inconsistent across appearances"else if(!all(valid))"A/B/C missing or invalid"else if(!group_ok)"language group inconsistent across appearances"else"eligible";data.frame(AREA=ar,CO_ITEM=id,ITEM_GROUP=if(group_ok)groups else NA_character_,IN_ITEM_ABAN=paste(unique(q$IN_ITEM_ABAN),collapse="|"),TX_GABARITO=paste(unique(q$TX_GABARITO),collapse="|"),TX_MOTIVO_ABAN=paste(unique(na.omit(as.character(q$TX_MOTIVO_ABAN))),collapse=" | "),n_appearances=nrow(q),annulled=ann,parameter_complete=all(complete),parameter_valid=all(valid),parameter_consistent=consistent,group_consistent=group_ok,exclusion_reason=reason)}),fill=TRUE)
  eligible<-au[exclusion_reason=="eligible",CO_ITEM];z[,AREA:=ar];keep<-c("AREA","CO_PROVA","CO_POSICAO","CO_ITEM","ITEM_GROUP","TP_LINGUA","TX_GABARITO","CO_HABILIDADE","IN_ITEM_ABAN","TX_MOTIVO_ABAN","NU_PARAM_A","NU_PARAM_B","NU_PARAM_C","TX_COR","IN_ITEM_ADAPTADO");  log_progress(ar, ": item map and audit saved.");arrow::write_parquet(as.data.frame(z[,..keep]),file.path(out,paste0("enem_",tolower(ar),"_item_map.parquet")),compression="zstd");arrow::write_parquet(as.data.frame(au),file.path(out,paste0("enem_",tolower(ar),"_item_audit.parquet")),compression="zstd");all_audit[[ar]]<-au
  if(ar=="LC") {
    for(lang in c("english","spanish")) {
      grp <- c("common",lang)
      
      bank <- z[
        CO_ITEM %chin% eligible & ITEM_GROUP %chin% grp,
        .(
          item_group = ITEM_GROUP[1],
          a = as.numeric(NU_PARAM_A[1]),
          b = as.numeric(NU_PARAM_B[1]),
          c = as.numeric(NU_PARAM_C[1])
        ),
        by = CO_ITEM
      ]
      
      data.table::setnames(bank,"CO_ITEM","item_id")
      
      if(anyDuplicated(bank$item_id))
        stop("Duplicate item IDs remain in LC ",lang," bank.")
      
      arrow::write_parquet(
        as.data.frame(bank),
        file.path(out,paste0("enem_lc_",lang,"_item_bank.parquet")),
        compression="zstd"
      )
      
      counts[[paste0(ar,"_",lang)]] <- data.frame(
        AREA=ar,
        language=lang,
        initially_identified_common=length(unique(z[ITEM_GROUP=="common",CO_ITEM])),
        eligible_common=sum(bank$item_group=="common"),
        initially_identified_language=length(unique(z[ITEM_GROUP==lang,CO_ITEM])),
        eligible_language=sum(bank$item_group==lang),
        required_common=40L,
        required_language=5L,
        structure_available=sum(bank$item_group=="common")>=40L &&
          sum(bank$item_group==lang)>=5L
      )
    }
  } else {
    bank <- z[
      CO_ITEM %chin% eligible,
      .(
        item_id=CO_ITEM,
        a=as.numeric(NU_PARAM_A[1]),
        b=as.numeric(NU_PARAM_B[1]),
        c=as.numeric(NU_PARAM_C[1])
      ),
      by=CO_ITEM
    ]
    
    bank[,CO_ITEM:=NULL]
    
    arrow::write_parquet(
      as.data.frame(bank),
      file.path(out,paste0("enem_",tolower(ar),"_item_bank.parquet")),
      compression="zstd"
    )
  }
  ann<-sum(au$annulled);parmiss<-sum(au$exclusion_reason=="A/B/C missing or invalid");other<-sum(!au$exclusion_reason%in%c("eligible","annulled: IN_ITEM_ABAN=1 or TX_GABARITO=X","A/B/C missing or invalid"));counts[[ar]]<-data.frame(AREA=ar,language=NA_character_,initially_identified=nrow(au),annulled=ann,calibration_convergence_identifiable=NA_integer_,missing_or_invalid_parameters=parmiss,other_exclusions=other,eligible_unique_items=sum(au$exclusion_reason=="eligible"),reconciliation=nrow(au)-ann-parmiss-other-sum(au$exclusion_reason=="eligible"))
}
summary<-data.table::rbindlist(counts,fill=TRUE);write.csv(summary,file.path(out,"enem_item_exclusion_summary.csv"),row.names=FALSE,na="");write.csv(data.table::rbindlist(all_audit,fill=TRUE),file.path(out,"enem_item_exclusion_audit.csv"),row.names=FALSE,na="");print(summary);log_progress("All four areas completed successfully.")
