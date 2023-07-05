# This R script runs the pool, fixed-effects (within) and random-effects regressions, given an n-equations object and the output folder name
# The results are exported in the folder (given the file path in the "folder" argument) in Microsoft excel and word in *.xls and *.doc files.


panelAnalysis=function(eqs,outputFolder,data,eqsType){
  
  Datos=data
  neqs=length(eqs)
  
  # Tablas de salida:
  
  eqsTableScores=data.frame(Pool=matrix(0,neqs,1),
                            Within=matrix(0,neqs,1),
                            Random=matrix(0,neqs,1))
  llfTableScores=eqsTableScores
  aicTableScores=eqsTableScores
  bicTableScores=eqsTableScores
  hqicTableScores=eqsTableScores
  robustErrorsTable=eqsTableScores
  robustTvalsTable=eqsTableScores
  robustPvalsTable=eqsTableScores
  
  panelTests=data.frame(modelo=paste0("Model ",seq(from=1,to=neqs,by=1)),
                        fTest=matrix(0,neqs,1),
                        hTest=matrix(0,neqs,1),
                        model=matrix(0,neqs,1))
  
  modeloFinal=data.frame(coeficientes=matrix(0,neqs,1),
                         es=matrix(0,neqs,1),
                         tvals=matrix(0,neqs,1),
                         pvals=matrix(0,neqs,1),
                         llfs=matrix(0,neqs,1),
                         aics=matrix(0,neqs,1),
                         bics=matrix(0,neqs,1),
                         hqics=matrix(0,neqs,1),                         
                         model=matrix(0,neqs,1))
  
  for (a in 1:neqs){
    switch(eqsType[a],
           "bestFitting"={
             
###############################################################################    
#--- Estima regresiones tipo pool:
             
             cat("\f")
             print(paste0("Calculating pool ",a," of ",neqs," for ",folder))
             
             eval(
               parse(text=paste0("pool",a,"=plm('",eqs[a],"',data=Datos,model='pooling')"))
             )
             # Registra nombre en tabla de modelos:
             eqsTableScores$Pool[a]=paste0("pool",a)
             
             # Registra el LLF:  
             eval(
               parse(text=paste0("llfTableScores$Pool[a]=logLik.plm(pool",a,")[1]"))
             )
             
             # Registra Akaike:
             eval(
               parse(text=paste0("aicTableScores$Pool[a]=AIC.plm(pool",a,")[1]"))
             )
             
             # Registra BIC:
             eval(
               parse(text=paste0("bicTableScores$Pool[a]=BIC.plm(pool",a,")[1]"))
             )  
             
             # Registra HQIC:
             eval(
               parse(text=paste0("hqicTableScores$Pool[a]=HQIC.plm(pool",a,")[1]"))
             )               
             
             # Realiza estimación robusta de errores estándar:
             eval(
               parse(
                 text=paste0("tbl = coeftest(pool",a, 
                             ",vcov=vcovNW(pool",a,",cluster='group'))")
               )
             )
             
             robustErrorsTable$Pool[a]=paste0("c(",paste(round(tbl[,2],4),collapse=","),")")
             
             robustTvalsTable$Pool[a]=paste0("c(",paste(round(tbl[,3],4),collapse=","),")")
             
             robustPvalsTable$Pool[a]=paste0("c(",paste(round(tbl[,4],4),collapse=","),")")
             
###############################################################################    
#--- Estima regresiones tipo efectos fijos "within":
             
             cat("\f")
             print(paste0("Calculating fixed effects ",a," of ",neqs," for ",folder))
             
             eval(
               parse(text=paste0("fixedEf",a,"=plm('",eqs[a],"',data=Datos,model='within')"))
             )
             # Registra nombre en tabla de modelos:
             eqsTableScores$Within[a]=paste0("fixedEf",a)
             
             # Registra el LLF:  
             eval(
               parse(text=paste0("llfTableScores$Within[a]=logLik.plm(fixedEf",a,")[1]"))
             )
             
             # Registra Akaike:
             eval(
               parse(text=paste0("aicTableScores$Within[a]=AIC.plm(fixedEf",a,")[1]"))
             )
             
             # Registra BIC:
             eval(
               parse(text=paste0("bicTableScores$Within[a]=BIC.plm(fixedEf",a,")[1]"))
             )
             
             # Registra HQIC:
             eval(
               parse(text=paste0("hqicTableScores$Within[a]=HQIC.plm(fixedEf",a,")[1]"))
             )             
             
             # Realiza estimación robusta de errores estándar:
             eval(
               parse(
                 text=paste0("tbl = coeftest(fixedEf",a, 
                             ",vcov=vcovNW(fixedEf",a,",cluster='group'))")
               )
             )
             
             robustErrorsTable$Within[a]=paste0("c(",paste(round(tbl[,2],4),collapse=","),")")
             
             robustTvalsTable$Within[a]=paste0("c(",paste(round(tbl[,3],4),collapse=","),")")
             
             robustPvalsTable$Within[a]=paste0("c(",paste(round(tbl[,4],4),collapse=","),")")
             
             # REaliza prueba F par COMPARAR:
             eval(
               parse(
                 text=paste0("f.test=round(pFtest(fixedEf",a,",pool",a,")$p.value,4)")
               )  
             )
             
             # Realiza prueba F:
             eval(
               parse(
                 text=paste0("panelTests$fTest[a]=round(pFtest(fixedEf",a,",pool",a,")$p.value,4)")
               )
             )
             
             if (f.test<0.05){
               panelTests$model[a]="Fixed effects within (best fitting)"
             } else {
               panelTests$model[a]="Pooled regression (best fitting)"
             }
             
###############################################################################    
             
#--- Estima regresiones tipo efectos aleatorios:
             
             cat("\f")
             print(paste0("Calculating random effects ",a," of ",neqs," for ",folder))
             
             eval(
               eval(
               parse(text=paste0("randomEf",a,"=tryCatch(plm('",eqs[a],"',data=Datos,model='random',random.method='swar'), error=function(e) e=NA)"))
               )
               )
             
             eval(
               parse(text=paste0("continueRandomEf=is.na(randomEf",a,")"))
             )
             
             if (continueRandomEf[1]){
              
# Random effects is na then fixed effects within:
              
               
               # Registra nombre en tabla de modelos:
               eqsTableScores$Random[a]=paste0("fixedEf",a)
               
               # Registra el LLF:  
               eval(
                 parse(text=paste0("llfTableScores$Random[a]=logLik.plm(fixedEf",a,")[1]"))
               )
               
               # Registra Akaike:
               eval(
                 parse(text=paste0("aicTableScores$Random[a]=AIC.plm(fixedEf",a,")[1]"))
               )
               
               # Registra BIC:
               eval(
                 parse(text=paste0("bicTableScores$Random[a]=BIC.plm(fixedEf",a,")[1]"))
               ) 
               
               # Registra HQIC:
               eval(
                 parse(text=paste0("hqicTableScores$Random[a]=HQIC.plm(fixedEf",a,")[1]"))
               )                
               
               # Realiza estimación robusta de errores estándar:
               eval(
                 parse(
                   text=paste0("tbl = coeftest(fixedEf",a, 
                               ",vcov=vcovNW(fixedEf",a,",cluster='group'))")
                 )
               )
               
               robustErrorsTable$Random[a]=paste0("c(",paste(round(tbl[,2],4),collapse=","),")")
               
               robustTvalsTable$Random[a]=paste0("c(",paste(round(tbl[,3],4),collapse=","),")")
               
               robustPvalsTable$Random[a]=paste0("c(",paste(round(tbl[,4],4),collapse=","),")")
               
               # REaliza prueba de Hausman par COMPARAR:
               eval(
                 parse(
                   text=paste0("hausman.test=NA")
                 )  
               )
               
               # Realiza prueba Hausman y registra en tabla:
               eval(
                 parse(
                   text=paste0("panelTests$hTest[a]='Not feasible'")
                 )
               )
               
               # Selección final para el análisis:
               if (f.test<0.05){
                 modelId=2
                 panelTests$model[a]="Fixed effects within (best fitting)" 
                 modeloFinal$model[a]="Fixed eff."
               } else {
                 modelId=1 
                 panelTests$model[a]="Pooled regression (best fitting)"  
                 modeloFinal$model[a]="Pool"
               }
               
               
               # Guarda el mejor modelo:
               modeloFinal$coeficientes[a]=eqsTableScores[a,modelId]
               modeloFinal$es[a]=robustErrorsTable[a,modelId]
               modeloFinal$tvals[a]=robustTvalsTable[a,modelId]
               modeloFinal$pvals[a]=robustPvalsTable[a,modelId]
               modeloFinal$llfs[a]=llfTableScores[a,modelId]
               modeloFinal$aics[a]=aicTableScores[a,modelId]                           
               modeloFinal$bics[a]=bicTableScores[a,modelId]
               modeloFinal$hqics[a]=hqicTableScores[a,modelId]
                                                  
             } else {
# Random effects is not na:
             
               # Registra nombre en tabla de modelos:
               eqsTableScores$Random[a]=paste0("randomEf",a)
               
               # Registra el LLF:  
               eval(
                 parse(text=paste0("llfTableScores$Random[a]=logLik.plm(randomEf",a,")[1]"))
               )
               
               # Registra Akaike:
               eval(
                 parse(text=paste0("aicTableScores$Random[a]=AIC.plm(randomEf",a,")[1]"))
               )
               
               # Registra BIC:
               eval(
                 parse(text=paste0("bicTableScores$Random[a]=BIC.plm(randomEf",a,")[1]"))
               ) 
               
               # Registra HQIC:
               eval(
                 parse(text=paste0("hqicTableScores$Random[a]=HQIC.plm(randomEf",a,")[1]"))
               )                
               
               # Realiza estimación robusta de errores estándar:
               eval(
                 parse(
                   text=paste0("tbl = coeftest(randomEf",a, 
                               ",vcov=vcovNW(randomEf",a,",cluster='group'))")
                 )
               )
               
               robustErrorsTable$Random[a]=paste0("c(",paste(round(tbl[,2],4),collapse=","),")")
               
               robustTvalsTable$Random[a]=paste0("c(",paste(round(tbl[,3],4),collapse=","),")")
               
               robustPvalsTable$Random[a]=paste0("c(",paste(round(tbl[,4],4),collapse=","),")")
               
               # REaliza prueba de Hausman par COMPARAR:
               eval(
                 parse(
                   text=paste0("hausman.test=round(phtest(fixedEf",a,",randomEf",a,")$p.value,4)")
                 )  
               )
               
               # Realiza prueba Hausman y registra en tabla:
               eval(
                 parse(
                   text=paste0("panelTests$hTest[a]=round(phtest(fixedEf",a,",randomEf",a,")$p.value,4)")
                 )
               )
               
               # Selección final para el análisis:
               if (f.test<0.05){
                 if (hausman.test>0.05){
                   modelId=3
                   panelTests$model[a]="Random effects within (best fitting)" 
                   modeloFinal$model[a]="Random eff."
                 } else {
                   modelId=2
                   panelTests$model[a]="Fixed effects within (best fitting)" 
                   modeloFinal$model[a]="Fixed eff."
                 }
               } else {
                 modelId=1  
                 panelTests$model[a]="Pooled regression (best fitting)" 
                 modeloFinal$model[a]="Pool"
               }
               
 
               
               # Guarda el mejor modelo:
               modeloFinal$coeficientes[a]=eqsTableScores[a,modelId]
               modeloFinal$es[a]=robustErrorsTable[a,modelId]
               modeloFinal$tvals[a]=robustTvalsTable[a,modelId]
               modeloFinal$pvals[a]=robustPvalsTable[a,modelId]
               modeloFinal$llfs[a]=llfTableScores[a,modelId]
               modeloFinal$aics[a]=aicTableScores[a,modelId]
               modeloFinal$bics[a]=bicTableScores[a,modelId]
               modeloFinal$hqics[a]=hqicTableScores[a,modelId]               
               
 # end else if NA model              
             }
             
             
           },
           
           "poolRegression"={

             ###############################################################################    
             #--- Estima regresiones tipo pool:
             
             cat("\f")
             print(paste0("Calculating pool ",a," of ",neqs," for ",folder))
             
             eval(
               parse(text=paste0("pool",a,"=plm('",eqs[a],"',data=Datos,model='pooling')"))
             )
             # Registra nombre en tabla de modelos:
             eqsTableScores$Pool[a]=paste0("pool",a)
             
             # Registra el LLF:  
             eval(
               parse(text=paste0("llfTableScores$Pool[a]=logLik.plm(pool",a,")[1]"))
             )
             
             # Registra Akaike:
             eval(
               parse(text=paste0("aicTableScores$Pool[a]=AIC.plm(pool",a,")[1]"))
             )
             
             # Registra BIC:
             eval(
               parse(text=paste0("bicTableScores$Pool[a]=BIC.plm(pool",a,")[1]"))
             ) 
             
             # Registra HQIC:
             eval(
               parse(text=paste0("hqicTableScores$Pool[a]=HQIC.plm(pool",a,")[1]"))
             )              
             
             # Realiza estimación robusta de errores estándar:
             eval(
               parse(
                 text=paste0("tbl = coeftest(pool",a, 
                             ",vcov=vcovNW(pool",a,",cluster='group'))")
               )
             )
             
             robustErrorsTable$Pool[a]=paste0("c(",paste(round(tbl[,2],4),collapse=","),")")
             
             robustTvalsTable$Pool[a]=paste0("c(",paste(round(tbl[,3],4),collapse=","),")")
             
             robustPvalsTable$Pool[a]=paste0("c(",paste(round(tbl[,4],4),collapse=","),")")
 
             # REaliza prueba de Hausman par COMPARAR:
             
             hausman.test="pool regression selected"
             # Realiza prueba Hausman y registra en tabla:
             
             panelTests$hTest[a]="pool regression selected"
             panelTests$model[a]="pool regression selected"
             
             
             # Guarda el mejor modelo:
             modeloFinal$coeficientes[a]=eqsTableScores[a,1]
             modeloFinal$es[a]=robustErrorsTable[a,1]
             modeloFinal$tvals[a]=robustTvalsTable[a,1]
             modeloFinal$pvals[a]=robustPvalsTable[a,1]
             modeloFinal$llfs[a]=llfTableScores[a,1]
             modeloFinal$aics[a]=aicTableScores[a,1] 
             modeloFinal$bics[a]=bicTableScores[a,1]
             modeloFinal$hqics[a]=hqicTableScores[a,1]             
             modeloFinal$model[a]="Pool"
           },
           
           "fixedEffects"={
   
             ###############################################################################    
             #--- Estima regresiones tipo efectos fijos "within":
             
             cat("\f")
             print(paste0("Calculating fixed effects ",a," of ",neqs," for ",folder))
             
             eval(
               parse(text=paste0("fixedEf",a,"=plm('",eqs[a],"',data=Datos,model='within')"))
             )
             # Registra nombre en tabla de modelos:
             eqsTableScores$Within[a]=paste0("fixedEf",a)
             
             # Registra el LLF:  
             eval(
               parse(text=paste0("llfTableScores$Within[a]=logLik.plm(fixedEf",a,")[1]"))
             )
             
             # Registra Akaike:
             eval(
               parse(text=paste0("aicTableScores$Within[a]=AIC.plm(fixedEf",a,")[1]"))
             )
             
             # Registra BIC:
             eval(
               parse(text=paste0("bicTableScores$Within[a]=BIC.plm(fixedEf",a,")[1]"))
             )             
             
             # Registra HQIC:
             eval(
               parse(text=paste0("hqicTableScores$Within[a]=HQIC.plm(fixedEf",a,")[1]"))
             )     
             
             # Realiza estimación robusta de errores estándar:
             eval(
               parse(
                 text=paste0("tbl = coeftest(fixedEf",a, 
                             ",vcov=vcovNW(fixedEf",a,",cluster='group'))")
               )
             )
             
             robustErrorsTable$Within[a]=paste0("c(",paste(round(tbl[,2],4),collapse=","),")")
             
             robustTvalsTable$Within[a]=paste0("c(",paste(round(tbl[,3],4),collapse=","),")")
             
             robustPvalsTable$Within[a]=paste0("c(",paste(round(tbl[,4],4),collapse=","),")")

             # REaliza prueba de Hausman par COMPARAR:
             
             hausman.test="fixed-effects (within) selected"
             # Realiza prueba Hausman y registra en tabla:
             
             panelTests$hTest[a]="fixed-effects (within) selected"
             panelTests$model[a]="fixed-effects (within) selected"
             
             # Guarda el mejor modelo:
             modeloFinal$coeficientes[a]=eqsTableScores[a,2]
             modeloFinal$es[a]=robustErrorsTable[a,2]
             modeloFinal$tvals[a]=robustTvalsTable[a,2]
             modeloFinal$pvals[a]=robustPvalsTable[a,2]
             modeloFinal$llfs[a]=llfTableScores[a,2]
             modeloFinal$aics[a]=aicTableScores[a,2] 
             modeloFinal$bics[a]=bicTableScores[a,2]
             modeloFinal$hqics[a]=hqicTableScores[a,2]             
             modeloFinal$model[a]="Fixed eff."
             
           },
           
           "randomEffects"={

             ###############################################################################    
             
             #--- Estima regresiones tipo efectos aleatorios:
             
             cat("\f")
             print(paste0("Calculating random effects ",a," of ",neqs," for ",folder))
             
             eval(
               parse(text=paste0("randomEf",a,"=plm('",eqs[a],"',data=Datos,model='random',random.method='swar')"))
             )
             # Registra nombre en tabla de modelos:
             eqsTableScores$Random[a]=paste0("randomEf",a)
             
             # Registra el LLF:  
             eval(
               parse(text=paste0("llfTableScores$Random[a]=logLik.plm(randomEf",a,")[1]"))
             )
             
             # Registra Akaike:
             eval(
               parse(text=paste0("aicTableScores$Random[a]=AIC.plm(randomEf",a,")[1]"))
             )
             
             # Registra BIC:
             eval(
               parse(text=paste0("bicTableScores$Random[a]=BIC.plm(randomEf",a,")[1]"))
             ) 
             
             # Registra HQIC:
             eval(
               parse(text=paste0("hqicTableScores$Random[a]=HQIC.plm(randomEf",a,")[1]"))
             )              
             
             # Realiza estimación robusta de errores estándar:
             eval(
               parse(
                 text=paste0("tbl = coeftest(randomEf",a, 
                             ",vcov=vcovNW(randomEf",a,",cluster='group'))")
               )
             )
             
             robustErrorsTable$Random[a]=paste0("c(",paste(round(tbl[,2],4),collapse=","),")")
             
             robustTvalsTable$Random[a]=paste0("c(",paste(round(tbl[,3],4),collapse=","),")")
             
             robustPvalsTable$Random[a]=paste0("c(",paste(round(tbl[,4],4),collapse=","),")")
             
             # REaliza prueba de Hausman par COMPARAR:
             
             hausman.test="random effects selected"
             # Realiza prueba Hausman y registra en tabla:
            
             panelTests$hTest[a]="random effects selected"
             panelTests$model[a]="random effects selected"
             
             # Guarda el mejor modelo:
             modeloFinal$coeficientes[a]=eqsTableScores[a,3]
             modeloFinal$es[a]=robustErrorsTable[a,3]
             modeloFinal$tvals[a]=robustTvalsTable[a,3]
             modeloFinal$pvals[a]=robustPvalsTable[a,3]
             modeloFinal$llfs[a]=llfTableScores[a,3]
             modeloFinal$aics[a]=aicTableScores[a,3]
             modeloFinal$bics[a]=bicTableScores[a,3]
             modeloFinal$hqics[a]=hqicTableScores[a,3]             
             modeloFinal$model[a]="Random eff."
             
           }
           )

################################################################################    
# Termina el loop de ecuaciones:        
    }
  
  # Exporta resultados a excel y word:
  if (exists(folder)==FALSE) {
    subdir1=dir.create(folder)
  }
  
  # 
  
  modelVector=paste0(paste("'",modeloFinal$model,sep=""),"'",collapse=",")
  
  
  eval(
    parse(
      text=paste0(
        "stargazer(",paste0(modeloFinal$coeficientes,collapse=","),
        ",type='html',",
        "se=list(",paste0(modeloFinal$es,collapse=","),"),",
        "t=list(",paste0(modeloFinal$es,collapse=","),"),",
        "p=list(",paste0(modeloFinal$pvals,collapse=","),"),",
        
        "add.lines=list(",
        "c('Model',",
        modelVector,
        "),",        
        "c('LLF',",
        paste0(round(modeloFinal$llfs,digits=4),collapse=","),
        "),",
        "c('AIC',",
        paste0(round(modeloFinal$aics,digits=4),collapse=","),
        ")",
        
        "),",
        "report='vc*',no.space=TRUE,digits=4,",
        "out=c('",
        paste0(folder,"/panelFinalReg.doc'"),",'",
        paste0(folder,"/panelFinalReg.xls'"),
        ")",
        ")"
      )
    )
  )
  
  # Exporta pruebas F y de Hausman:
  colnames(panelTests)=c("Regression","f-test","Hausman test","Best fitting model")
  
  eval(parse(text=paste0("stargazer(panelTests,summary=FALSE,type='html',rownames = FALSE,
  title='F and Hausman test summary table',
          out=paste0(","c('",
                         paste0(folder,"/pruebasEfectosReg.doc'"),",'",
                         paste0(folder,"/pruebasEfectosReg.xls'"),
                         ")))")))
  
  # Exporta tabla de pValues:
  eval(parse(text=paste0("stargazer(robustPvalsTable,summary=FALSE,type='html',rownames = FALSE,
  title='Pvalues',
          out=paste0(","c('",
                         paste0(folder,"/pValuesregs.doc'"),",'",
                         paste0(folder,"/pValuesRegs.xls'"),
                         ")))")))
  # Generates a list objetc with the results:
  objtecOut=list(
    pValues=robustPvalsTable,
    panelTable=modeloFinal
  )
  
  cat("\f")
  print(paste0("The panel regression analisys is done for the ",neqs," equations of interest..."))
  
  return(objtecOut)
}

logLik.plm <- function(object){
  out <- -length(object$residuals) * log(2 * var(object$residuals) * pi)/2 - deviance(object)/(2 * var(object$residuals))
  
  attr(out,"df") <- length(object$residuals) - object$df.residual
  attr(out,"nobs") <- length(object$residuals)
  return(out)
}

AIC.plm <- function(object){
  llf <- -length(object$residuals) * log(2 * var(object$residuals) * pi)/2 - deviance(object)/(2 * var(object$residuals))
  k.plm=object$df.residual
  n.plm=length(object$coefficients)
  hq.plm=(2*k.plm)-(2*llf)
  return(hq.plm)
}

BIC.plm <- function(object){
  llf <- -length(object$residuals) * log(2 * var(object$residuals) * pi)/2 - deviance(object)/(2 * var(object$residuals))
  k.plm=object$df.residual
  n.plm=length(object$coefficients)
  hq.plm=(k.plm*log(n.plm))-(2*llf)
  return(hq.plm)
}

HQIC.plm <- function(object){
  llf <- -length(object$residuals) * log(2 * var(object$residuals) * pi)/2 - deviance(object)/(2 * var(object$residuals))
  k.plm=object$df.residual
  n.plm=length(object$coefficients)
  hq.plm=-(2*llf)+((2*k.plm)*log(log(n.plm)))
  return(hq.plm)
}
