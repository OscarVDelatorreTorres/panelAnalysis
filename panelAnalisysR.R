# This R script runs the pool, fixed-effects (within) and random-effects regressions, given an n-equations object and the output folder name
# The results are exported in the folder (given the file path in the "folder" argument) in Microsoft excel and word in *.xls and *.doc files.


panelAnalisys=function(eqs,outputFolder,data,eqsType){
  
  Datos=data
  neqs=length(eqs)
  
  # Tablas de salida:
  
  eqsTableScores=data.frame(Pool=matrix(0,neqs,1),
                            Within=matrix(0,neqs,1),
                            Random=matrix(0,neqs,1))
  llfTableScores=eqsTableScores
  aicTableScores=eqsTableScores
  robustErrorsTable=eqsTableScores
  robustTvalsTable=eqsTableScores
  robustPvalsTable=eqsTableScores
  
  panelTests=data.frame(modelo=paste0("Modelo",seq(from=1,to=neqs,by=1)),
                        fTest=matrix(0,neqs,1),
                        hTest=matrix(0,neqs,1))
  
  modeloFinal=data.frame(coeficientes=matrix(0,neqs,1),
                         es=matrix(0,neqs,1),
                         tvals=matrix(0,neqs,1),
                         pvals=matrix(0,neqs,1),
                         llfs=matrix(0,neqs,1),
                         aics=matrix(0,neqs,1))
  
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
               } else {
                 modelId=2
               }
             } else {
               modelId=1  
             }
             # Guarda el mejor modelo:
             modeloFinal$coeficientes[a]=eqsTableScores[a,modelId]
             modeloFinal$es[a]=robustErrorsTable[a,modelId]
             modeloFinal$tvals[a]=robustTvalsTable[a,modelId]
             modeloFinal$pvals[a]=robustPvalsTable[a,modelId]
             modeloFinal$llfs[a]=llfTableScores[a,modelId]
             modeloFinal$aics[a]=aicTableScores[a,modelId]
             
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
             
             # Guarda el mejor modelo:
             modeloFinal$coeficientes[a]=eqsTableScores[a,1]
             modeloFinal$es[a]=robustErrorsTable[a,1]
             modeloFinal$tvals[a]=robustTvalsTable[a,1]
             modeloFinal$pvals[a]=robustPvalsTable[a,1]
             modeloFinal$llfs[a]=llfTableScores[a,1]
             modeloFinal$aics[a]=aicTableScores[a,1]                         
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
             
             # Guarda el mejor modelo:
             modeloFinal$coeficientes[a]=eqsTableScores[a,2]
             modeloFinal$es[a]=robustErrorsTable[a,2]
             modeloFinal$tvals[a]=robustTvalsTable[a,2]
             modeloFinal$pvals[a]=robustPvalsTable[a,2]
             modeloFinal$llfs[a]=llfTableScores[a,2]
             modeloFinal$aics[a]=aicTableScores[a,2]  
             
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
             
             # Guarda el mejor modelo:
             modeloFinal$coeficientes[a]=eqsTableScores[a,3]
             modeloFinal$es[a]=robustErrorsTable[a,3]
             modeloFinal$tvals[a]=robustTvalsTable[a,3]
             modeloFinal$pvals[a]=robustPvalsTable[a,3]
             modeloFinal$llfs[a]=llfTableScores[a,3]
             modeloFinal$aics[a]=aicTableScores[a,3]
             
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
  eval(
    parse(
      text=paste0(
        "stargazer(",paste0(modeloFinal$coeficientes,collapse=","),
        ",type='html',",
        "se=list(",paste0(modeloFinal$es,collapse=","),"),",
        "t=list(",paste0(modeloFinal$es,collapse=","),"),",
        "p=list(",paste0(modeloFinal$pvals,collapse=","),"),",
        
        "add.lines=list(",
        
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
  colnames(panelTests)=c("Regresión","Prueba F","Prueba de Hausman")
  eval(parse(text=paste0("stargazer(panelTests,summary=FALSE,type='html',rownames = FALSE,title='Pruebas F y de Hausman para las regresiones',
          out=paste0(","c('",
                         paste0(folder,"/pruebasEfectosReg.doc'"),",'",
                         paste0(folder,"/pruebasEfectosReg.xls'"),
                         ")))")))
  
  # Exporta tabla de pValues:
  eval(parse(text=paste0("stargazer(robustPvalsTable,summary=FALSE,type='html',rownames = FALSE,title='Valores p de las regresiones',
          out=paste0(","c('",
                         paste0(folder,"/pValuesregs.doc'"),",'",
                         paste0(folder,"/pValuesRegs.xls'"),
                         ")))")))
  
  cat("\f")
  print(paste0("The panel regression analisys is done for the ",neqs," equations of interest..."))
}

