##################################################################################################################################
# SCRIPT DE ORGANIZACAO DOS DADOS ###############################################################################################
##################################################################################################################################

    library(quantmod)
    library(imputeTS)

    ### ################################################
    ### 1. LEITURA DOS DADOS - PRECOS DE FECHAMENTO
    ### ################################################
    load("dados/CAC.RData")
    load("dados/DAX.RData")
    load("dados/FTSE.RData")
    load("dados/NIKKEI.RData")
    load("dados/NYSE.RData")
    load("dados/TSX.RData")
    
    ### ################################################
    ### 2. GRAFICOS - PRECOS DE FECHAMENTO, LOG-RETORNOS
    ### E SELECAO DE JANELA
    ### ################################################
    
    # Janelas 'Completas'
    # (mais abaixo, redefino as séries,
    # selecionando janela - para voltar às 
    # originais, execute código desde o início)
    par(mfrow=c(2,1))
    plot(CAC[,"CAC"]); plot(CAC[,"cac"])
    plot(DAX[,"DAX"]); plot(DAX[,"dax"])
    plot(FTSE[,"FTSE"]); plot(FTSE[,"ftse"])
    plot(NIKKEI[,"NIKKEI"]); plot(NIKKEI[,"nikkei"])
    plot(NYSE[,"NYSE"]); plot(NYSE[,"nyse"])
    plot(TSX[,"TSX"]); plot(TSX[,"tsx"])
    par(mfrow=c(1,1))

    # Selecionando janela de trabalho
    BegSample = '2004-01-01'
    EndSample = '2009-12-31'
    CAC = window(CAC,start=BegSample,end=EndSample)
    DAX = window(DAX,start=BegSample,end=EndSample)
    FTSE = window(FTSE,start=BegSample,end=EndSample)
    NIKKEI = window(NIKKEI,start=BegSample,end=EndSample)
    NYSE = window(NYSE,start=BegSample,end=EndSample)
    TSX = window(TSX,start=BegSample,end=EndSample)
    par(mfrow=c(1,1))

    ### ################################################
    ### 3. DATAS
    ### ################################################
    
    # 3.a Deflagra crise
    # https://fraser.stlouisfed.org/timeline/financial-crisis#1
    BegCrisis = as.Date("2007-02-27")
    # (OU) 
    BegCrisis = as.Date("2007-07-01") 
    # (a) February 27, 2007 | Freddie Mac Press Release
    # The Federal Home Loan Mortgage Corporation (Freddie Mac) announces that 
    # it will no longer buy the most risky subprime mortgages and 
    # mortgage-related securities.
    # Freddie Mac Announces Tougher Subprime Lending Standards to Help Reduce 
    # the Risk of Future Borrower Default: Company Also to Develop Model 
    # Subprime Mortgages
    # https://fraser.stlouisfed.org/title/freddie-mac-5132/freddie-mac-announces-tougher-subprime-lending-standards-help-reduce-risk-future-borrower-default-518857
    # (b) July 27, 2007 | Freddie Mac Press Release - início "sugerido pelos dados"
    
    # 3.b Restrições
    CAC.RES.BEG = as.Date("2008-09-22")
    CAC.RES.END = as.Date("2012-12-11")
    DAX.RES.BEG = as.Date("2008-09-20")
    DAX.RES.END = as.Date("2011-03-31")  
    FTSE.RES.BEG = as.Date("2008-09-19")
    FTSE.RES.END = as.Date("2009-01-19")  
    NIKKEI.RES.BEG = as.Date("2008-10-20")
    NIKKEI.RES.END = as.Date("2010-07-31")  
    NYSE.RES.BEG = as.Date("2008-09-19")
    NYSE.RES.END = as.Date("2008-10-08")  
    TSX.RES.BEG = as.Date("2008-09-19")
    TSX.RES.END = as.Date("2008-10-08")  
    
    ### ######################################################
    ### 4. GRAFICOS (destacando periodos de crise, restricoes 
    ### ######################################################
    
    Col0 = c("gray50","gray99")[1]
    
    # 4.a CAC
    serie = window(CAC[,"cac"], start=CAC.RES.BEG)
    shade = cbind(upper = rep(1,length(serie)), lower = rep(-1, length(serie)))
    shade = merge(serie*0+1,serie*0-1)
    names(shade)=c("upper","lower")
    WindowShade = window(shade,start=CAC.RES.BEG,end=CAC.RES.END)
    length(window(CAC[,"cac"], start=CAC.RES.BEG))
    BeforeCrisis = length(window(CAC[,"cac"], end=BegCrisis-1))
    BeforeRestriction = length(window(CAC[,"cac"], start=BegCrisis, end=CAC.RES.BEG-1))
    AfterRestriction = length(window(CAC[,"cac"], start=CAC.RES.BEG))
    plot(CAC[,"cac"],main=paste0("cac: "
        ,"Win1:",BeforeCrisis,", Win2:",BeforeRestriction, ", Win3:",AfterRestriction, " - "  
       ,"restrictions were active until the end of the sample"))
    print(addPolygon(WindowShade, col = "gray90", on = -1))
    addEventLines(xts("50th TD day after the start of the bans",index(serie[50]))
                  ,col=Col0,lwd=10,lty=3,srt=90, pos=2,cex=.85)
    addEventLines(xts("50th TD day after the start of the bans",BegCrisis)
                  ,col=c("gray50"),lwd=10,lty=3,srt=90, pos=2,cex=.85)
    
    # 4.b DAX
    serie = window(DAX[,"dax"], start=DAX.RES.BEG)
    shade = cbind(upper = rep(1,length(serie)), lower = rep(-1, length(serie)))
    shade = merge(serie*0+1,serie*0-1)
    names(shade)=c("upper","lower")
    WindowShade = window(shade,start=DAX.RES.BEG,end=DAX.RES.END)
    length(window(DAX[,"dax"], start=DAX.RES.BEG))
    BeforeCrisis = length(window(DAX[,"dax"], end=BegCrisis-1))
    BeforeRestriction = length(window(DAX[,"dax"], start=BegCrisis, end=DAX.RES.BEG-1))
    AfterRestriction = length(window(DAX[,"dax"], start=DAX.RES.BEG))
    plot(DAX[,"dax"],main=paste0("dax: "
                                 ,"Win1:",BeforeCrisis,", Win2:",BeforeRestriction, ", Win3:",AfterRestriction, " - "  
                           ,"restrictions were active until the end of the sample"))
    print(addPolygon(WindowShade, col = "gray90", on = -1))
    addEventLines(xts("50th TD day after the start of the bans",index(serie[50]))
                  ,col=Col0,lwd=10,lty=3,srt=90, pos=2,cex=.85)
    addEventLines(xts("50th TD day after the start of the bans",BegCrisis)
                  ,col=c("gray50"),lwd=10,lty=3,srt=90, pos=2,cex=.85)
    
    # 4.c FTSE
    serie = window(FTSE[,"ftse"], start=FTSE.RES.BEG)
    shade = cbind(upper = rep(1,length(serie)), lower = rep(-1, length(serie)))
    shade = merge(serie*0+1,serie*0-1)
    names(shade)=c("upper","lower")
    WindowShade = window(shade,start=FTSE.RES.BEG,end=FTSE.RES.END)
    length(window(FTSE[,"ftse"], start=FTSE.RES.BEG))
    BeforeCrisis = length(window(FTSE[,"ftse"], end=BegCrisis-1))
    BeforeRestriction = length(window(FTSE[,"ftse"], start=BegCrisis, end=FTSE.RES.BEG-1))
    AfterRestriction = length(window(FTSE[,"ftse"], start=FTSE.RES.BEG))
    plot(FTSE[,"ftse"],main=paste0("ftse: "
                    ,"Win1:",BeforeCrisis,", Win2:",BeforeRestriction, ", Win3:",AfterRestriction, " - "  
    , "restrictions remained active for ",nrow(WindowShade), " trading days"))
    print(addPolygon(WindowShade, col = "gray90", on = -1))
    addEventLines(xts("50th TD day after the start of the bans",index(serie[50]))
                  ,col=Col0,lwd=10,lty=3,srt=90, pos=2,cex=.85)
    addEventLines(xts("50th TD day after the start of the bans",BegCrisis)
                  ,col=c("gray50"),lwd=10,lty=3,srt=90, pos=2,cex=.85)
    
    # 4.d NIKKEI    
    serie = window(NIKKEI[,"nikkei"], start=NIKKEI.RES.BEG)
    shade = cbind(upper = rep(1,length(serie)), lower = rep(-1, length(serie)))
    shade = merge(serie*0+1,serie*0-1)
    names(shade)=c("upper","lower")
    WindowShade = window(shade,start=NIKKEI.RES.BEG,end=NIKKEI.RES.END)
    length(window(NIKKEI[,"nikkei"], start=NIKKEI.RES.BEG))
    BeforeCrisis = length(window(NIKKEI[,"nikkei"], end=BegCrisis-1))
    BeforeRestriction = length(window(NIKKEI[,"nikkei"], start=BegCrisis, end=NIKKEI.RES.BEG-1))
    AfterRestriction = length(window(NIKKEI[,"nikkei"], start=NIKKEI.RES.BEG))
    plot(NIKKEI[,"nikkei"],main=paste0("nikkei: "
                                   ,"Win1:",BeforeCrisis,", Win2:",BeforeRestriction, ", Win3:",AfterRestriction, " - "                                         
                           ,"restrictions were active until the end of the sample"))
    print(addPolygon(WindowShade, col = "gray90", on = -1))
    addEventLines(xts("50th TD day after the start of the bans",index(serie[50]))
                  ,col=Col0,lwd=10,lty=3,srt=90, pos=2,cex=.85)
    addEventLines(xts("50th TD day after the start of the bans",BegCrisis)
                  ,col=c("gray50"),lwd=10,lty=3,srt=90, pos=2,cex=.85)
    
    # 4.5 NYSE
    serie = window(NYSE[,"nyse"], start=NYSE.RES.BEG)
    shade = cbind(upper = rep(1,length(serie)), lower = rep(-1, length(serie)))
    shade = merge(serie*0+1,serie*0-1)
    names(shade)=c("upper","lower")
    WindowShade = window(shade,start=NYSE.RES.BEG,end=NYSE.RES.END)
    length(window(NYSE[,"nyse"], start=NYSE.RES.BEG))
    BeforeCrisis = length(window(NYSE[,"nyse"], end=BegCrisis-1))
    BeforeRestriction = length(window(NYSE[,"nyse"], start=BegCrisis, end=NYSE.RES.BEG-1))
    AfterRestriction = length(window(NYSE[,"nyse"], start=NYSE.RES.BEG))
    plot(NYSE[,"nyse"],main=paste0("nyse: "
           ,"Win1:",BeforeCrisis,", Win2:",BeforeRestriction, ", Win3:",AfterRestriction, " - "                                         
                           , "restrictions remained active for ",nrow(WindowShade), " trading days"))
    print(addPolygon(WindowShade, col = "gray90", on = -1))
    addEventLines(xts("50th TD day after the start of the bans",index(serie[50]))
                  ,col=Col0,lwd=10,lty=3,srt=90, pos=2,cex=.85)
    addEventLines(xts("50th TD day after the start of the bans",BegCrisis)
                  ,col=c("gray50"),lwd=10,lty=3,srt=90, pos=2,cex=.85)

    # 4.6 TSX
    serie = window(TSX[,"tsx"], start=TSX.RES.BEG)
    shade = cbind(upper = rep(1,length(serie)), lower = rep(-1, length(serie)))
    shade = merge(serie*0+1,serie*0-1)
    names(shade)=c("upper","lower")
    WindowShade = window(shade,start=TSX.RES.BEG,end=TSX.RES.END)
    BeforeCrisis = length(window(TSX[,"tsx"], end=BegCrisis-1))
    BeforeRestriction = length(window(TSX[,"tsx"], start=BegCrisis, end=TSX.RES.BEG-1))
    AfterRestriction = length(window(TSX[,"tsx"], start=TSX.RES.BEG))
    plot(TSX[,"tsx"],main=paste0("tsx, "
                     ,"Win1:",BeforeCrisis,", Win2:",BeforeRestriction, ", Win3:",AfterRestriction, " - "                                         
                           , "restrictions remained active for ",nrow(WindowShade), " trading days"))
    print(addPolygon(WindowShade, col = "gray90", on = -1))
    addEventLines(xts("50th TD day after the start of the bans",index(serie[50]))
                  ,col=Col0,lwd=10,lty=3,srt=90, pos=2,cex=.85)
    addEventLines(xts("50th TD day after the start of the bans",BegCrisis)
                  ,col=c("gray50"),lwd=10,lty=3,srt=90, pos=2,cex=.85)