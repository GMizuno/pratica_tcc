##################################################################################################################################
# SCRIPT DE ORGANIZACAO DOS DADOS ###############################################################################################
##################################################################################################################################

    library(quantmod)
    library(imputeTS)

    ### ################################################
    ### 1. ORGANIZACAO DOS DADOS - PRECOS DE FECHAMENTO
    ### ################################################
    START = as.Date("2000-01-01")
    END = as.Date("2021-07-19")
    START.DF = as.Date("2000-01-01")
    END.DF = as.Date("2015-12-31")
    
    # 0.a CAC 
    getSymbols(Symbols = "^FCHI", from = START-30, to = END) #@
    CAC = FCHI[,"FCHI.Close"] #@
    names(CAC)="CAC" #@
    remove(FCHI) #@
    CAC = CAC[!is.na(CAC)] #@
    cac = diff(log(CAC)) #@
    CAC = merge(CAC,cac) #@
    CAC = window(CAC, start = START.DF, end  = END.DF) #@
    names(CAC)=c("CAC","cac") #@
    sum(is.na(CAC)) #@
    par(mfrow=c(2,1)); plot(CAC[,"CAC"]); plot(CAC[,"cac"]); par(mfrow=c(1,1)) #@
    save(CAC, file = "CAC.RData") #@

    # 0.b DAX
    getSymbols(Symbols = "^GDAXI", from = START-30, to = END) #@
    DAX = GDAXI[,"GDAXI.Close"] #@
    names(DAX)="DAX" #@
    remove(GDAXI) #@
    DAX = DAX[!is.na(DAX)] #@
    dax = diff(log(DAX)) #@
    DAX = merge(DAX,dax) #@
    DAX = window(DAX, start = START.DF, end  = END.DF) #@
    names(DAX)=c("DAX","dax") #@
    sum(is.na(DAX)) #@
    par(mfrow=c(2,1)); plot(DAX[,"DAX"]); plot(DAX[,"dax"]); par(mfrow=c(1,1)) #@
    save(DAX, file = "DAX.RData") #@
    
    # 0.c FTSE
    getSymbols(Symbols = "^FTSE", from = START-30, to = END) #@
    FTSE = FTSE[,"FTSE.Close"] #@
    names(FTSE)="FTSE" #@
    #remove(FTSE) #@
    FTSE = FTSE[!is.na(FTSE)] #@
    ftse = diff(log(FTSE)) #@
    FTSE = merge(FTSE,ftse) #@
    FTSE = window(FTSE, start = START.DF, end  = END.DF) #@
    names(FTSE)=c("FTSE","ftse") #@
    sum(is.na(FTSE)) #@
    par(mfrow=c(2,1)); plot(FTSE[,"FTSE"]); plot(FTSE[,"ftse"]); par(mfrow=c(1,1)) #@
    save(FTSE, file = "FTSE.RData") #@

    # 0.d NIKKEI
    getSymbols(Symbols = "^N225", from = START-30, to = END) #@
    NIKKEI = N225[,"N225.Close"] #@
    names(NIKKEI)="NIKKEI" #@
    remove(N225) #@
    NIKKEI = NIKKEI[!is.na(NIKKEI)] #@
    nikkei = diff(log(NIKKEI)) #@
    NIKKEI = merge(NIKKEI,nikkei) #@
    NIKKEI = window(NIKKEI, start = START.DF, end  = END.DF) #@
    names(NIKKEI)=c("NIKKEI","nikkei") #@
    sum(is.na(NIKKEI)) #@
    par(mfrow=c(2,1)); plot(NIKKEI[,"NIKKEI"]); plot(NIKKEI[,"nikkei"]); par(mfrow=c(1,1)) #@
    save(NIKKEI, file = "NIKKEI.RData") #@

    # 0.e NYSE
    getSymbols(Symbols = "^NYA", from = START-30, to = END) #@
    NYSE = NYA[,"NYA.Close"] #@
    names(NYSE)="NYSE" #@
    remove(NYA) #@
    NYSE = NYSE[!is.na(NYSE)] #@
    nyse = diff(log(NYSE)) #@
    NYSE = merge(NYSE,nyse) #@
    NYSE = window(NYSE, start = START.DF, end  = END.DF) #@
    names(NYSE)=c("NYSE","nyse") #@
    sum(is.na(NYSE)) #@
    par(mfrow=c(2,1)); plot(NYSE[,"NYSE"]); plot(NYSE[,"nyse"]); par(mfrow=c(1,1)) #@
    save(NYSE, file = "NYSE.RData") #@
    
    # 0.f TSX
    getSymbols(Symbols = "^GSPTSE", from = START-30, to = END) #@
    TSX = GSPTSE[,"GSPTSE.Close"] #@
    names(TSX)="TSX" #@
    remove(GSPTSE) #@
    TSX = TSX[!is.na(TSX)] #@
    tsx = diff(log(TSX)) #@
    TSX = merge(TSX,tsx) #@
    TSX = window(TSX, start = START.DF, end  = END.DF) #@
    names(TSX)=c("TSX","tsx") #@
    sum(is.na(TSX)) #@
    par(mfrow=c(2,1)); plot(TSX[,"TSX"]); plot(TSX[,"tsx"]); par(mfrow=c(1,1)) #@ 
    save(TSX, file = "TSX.RData") #@