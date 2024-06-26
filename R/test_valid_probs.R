v.n <- c("NOH","COH","FOH","NOD","COD","FOD","NOR","COR","FOR","NVH","CVH","FVH","NVD","CVD","FVD","NVR","CVR","FVR","NQH","CQH","FQH","NQD","CQD","FQD","NQR","CQR","FQR", "X")

for(bc in 1900:2100){
  for(t in 1:100){
    for(v.ysq in 1:40){
      for(M_t in v.n){
        
        bc1 = bc-1899
        yr = bc + t - 1899
        
        # create matrix of state transition probabilities
        m.p_t <- matrix(data = 0, nrow = length(v.n), ncol = n.i)  
        # give the state names to the rows
        rownames(m.p_t) <-  v.n                               
        
        # update m.p_t with the probabilities conditional on survival
        # interaction effects: rr.ND.CD, rr.CH.CD, rr.CR.CD, rr.CD.FD)

        # #Happy
        # m.p_t["NH", M_t == "NH"] <- (1-p.NX[t,bc1])*(1-p.NC[t,bc1]-p.HD[t,bc1])
        # m.p_t["CH", M_t == "NH"] <- (1-p.NX[t,bc1])*p.NC[t,bc1]
        # m.p_t["ND", M_t == "NH"] <- (1-p.NX[t,bc1])*p.HD[t,bc1]
        # m.p_t["X" , M_t == "NH"] <- p.NX[t,bc1]

        #from NHO state
        print(paste("t:", t, "yr:", yr))
        m.p_t["NOH", M_t == "NOH"] <- (1-p.NX[t,bc1])*(1-p.NOH.COH[t,yr]-p.NOH.CVH[t,yr]-p.NOH.NVH[t,yr]-p.HD[t,bc1]) #should I use p.NOH.NOH for this?
        m.p_t["COH", M_t == "NOH"] <- (1-p.NX[t,bc1])*(p.NOH.COH[t,yr])
        m.p_t["CVH", M_t == "NOH"] <- (1-p.NX[t,bc1])*(p.NOH.CVH[t,yr])
        m.p_t["NVH", M_t == "NOH"] <- (1-p.NX[t,bc1])*(p.NOH.NVH[t,yr])
        m.p_t["NOD", M_t == "NOH"] <- (1-p.NX[t,bc1])*(p.HD[t,bc1])

        #from CHO state
        m.p_t["COH", M_t == "COH"] <- (1-p.NX[t,bc1])*(1-p.COH.FOH[t,yr]-p.COH.FVH[t,yr]-p.COH.CVH[t,yr]-p.HD[t,bc1])
        m.p_t["FOH", M_t == "COH"] <- (1-p.NX[t,bc1])*(p.COH.FOH[t,yr])
        m.p_t["FVH", M_t == "COH"] <- (1-p.NX[t,bc1])*(p.COH.FVH[t,yr])
        m.p_t["CVH", M_t == "COH"] <- (1-p.NX[t,bc1])*(p.COH.CVH[t,yr])
        m.p_t["COD", M_t == "COH"] <- (1-p.NX[t,bc1])*(p.HD[t,bc1])

        ##from FHO state
        m.p_t["FOH", M_t == "FOH"] <- (1-p.NX[t,bc1])*(1-p.FOH.COH[t,yr]-p.FOH.FVH[t,yr]-p.FOH.CVH[t,yr]-p.HD[t,bc1])
        m.p_t["COH", M_t == "FOH"] <- (1-p.NX[t,bc1])*(p.FOH.COH[t,yr])
        m.p_t["FVH", M_t == "FOH"] <- (1-p.NX[t,bc1])*(p.FOH.FVH[t,yr])
        m.p_t["CVH", M_t == "FOH"] <- (1-p.NX[t,bc1])*(p.FOH.CVH[t,yr])
        m.p_t["FOD", M_t == "FOH"] <- (1-p.NX[t,bc1])*(p.HD[t,bc1])

        ##from NHV state
        m.p_t["NVH", M_t == "NVH"] <- (1-p.NX[t,bc1])*(1-p.NVH.CVH[t,yr]-p.NVH.NQH[t,yr]-p.NVH.CQH[t,yr]-p.HD[t, bc1])
        m.p_t["CVH", M_t == "NVH"] <- (1-p.NX[t,bc1])*(p.NVH.CVH[t,yr])
        m.p_t["NQH", M_t == "NVH"] <- (1-p.NX[t,bc1])*(p.NVH.NQH[t,yr])
        m.p_t["CQH", M_t == "NVH"] <- (1-p.NX[t,bc1])*(p.NVH.CQH[t,yr])
        m.p_t["NVD", M_t == "NVH"] <- (1-p.NX[t,bc1])*(p.HD[t,bc1])

        ##from CHV state
        m.p_t["CVH", M_t == "CVH"] <- (1-p.NX[t,bc1])*(1-p.CVH.CQH[t,yr]-p.CVH.FVH[t,yr]-p.CVH.FQH[t,yr]-p.HD[t,bc1])
        m.p_t["FVH", M_t == "CVH"] <- (1-p.NX[t,bc1])*(p.CVH.FVH[t,yr])
        m.p_t["FQH", M_t == "CVH"] <- (1-p.NX[t,bc1])*(p.CVH.FQH[t,yr])
        m.p_t["CQH", M_t == "CVH"] <- (1-p.NX[t,bc1])*(p.CVH.CQH[t,yr])
        m.p_t["CVD", M_t == "CVH"] <- (1-p.NX[t,bc1])*(p.HD[t,bc1])

        ##from FHV state
        m.p_t["FVH", M_t == "FVH"] <- (1-p.NX[t,bc1])*(1-p.FVH.FQH[t,yr]-p.FVH.CQH[t,yr]-p.FVH.CVH[t,yr]-p.HD[t,bc1])
        m.p_t["FQH", M_t == "FVH"] <- (1-p.NX[t,bc1])*(p.FVH.FQH[t,yr])
        m.p_t["CQH", M_t == "FVH"] <- (1-p.NX[t,bc1])*(p.FVH.CQH[t,yr])
        m.p_t["CVH", M_t == "FVH"] <- (1-p.NX[t,bc1])*(p.FVH.CVH[t,yr])
        m.p_t["FVD", M_t == "FVH"] <- (1-p.NX[t,bc1])*(p.FVH.FVD[t,yr])

        ##from NHQ state
        m.p_t["NQH", M_t == "NQH"] <- (1-p.NX[t,bc1])*(1-p.NQH.CQH[t,yr]-p.NQH.CVH[t,yr]-p.NQH.NVH[t,yr]-p.HD[t,bc1])
        m.p_t["CQH", M_t == "NQH"] <- (1-p.NX[t,bc1])*(p.NQH.CQH[t,yr])
        m.p_t["CVH", M_t == "NQH"] <- (1-p.NX[t,bc1])*(p.NQH.CVH[t,yr])
        m.p_t["NVH", M_t == "NQH"] <- (1-p.NX[t,bc1])*(p.NQH.NVH[t,yr])
        m.p_t["NQD", M_t == "NQH"] <- (1-p.NX[t,bc1])*(p.NQH.NQD[t,yr])

        ##from CHQ state
        m.p_t["CQH", M_t == "CQH"] <- (1-p.NX[t,bc1])*(1-p.CQH.FQH[t,yr]-p.CQH.FVH[t,yr]-p.CQH.CVH[t,yr]-p.HD[t,bc1])
        m.p_t["FQH", M_t == "CQH"] <- (1-p.NX[t,bc1])*(p.CQH.FQH[t,yr])
        m.p_t["FVH", M_t == "CQH"] <- (1-p.NX[t,bc1])*(p.CQH.FVH[t,yr])
        m.p_t["CVH", M_t == "CQH"] <- (1-p.NX[t,bc1])*(p.CQH.CVH[t,yr])
        m.p_t["CQD", M_t == "CQH"] <- (1-p.NX[t,bc1])*(p.HD[t,bc1])

        ##from FHQ state
        m.p_t["FQH", M_t == "FQH"] <- (1-p.NX[t,bc1])*(1-p.FQH.FVH[t,yr]-p.FQH.CVH[t,yr]-p.FQH.CQH[t,yr]-p.HD[t,bc1])
        m.p_t["FVH", M_t == "FQH"] <- (1-p.NX[t,bc1])*(p.FQH.FVH[t,yr])
        m.p_t["CVH", M_t == "FQH"] <- (1-p.NX[t,bc1])*(p.FQH.CVH[t,yr])
        m.p_t["CQH", M_t == "FQH"] <- (1-p.NX[t,bc1])*(p.FQH.CQH[t,yr])
        m.p_t["FQD", M_t == "FQH"] <- (1-p.NX[t,bc1])*(p.HD[t,bc1])

        ##from NDO state
        m.p_t["NOD", M_t == "NOD"] <- (1-p.NX[t,bc1])*(1-p.NOD.COD[t,yr]-p.NOD.CVD[t,yr]-p.NOD.NVD[t,yr]-p.DR[t])
        m.p_t["COD", M_t == "NOD"] <- (1-p.NX[t,bc1])*(p.NOD.COD[t,yr])
        m.p_t["CVD", M_t == "NOD"] <- (1-p.NX[t,bc1])*(p.NOD.CVD[t,yr])
        m.p_t["NVD", M_t == "NOD"] <- (1-p.NX[t,bc1])*(p.NOD.NVD[t,yr])
        m.p_t["NOR", M_t == "NOD"] <- (1-p.NX[t,bc1])*(p.DR[t])

        ##from CDO state
        m.p_t["COD", M_t == "COD"] <- (1-p.NX[t,bc1])*(1-p.COD.FOD[t,yr]-p.COD.FVD[t,yr]-p.COD.CVD[t,yr]-p.DR[t])
        m.p_t["FOD", M_t == "COD"] <- (1-p.NX[t,bc1])*(p.COD.FOD[t,yr])
        m.p_t["FVD", M_t == "COD"] <- (1-p.NX[t,bc1])*(p.COD.FVD[t,yr])
        m.p_t["CVD", M_t == "COD"] <- (1-p.NX[t,bc1])*(p.COD.CVD[t,yr])
        m.p_t["COR", M_t == "COD"] <- (1-p.NX[t,bc1])*(p.DR[t])

        ##from FDO state
        m.p_t["FOD", M_t == "FOD"] <- (1-p.NX[t,bc1])*(1-p.FOD.FVD[t,yr]-p.FOD.CVD[t,yr]-p.FOD.COD[t,yr]-p.DR[t])
        m.p_t["FVD", M_t == "FOD"] <- (1-p.NX[t,bc1])*(p.FOD.FVD[t,yr])
        m.p_t["CVD", M_t == "FOD"] <- (1-p.NX[t,bc1])*(p.FOD.CVD[t,yr])
        m.p_t["COD", M_t == "FOD"] <- (1-p.NX[t,bc1])*(p.FOD.COD[t,yr])
        m.p_t["FOR", M_t == "FOD"] <- (1-p.NX[t,bc1])*(p.DR[t])

        ##from NDV state
        m.p_t["NVD", M_t == "NVD"] <- (1-p.NX[t,bc1])*(1-p.NVD.CVD[t,yr]-p.NVD.NQD[t,yr]-p.NVD.CQD[t,yr]-p.DR[t])
        m.p_t["CVD", M_t == "NVD"] <- (1-p.NX[t,bc1])*(p.NVD.CVD[t,yr])
        m.p_t["CQD", M_t == "NVD"] <- (1-p.NX[t,bc1])*(p.NVD.CQD[t,yr])
        m.p_t["NQD", M_t == "NVD"] <- (1-p.NX[t,bc1])*(p.NVD.NQD[t,yr])
        m.p_t["NVR", M_t == "NVD"] <- (1-p.NX[t,bc1])*(p.DR[t])

        ##from CDV state
        m.p_t["CVD", M_t == "CVD"] <- (1-p.NX[t,bc1])*(1-p.CVD.FVD[t,yr]-p.CVD.FQD[t,yr]-p.CVD.CQD[t,yr]-p.DR[t])
        m.p_t["FVD", M_t == "CVD"] <- (1-p.NX[t,bc1])*(p.CVD.FVD[t,yr])
        m.p_t["FQD", M_t == "CVD"] <- (1-p.NX[t,bc1])*(p.CVD.FQD[t,yr])
        m.p_t["CQD", M_t == "CVD"] <- (1-p.NX[t,bc1])*(p.CVD.CQD[t,yr])
        m.p_t["CVR", M_t == "CVD"] <- (1-p.NX[t,bc1])*(p.DR[t])

        ##from FDV state
        m.p_t["FVD", M_t =="FVD"] <- (1-p.NX[t,bc1])*(1-p.FVD.FQD[t,yr]-p.FVD.CQD[t,yr]-p.FVD.CVD[t,yr]-p.DR[t])
        m.p_t["FQD", M_t =="FVD"] <- (1-p.NX[t,bc1])*(p.FVD.FQD[t,yr])
        m.p_t["CQD", M_t =="FVD"] <- (1-p.NX[t,bc1])*(p.FVD.CQD[t,yr])
        m.p_t["CVD", M_t =="FVD"] <- (1-p.NX[t,bc1])*(p.FVD.CVD[t,yr])
        m.p_t["FVR", M_t =="FVD"] <- (1-p.NX[t,bc1])*(p.DR[t])

        ##from NDQ state
        m.p_t["NQD", M_t == "NQD"] <- (1-p.NX[t,bc1])*(1-p.NQD.NVD[t,yr]-p.NVD.CVD[t,yr]-p.NVD.CQD[t,yr]-p.DR[t])
        m.p_t["NVD", M_t == "NQD"] <- (1-p.NX[t,bc1])*(p.NQD.NVD[t,yr])
        m.p_t["CVD", M_t == "NQD"] <- (1-p.NX[t,bc1])*(p.NQD.CVD[t,yr])
        m.p_t["CQD", M_t == "NQD"] <- (1-p.NX[t,bc1])*(p.NQD.CQD[t,yr])
        m.p_t["NQR", M_t == "NQD"] <- (1-p.NX[t,bc1])*(p.DR[t])

        ##from CDQ state
        m.p_t["CQD", M_t == "CQD"] <- (1-p.NX[t,bc1])*(1-p.CQD.FQD[t,yr]-p.CQD.FVD[t,yr]-p.CQD.CVD[t,yr]-p.DR[t])
        m.p_t["FQD", M_t == "CQD"] <- (1-p.NX[t,bc1])*(p.CQD.FQD[t,yr])
        m.p_t["FVD", M_t == "CQD"] <- (1-p.NX[t,bc1])*(p.CQD.FVD[t,yr])
        m.p_t["CVD", M_t == "CQD"] <- (1-p.NX[t,bc1])*(p.CQD.CVD[t,yr])
        m.p_t["CQR", M_t == "CQD"] <- (1-p.NX[t,bc1])*(p.DR[t])

        ##from FDQ state
        m.p_t["FQD", M_t == "FQD"] <- (1-p.NX[t,bc1])*(1-p.FQD.FVD[t,yr]-p.FQD.CVD[t,yr]-p.FQD.CQD[t,yr]-p.DR[t])
        m.p_t["FVD", M_t == "FQD"] <- (1-p.NX[t,bc1])*(p.FQD.FVD[t,yr])
        m.p_t["CVD", M_t == "FQD"] <- (1-p.NX[t,bc1])*(p.FQD.CVD[t,yr])
        m.p_t["CQD", M_t == "FQD"] <- (1-p.NX[t,bc1])*(p.FQD.CQD[t,yr])
        m.p_t["FQR", M_t == "FQD"] <- (1-p.NX[t,bc1])*(p.DR[t])

        ##from NRO state
        m.p_t["NOR", M_t == "NOR"] <- (1-p.NX[t,bc1])*(1-p.NOR.COR[t,yr]-p.NOR.CVR[t,yr]-p.NOR.NVR[t,yr]-p.RD[t])
        m.p_t["COR", M_t == "NOR"] <- (1-p.NX[t,bc1])*(p.NOR.COR[t,yr])
        m.p_t["CVR", M_t == "NOR"] <- (1-p.NX[t,bc1])*(p.NOR.CVR[t,yr])
        m.p_t["NVR", M_t == "NOR"] <- (1-p.NX[t,bc1])*(p.NOR.NVR[t,yr])
        m.p_t["NOD", M_t == "NOR"] <- (1-p.NX[t,bc1])*(p.RD[t])

        ##from CRO state
        m.p_t["COR", M_t == "COR"] <- (1-p.NX[t,bc1])*(1-p.COR.FOR[t,yr]-p.COR.FVR[t,yr]-p.COR.CVR[t,yr]-p.RD[t])
        m.p_t["FOR", M_t == "COR"] <- (1-p.NX[t,bc1])*(p.COR.FOR[t,yr])
        m.p_t["FVR", M_t == "COR"] <- (1-p.NX[t,bc1])*(p.COR.FVR[t,yr])
        m.p_t["CVR", M_t == "COR"] <- (1-p.NX[t,bc1])*(p.COR.CVR[t,yr])
        m.p_t["COD", M_t == "COR"] <- (1-p.NX[t,bc1])*(p.RD[t])

        ##from FRO state
        m.p_t["FOR", M_t == "FOR"] <- (1-p.NX[t,bc1])*(1-p.FOR.FVR[t,yr]-p.FOR.CVR[t,yr]-p.FOR.COR[t,yr]-p.RD[t])
        m.p_t["FVR", M_t == "FOR"] <- (1-p.NX[t,bc1])*(p.FOR.FVR[t,yr])
        m.p_t["CVR", M_t == "FOR"] <- (1-p.NX[t,bc1])*(p.FOR.CVR[t,yr])
        m.p_t["COR", M_t == "FOR"] <- (1-p.NX[t,bc1])*(p.FOR.COR[t,yr])
        m.p_t["FOD", M_t == "FOR"] <- (1-p.NX[t,bc1])*(p.RD[t])

        ##from NRV state
        m.p_t["NVR", M_t == "NVR"] <- (1-p.NX[t,bc1])*(1-p.NVR.CVR[t,yr]-p.NVR.CQR[t,yr]-p.NVR.NQR[t,yr]-p.RD[t])
        m.p_t["CVR", M_t == "NVR"] <- (1-p.NX[t,bc1])*(p.NVR.CVR[t,yr])
        m.p_t["CQR", M_t == "NVR"] <- (1-p.NX[t,bc1])*(p.NVR.CQR[t,yr])
        m.p_t["NQR", M_t == "NVR"] <- (1-p.NX[t,bc1])*(p.NVR.NQR[t,yr])
        m.p_t["NVD", M_t == "NVR"] <- (1-p.NX[t,bc1])*(p.RD[t])

        ##from CRV state
        m.p_t["CVR", M_t == "CVR"] <- (1-p.NX[t,bc1])*(1-p.CVR.FVR[t,yr]-p.CVR.FQR[t,yr]-p.CVR.CQR[t,yr]-p.RD[t])
        m.p_t["FVR", M_t == "CVR"] <- (1-p.NX[t,bc1])*(p.CVR.FVR[t,yr])
        m.p_t["FQR", M_t == "CVR"] <- (1-p.NX[t,bc1])*(p.CVR.FQR[t,yr])
        m.p_t["CQR", M_t == "CVR"] <- (1-p.NX[t,bc1])*(p.CVR.CQR[t,yr])
        m.p_t["CVD", M_t == "CVR"] <- (1-p.NX[t,bc1])*(p.RD[t])

        ##from FRV state
        m.p_t["FVR", M_t == "FVR"] <- (1-p.NX[t,bc1])*(1-p.FVR.FQR[t,yr]-p.FVR.CQR[t,yr]-p.FVR.CVR[t,yr]-p.RD[t])
        m.p_t["FQR", M_t == "FVR"] <- (1-p.NX[t,bc1])*(p.FVR.FQR[t,yr])
        m.p_t["CQR", M_t == "FVR"] <- (1-p.NX[t,bc1])*(p.FVR.CQR[t,yr])
        m.p_t["CVR", M_t == "FVR"] <- (1-p.NX[t,bc1])*(p.FVR.CVR[t,yr])
        m.p_t["FVD", M_t == "FVR"] <- (1-p.NX[t,bc1])*(p.RD[t])

        ##from NRQ state
        m.p_t["NQR", M_t == "NQR"] <- (1-p.NX[t,bc1])*(1-p.NQR.CQR[t,yr]-p.NQR.CVR[t,yr]-p.NQR.NVR[t,yr]-p.RD[t])
        m.p_t["CQR", M_t == "NQR"] <- (1-p.NX[t,bc1])*(p.NQR.CQR[t,yr])
        m.p_t["CVR", M_t == "NQR"] <- (1-p.NX[t,bc1])*(p.NQR.CVR[t,yr])
        m.p_t["NVR", M_t == "NQR"] <- (1-p.NX[t,bc1])*(p.NQR.NVR[t,yr])
        m.p_t["NQD", M_t == "NQR"] <- (1-p.NX[t,bc1])*(p.RD[t])

        ##from CRQ state
        m.p_t["CQR", M_t == "CQR"] <- (1-p.NX[t,bc1])*(1-p.CQR.FQR[t,yr]-p.CQR.FVR[t,yr]-p.CQR.CVR[t,yr]-p.RD[t])
        m.p_t["FQR", M_t == "CQR"] <- (1-p.NX[t,bc1])*(p.CQR.FQR[t,yr])
        m.p_t["FVR", M_t == "CQR"] <- (1-p.NX[t,bc1])*(p.CQR.FVR[t,yr])
        m.p_t["CVR", M_t == "CQR"] <- (1-p.NX[t,bc1])*(p.CQR.CVR[t,yr])
        m.p_t["CQD", M_t == "CQR"] <- (1-p.NX[t,bc1])*(p.RD[t])

        ##from FRQ state
        m.p_t["FQR", M_t == "FQR"] <- (1-p.NX[t,bc1])*(1-p.FQR.FVR[t,yr]-p.FQR.CVR[t,yr]-p.FQR.CQR[t,yr]-p.RD[t])
        m.p_t["FVR", M_t == "FQR"] <- (1-p.NX[t,bc1])*(p.FQR.FVR[t,yr])
        m.p_t["CVR", M_t == "FQR"] <- (1-p.NX[t,bc1])*(p.FQR.CVR[t,yr])
        m.p_t["CQR", M_t == "FQR"] <- (1-p.NX[t,bc1])*(p.FQR.CQR[t,yr])
        m.p_t["FQD", M_t == "FQR"] <- (1-p.NX[t,bc1])*(p.RD[t])


        # m.p_t["CH", M_t == "CH"] <- (1-p.CX[t,bc1])*(1-rr.CH.CD*p.HD[t,bc1]-p.CF[t,bc1])
        # m.p_t["FH", M_t == "CH"] <- (1-p.CX[t,bc1])*p.CF[t,bc1]
        # m.p_t["CD", M_t == "CH"] <- (1-p.CX[t,bc1])*rr.CH.CD*p.HD[t,bc1]
        # m.p_t["X" , M_t == "CH"] <- p.CX[t,bc1]
        # 
        # m.p_t["FH", M_t == "FH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t=="FH"]])*(1 -p.HD[t,bc1])
        # m.p_t["FD", M_t == "FH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t=="FH"]])*p.HD[t,bc1]
        # m.p_t["X" , M_t =="FH"] <-	a_p.FX.ysq[t, bc1, v.ysq[M_t=="FH"]]
        # 
        # # Depressed
        # m.p_t["ND", M_t == "ND"] <- (1-rr.DX[t]*p.NX[t,bc1])*(1-rr.ND.CD*p.NC[t,bc1]-p.DR[t])
        # m.p_t["CD", M_t == "ND"] <- (1-rr.DX[t]*p.NX[t,bc1])*rr.ND.CD*p.NC[t,bc1]
        # m.p_t["NR", M_t == "ND"] <- (1-rr.DX[t]*p.NX[t,bc1])*p.DR[t]
        # m.p_t["X" , M_t == "ND"] <- rr.DX[t]*p.NX[t,bc1]
        # 
        # m.p_t["CD", M_t == "CD"] <- (1-rr.DX[t]*p.CX[t,bc1])*(1-rr.CD.FD*p.CF[t,bc1]-p.DR[t])
        # m.p_t["FD", M_t == "CD"] <-  (1-rr.DX[t]*p.CX[t,bc1])*rr.CD.FD*p.CF[t,bc1]
        # m.p_t["CR", M_t == "CD"] <-  (1-rr.DX[t]*p.CX[t,bc1])*p.DR[t]
        # m.p_t["X" , M_t == "CD"] <-  rr.DX[t]*p.CX[t,bc1]
        # 
        # m.p_t["FD", M_t == "FD"] <- (1-rr.DX[t]*a_p.FX.ysq[t, bc1, v.ysq[M_t=="FD"]])*(1-p.DR[t])
        # m.p_t["FR", M_t == "FD"] <- (1-rr.DX[t]*a_p.FX.ysq[t, bc1, v.ysq[M_t=="FD"]])*p.DR[t]
        # m.p_t["X" , M_t == "FD"] <- rr.DX[t]*a_p.FX.ysq[t, bc1, v.ysq[M_t=="FD"]]
        # 
        # # Recovered
        # m.p_t["ND", M_t == "NR"] <- (1-p.NX[t,bc1])*p.RD[t]
        # m.p_t["NR", M_t == "NR"] <- (1-p.NX[t,bc1])*(1-p.NC[t,bc1]-p.RD[t])
        # m.p_t["CR", M_t == "NR"] <- (1-p.NX[t,bc1])*p.NC[t,bc1]
        # m.p_t["X" , M_t == "NR"] <- p.NX[t,bc1]
        # 
        # m.p_t["CD", M_t == "CR"] <- (1-p.CX[t,bc1])*rr.CR.CD*p.RD[t]
        # m.p_t["CR", M_t == "CR"] <- (1-p.CX[t,bc1])*(1-rr.CR.CD*p.RD[t]-p.CF[t,bc1])
        # m.p_t["FR", M_t == "CR"] <- (1-p.CX[t,bc1])*p.CF[t,bc1]
        # m.p_t["X" , M_t == "CR"] <-   p.CX[t,bc1]
        # 
        # m.p_t["FD", M_t == "FR"] <- (1-a_p.FX.ysq[t, bc1, v.ysq[M_t=="FR"]])*p.RD[t]
        # m.p_t["FR", M_t == "FR"] <- (1-a_p.FX.ysq[t, bc1, v.ysq[M_t=="FR"]])*(1-p.RD[t])
        # m.p_t["X" , M_t == "FR"] <- a_p.FX.ysq[t, bc1, v.ysq[M_t=="FR"]]
        # 
        m.p_t["X" , M_t == "X"] <-  1		
        
        
        
        # print birth cohort and age for debugging problematic transition probabilities
        print(paste0("bc: ", bc, ", age: ",t))
        check_transition_probability(m.p_t,verbose=TRUE)
        #check_sum_of_transition_array(t(m.p_t), n_rows=n.i, n_cycles= 100, verbose = TRUE)
      }
    }
  }
}
