# Create a basic model diagram representing the different health states and transitions between them
library(diagram)
m_P_diag <- matrix(0, nrow = n.s, ncol = n.s, dimnames = list(v.n, v.n))
m_P_diag["NH", "CH" ] = "" 
m_P_diag["NH", "ND" ] = ""
m_P_diag["NH", "X" ] = ""
m_P_diag["CH", "CD" ] = ""
m_P_diag["CH", "FH" ] = ""
m_P_diag["CH", "X" ] = ""
m_P_diag["FH", "FD" ] = ""
m_P_diag["FH", "X" ] = ""

m_P_diag["ND", "CD" ] = "" 
m_P_diag["ND", "NR" ] = ""
m_P_diag["ND", "X" ] = ""
m_P_diag["CD", "CR" ] = ""
m_P_diag["CD", "FD" ] = ""
m_P_diag["CD", "X" ] = ""
m_P_diag["FD", "FR" ] = ""
m_P_diag["FD", "X" ] = ""

m_P_diag["NR", "CR" ] = "" 
m_P_diag["NR", "ND" ] = ""
m_P_diag["NR", "X" ] = ""
m_P_diag["CR", "CD" ] = ""
m_P_diag["CR", "FR" ] = ""
m_P_diag["CR", "X" ] = ""
m_P_diag["FR", "FD" ] = ""
m_P_diag["FR", "X" ] = ""

layout.fig <- c(3,3,3,1)
plotmat(t(m_P_diag), t(layout.fig), self.cex = 0.2, curve = 0, arr.pos = 0.7,
        arr.length = 0.2,
        latex = T, arr.type = "curved", relsize = 0.85, box.prop = 0.8, 
        cex = 0.8, box.cex = 0.7, lwd = 1,dtext=c(-4,1),endhead=TRUE)
