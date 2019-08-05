
T_spill05 <- rbind(
  "\\centering",
  "\\scriptsize",
  "\\begin{tabular}{l|ccccc}",
  "	& Direct       &  Indirect  &  RMSE & ($p$) & N    \\\\ \\hline \\hline",
  "\\mc{6}{l}{\\textbf{Spillovers at 5km}} \\\\ \\hline",
  mat_to_tex(CONTENT05 [1:5,-c(2,4)], rownames = varnames[1:5]),
  "\\hline \\mc{6}{l}{\\textbf{Participation}} \\\\ \\hline",
  mat_to_tex(CONTENT05 [6:10,-c(2,4)], rownames = varnames[6:10]),
  "\\hline \\mc{6}{l}{\\textbf{Accountability}} \\\\ \\hline",
  mat_to_tex(CONTENT05 [11:12,-c(2,4)], rownames = varnames[11:12]),
  "\\hline \\mc{6}{l}{\\textbf{Transparency}} \\\\ \\hline",
  mat_to_tex(CONTENT05 [13:15,-c(2,4)], rownames = varnames[13:15]),
  "\\hline \\hline",
  "\\label{table_spillovers05}",
  "\\end{tabular}",
  "\\begin{flushleft}\\textit{Notes:} 
  Spillover effects estimated using a regression model of the form $Y = \\alpha Direct + \\beta Indirect + \\gamma Direct \\times
  Indirect$ where both the  direct and indirect maesures are normalized to have zero means. Average
  direct and indirect effects are then given by $\\alpha$ and $\\beta$. 
  RMSE is used as a test statistic for the randomization infernence and the $p$ value reports the probability of such a low RMSE under the sharp null of no effects. \\end{flushleft}"
)




T_spill20 <- rbind(
  "\\centering",
  "\\scriptsize",
  "\\begin{tabular}{l|ccccc}",
  "	& Direct      &    Indirect  &  RMSE & ($p$) & N   \\\\ \\hline \\hline",
  "\\mc{6}{l}{\\textbf{Spillovers at 20km}} \\\\ \\hline",
  mat_to_tex(CONTENT20 [1:5,-c(2,4)], rownames = varnames[1:5]),
  "\\hline \\mc{6}{l}{\\textbf{Participation}} \\\\ \\hline",
  mat_to_tex(CONTENT20 [6:10,-c(2,4)], rownames = varnames[6:10]),
  "\\hline \\mc{6}{l}{\\textbf{Accountability}} \\\\ \\hline",
  mat_to_tex(CONTENT20 [11:12,-c(2,4)], rownames = varnames[11:12]),
  "\\hline \\mc{6}{l}{\\textbf{Transparency}} \\\\ \\hline",
  mat_to_tex(CONTENT20 [13:15,-c(2,4)], rownames = varnames[13:15]),
  "\\hline \\hline",   "\\label{table_spillovers20}",
  "\\end{tabular}",
  "\\begin{flushleft}\\textit{Notes:} 
  Spillover effects estimated using a regression model of the form $Y = \\alpha Direct + \\beta Indirect + \\gamma Direct \\times
  Indirect$ where both the  direct and indirect maesures are normalized to have zero means. Average
  direct and indirect effects are then given by $\\alpha$ and $\\beta$. 
  RMSE is used as a test statistic for the randomization infernence and the $p$ value reports the probability of such a low RMSE under the sharp null of no effects. \\end{flushleft}"
  
)

