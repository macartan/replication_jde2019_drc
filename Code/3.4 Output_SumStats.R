

summary_stats <- t(sumStats)
summary_stats[,2:5] <- round(summary_stats[,2:5], 2)

rapid_only <- c("NA","NA",rep("Yes",13), "No", "Yes")

mfi_index <-  c("NA","NA", rep("No",8),rep("Yes",4), "No", "No", "Yes")
step <- c("NA","NA", rep("D",4), "A,B", rep("A",3), rep("B",2), rep("D",3), "D", "D" )

summary_stats <- cbind(rapid_only, step, mfi_index, summary_stats)

T_SS <- rbind(
  "\\begin{tabular}{lcccccccc}",
  "	Variable &	RAPID only &  Step & Index & N	&	Mean	&	SD	&	Min. & Max.	 \\\\ \\hline \\hline",
#  "\\mc{9}{l}{\\textbf{Village level variables}} \\\\ \\hline",
  mat_to_tex(summary_stats[1:2,], rownames= c("RAPID", "TUUNGANE")) ,
  mat_to_tex(summary_stats[3:17,], rownames= c(varnames[1:15])),
  "\\hline \\hline",  
  "\\label{table_ss}",
  "\\end{tabular}",
  "\\begin{flushleft}\\textit{Notes:} Summary statistics given at the village mean level. The RAPID column indicates whether data was available only in locations in which the RAPID project was introduced. The mean effects column indicates whether a mean effects index us used in place of multiple related outcome variables. 
For variables analyzed as interactions we report here the difference in mean outcomes between the two relevant groups (ie for the list
experiment the difference between the mean outcomes for the long list and short list subjects, and for chief dominance between the chiefs outcome and average citizen outcome).\\end{flushleft}"
)


