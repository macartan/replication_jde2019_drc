
## Remaining:
# hline above the bottom two lines without an empty row

###########
# create table. Data is 4x4
###########

# for SEs we use *2 so we get parentheses in

OUTPUT_varnames <- c("Control", "\\textit{Tuungane}", "Difference", "(se)")
OUTPUT <- array(NA, c(4,4))

pos <- tidy(pos); rownames(pos) <- pos$term
neg <- tidy(neg); rownames(neg) <- neg$term

OUTPUT[1,1] <-round(pos["(Intercept)","estimate"],3)
OUTPUT[2,1] <-round(pos["(Intercept)","estimate"] + pos["TUUNGANE","estimate"],3)
OUTPUT[3,1] <-round(pos["TUUNGANE","estimate"],3)
OUTPUT[4,1] <-round(pos["TUUNGANE","std.error"], 3)

# OUTPUT[1,1] <-round(pos[[1]][1,1],3)
# OUTPUT[2,1] <-round(pos[[1]][1,1] + pos[[1]][2,1],3)
# OUTPUT[3,1] <-round(pos[[1]][2,1],3)
# OUTPUT[4,1] <-pos2[2]

OUTPUT[1,2] <-round(neg["(Intercept)","estimate"],3)
OUTPUT[2,2] <-round(neg["(Intercept)","estimate"] + neg["TUUNGANE","estimate"],3)
OUTPUT[3,2] <-round(neg["TUUNGANE","estimate"],3)
OUTPUT[4,2] <-round(neg["TUUNGANE","std.error"], 3)

# OUTPUT[1,3] <-round(pos[[1]][4,1],3)
# OUTPUT[2,3] <-round(-neg[[1]][4,1],3)
# OUTPUT[3,3] <-round(neg[[1]][3,1],3)
# OUTPUT[4,3] <- neg2[4]

OUTPUT[1,3] <-round(pos["POS_PROMPT","estimate"],3)
OUTPUT[2,3] <-round(-neg["NEG_PROMPT","estimate"],3)
OUTPUT[3,3] <-round(neg["IDS_TUUNGANE_NEG","estimate"],3)
OUTPUT[4,3] <- round(neg["IDS_TUUNGANE_NEG", "std.error"], 3)

OUTPUT[1,4] <-round(pos["POS_PROMPT", "std.error"], 3)
OUTPUT[2,4] <-round(neg["NEG_PROMPT", "std.error"], 3)
OUTPUT[3,4] <- ""
OUTPUT[4,4] <- ""

sd.Table <- rbind(
  "\\begin{tabular}{lcc|cc}",
  "	& Positive prompt &	Negative prompt	&	Difference & (se)\\\\ \\hline \\hline",
  mat_to_tex(OUTPUT, rownames = OUTPUT_varnames),
  "\\hline \\hline ",
  "\\label{table_desirability}",
  "\\end{tabular}",
  "\\begin{flushleft} \\textit{Notes:} N=3,802. 
  Share of individuals answering `yes' to the question ``Do you agree with the view that elections are the best way to choose community representatives
    to serve in positions that require technical expertise?'' $^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$. \\end{flushleft}"
)



## END ##


