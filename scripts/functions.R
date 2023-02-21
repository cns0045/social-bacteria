## Custom functions

## Read log data in
  # @param cat_file the filename of the concatenated file
  # @param log_file where to find the log files if the concatenated file does not exist
readLogData <- function(cat_file = "data/cat-data.csv", log_folder = "logs/", overwrite = FALSE) {
## Only create new file if concatenated csv doesn't already exist
if (!file.exists(cat_file) || overwrite == TRUE) {
  ## List all files in the logs directory
  files <- list.files(log_folder, full.names = TRUE)

  ## Define an empty data frame to populate with logs
  cat <- data.frame()

  ## Read all files into one data frame
  for (file in files) {
    . <- read.csv(file)
    cat = rbind(cat, .) }

  ## Save the concatenated data frame
  write.csv(cat, file = cat_file, row.names = FALSE)
}

## Else read existing file
cat <- read.csv(cat_file)

## Change vector types
cat$Run <- as.factor(cat$Run)
cat$cycle <- as.integer(cat$cycle)
cat$Cheat <- as.factor(cat$Cheat)
cat$Spite <- as.factor(cat$Spite)
#cat$Virulence <- as.integer(cat$Virulence)
cat$MigrationRate <- as.factor(signif(as.numeric(cat$MigrationRate),2))
cat$SubpopCarryingCapacity <- as.factor(signif(as.numeric(cat$SubpopCarryingCapacity),2))
cat$CheatFreq <- signif(as.numeric(cat$CheatFreq),2)
cat$SpiteFreq <- signif(as.numeric(cat$SpiteFreq),2)

cat
}

## Summarize log file (after grouping)
    ## mean and sd of number of cycles it takes to fix or lose
    ## mean and sd of cheat frequency and spite frequency
    # @param grouped_data a data.frame grouped as desired
createSummaryTable <- function(grouped_data) {
  . <- summarise(grouped_data, meanCycle=mean(cycle), sdCycle=sd(cycle),
                 meanCheatFreq=mean(CheatFreq), sdCheatFreq=sd(CheatFreq),
                 meanSpiteFreq=mean(SpiteFreq), sdSpiteFreq=sd(SpiteFreq))
  data.frame(.)
}

plotEndsCheat <- function(data) {
  ggplot(data, aes(x = MigrationRate, y = meanCheatFreq,
    color = SubpopCarryingCapacity, group = SubpopCarryingCapacity))+
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +
    geom_line(aes(x = MigrationRate, y = meanCheatFreq))+
    geom_pointrange(aes(ymin = meanCheatFreq - sdCheatFreq,
      ymax=meanCheatFreq + sdCheatFreq), position = position_dodge(width = 0.3)) +
    theme_minimal() +
    scale_color_brewer(palette="Dark2")
}

plotEndsSpite <- function(data) {
  ggplot(data, aes(x = MigrationRate, y = meanSpiteFreq,
    color = SubpopCarryingCapacity, group = SubpopCarryingCapacity))+
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +
    geom_line(aes(x = MigrationRate, y = meanSpiteFreq))+
    geom_pointrange(aes(ymin = meanSpiteFreq - sdSpiteFreq,
      ymax=meanSpiteFreq + sdSpiteFreq), position = position_dodge(width = 0.3)) +
    theme_minimal() +
    scale_color_brewer(palette="Dark2")
}

plotEndsNoVirulence <- function(summary_data) {
  a <- summary_data[summary_data$Cheat==1 & summary_data$Spite==0,]
  b <- summary_data[summary_data$Cheat==1 & summary_data$Spite==1,]
  c <- summary_data[summary_data$Cheat==0 & summary_data$Spite==1,]

  a_plot <- plotEndsCheat(a) + labs(x = expression(italic("m")), y = expression(italic("G"["c"])),
    title='a. cheat, no spite', col = expression(italic("K"["x"])))
  b_plot <- plotEndsCheat(b) + labs(x = expression(italic("m")), y = expression(italic("G"["c"])),
    title='b. cheat and spite', col = expression(italic("K"["x"])))
  c_plot <- plotEndsSpite(c) + labs(x = expression(italic("m")), y = expression(italic("G"["s"])),
    title='c. no cheat, spite', col = expression(italic("K"["x"])))

  . <- grid.arrange(a_plot, b_plot, c_plot, ncol=1)
  .
}
