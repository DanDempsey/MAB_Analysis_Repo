##### Visualise Data
##### Daniel Dempsey

### Load libraries
library( readr )

### Colour pallette for plots, taken from: https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
mycol <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",  
           "#0072B2", "#D55E00", "#CC79A7", "#999999", '#FFFFFF')
scales::show_col(mycol)

### Read in data
exp1_dat <- read_csv( 'Data/Processed_Data/Exp1_dat.csv', col_types = 'icffl' )
exp2_dat <- read_csv( 'Data/Processed_Data/Exp2_dat.csv', col_types = 'iciiil' )
exp3_dat <- read_csv( 'Data/Processed_Data/Exp3_dat.csv', col_types = 'icff' )

setwd( 'Output' )

##### Experiment 1
### Missing data
exp1_dat[is.na( exp1_dat$result ), c('ID', 'question')] # No missing

### Barplots split by question
q1_dat <- split( exp1_dat, exp1_dat$question )
expected_responses <- sapply( q1_dat, function(x) {x$expected[1]} )

pdf( 'Experiment_1/Bar_All.pdf' )
for ( i in seq_along(q1_dat) ) {
  
  #pdf( paste0('Experiment_1/Bar_', i, '.pdf') )
  q_dat <- q1_dat[[i]]
  plot( 0, 0, type = 'n', xaxt = 'n', yaxt = 'n', xlim = c(0.25, 2.35), bty = 'n',
        ylim = c(0, 1), main = q_dat$question[1], xlab = '', ylab = 'Proportion' )
  q_exp <- levels(q_dat$result) == expected_responses[i]
  use_col <- ifelse( q_exp, mycol[2], mycol[1] )
  barplot( table( q_dat$result ) / nrow(q_dat), add = TRUE, border = use_col,
           col = use_col )
  abline( h = seq(0, 1, 0.2), lty = 3, col = 'lightgrey' )
  #dev.off()
  
}
dev.off()

### Individual plots
drop_ind <- !exp1_dat$corrupt
ind1_wrong <- split( exp1_dat$result[drop_ind] != exp1_dat$expected[drop_ind], 
                     exp1_dat$ID[drop_ind] )
count_wrong1 <- sapply( ind1_wrong, sum )

pdf( 'Experiment_1/Num_Incorrect.pdf' )
barplot( table( count_wrong1 ), col = mycol[3], yaxt = 'n', 
         ylim = c(0, 40), main = 'Number of Incorrect Answers 1' )
axis( 2, at = seq(0, 40, 5) )
abline( h = seq(0, 35, 5), lty = 3, col = 'lightgrey' )
dev.off()

##### Experiment 2
### Missing data
exp2_dat[is.na( exp2_dat$result ), c('ID', 'question')]

### Boxplots split by question groupings
q2_dat <- split( exp2_dat, exp2_dat$groups )

pdf( 'Experiment_2/Box_All.pdf', width = 15 )
for ( i in seq_along(q2_dat) ) {
  
  #pdf( paste0('Experiment_2/Box_', i, '.pdf'), width = 15 )
  q_dat <- q2_dat[[i]]
  main_question <- unique( sub( "\n.*", "", q_dat$question ) )
  q_dat$sub_question <- sub( ".*\n", "", q_dat$question )
  
  use_col <- rep( mycol[1], 4 )
  if ( any(q_dat$expected) > 0 ) {
    blue_answer <- which( table( q_dat$sub_question, q_dat$expected )[, 2] > 0 )
    use_col[blue_answer] <- mycol[2]
  }
  
  boxplot( result ~ sub_question, data = q_dat, main = main_question,
           xlab = '', ylab = 'Score', border = use_col, col = mycol[9],
           ylim = c(0, 10), pch = 20 )
  abline( h = seq(0, 10, 2), lty = 3, col = 'lightgrey' )
  #dev.off()
  
}
dev.off()

### Distribution of 'correctness'
pdf( 'Experiment_2/Box_Correctness.pdf' )
boxplot( result ~ expected, data = exp2_dat[!exp2_dat$corrupt, ],
         col = mycol[3], pch = 20, xlab = 'Expected', ylab = 'Score',
         main = 'Distribution of Expectation Matching' )
dev.off()


##### Experiment 3
### Missing data
exp3_dat[is.na( exp3_dat$result ), c('ID', 'question')] # No missing

### Barplots split by question
q3_dat <- split( exp3_dat, exp3_dat$question )

pdf( 'Experiment_3/Bar_All.pdf' )
for ( i in seq_along(q3_dat) ) {
  
  #pdf( paste0('Experiment_3/Bar_', i, '.pdf') )
  q_dat <- q3_dat[[i]]
  plot( 0, 0, type = 'n', xaxt = 'n', yaxt = 'n', xlim = c(0.25, 2.35), bty = 'n',
        ylim = c(0, 1), main = q_dat$question[1], xlab = '', ylab = 'Proportion' )
  use_col <- mycol[2:1]
  barplot( table( q_dat$result ) / nrow(q_dat), add = TRUE, border = use_col,
           col = use_col )
  abline( h = seq(0, 1, 0.2), lty = 3, col = 'lightgrey' )
  #dev.off()
  
}
dev.off()

### Individual plots
ind3_wrong <- split( as.character(exp3_dat$result) != as.character(exp3_dat$expected), 
                     exp3_dat$ID )
count_wrong3 <- sapply( ind3_wrong, sum )

pdf( 'Experiment_3/Num_Incorrect.pdf' )
barplot( table( count_wrong3 ), col = mycol[3], yaxt = 'n', 
         ylim = c(0, 60), main = 'Number of Incorrect Answers 3' )
axis( 2, at = seq(0, 60, 10) )
abline( h = seq(0, 60, 10), lty = 3, col = 'lightgrey' )
dev.off()


##### 1 + 3 Wrong
ind_wrong <- Map( c, ind1_wrong, ind3_wrong )
count_wrong <- sapply( ind_wrong, sum )
tab_count <- table( count_wrong )
tab_count_corrected <- c( tab_count[1:8], 0, tab_count[9] )
names( tab_count_corrected )[9] <- '8'

pdf( 'All/Num_Incorrect_1_3.pdf' )
barplot( tab_count_corrected, col = mycol[3], yaxt = 'n', 
         ylim = c(0, 30), main = 'Number of Incorrect Answers 1 and 3' )
axis( 2, at = seq(0, 30, 5) )
abline( h = seq(0, 30, 5), lty = 3, col = 'lightgrey' )
dev.off()

all_wrong_dat <- data.frame( ID = names( count_wrong ),
                             experiment_1 = count_wrong1,
                             experiment_3 = count_wrong3,
                             experiment_1_and_3 = count_wrong )

# Write wrong data to file
write_csv( all_wrong_dat, 'All/Number_Incorrect.csv' )

