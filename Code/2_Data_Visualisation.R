##### Visualise Data
##### Daniel Dempsey

### Load libraries
library( readr )
library( dplyr )

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
#expected_responses <- sapply( q1_dat, function(x) {x$expected[1]} )
with_mab <- c( 1, 2, 2, 1, 2, 2, 1, 2, 1, 1, 0, 1, 1, 2, 0, 1 ) # Given by Valentina

#pdf( 'Experiment_1/Bar_All.pdf' )
for ( i in seq_along(q1_dat) ) {
  
  if ( i %in% c(11, 15) ) { next } # Spoiled responses
  pdf( paste0('Experiment_1/Bar_', i, '.pdf') )
  q_dat <- q1_dat[[i]]
  header <- ifelse( i == 1, 'Cannot-find-the-value-of-x question', q_dat$question[1] ) # To resolve issues from the apostrophe in original text
  plot( 0, 0, type = 'n', xaxt = 'n', yaxt = 'n', xlim = c(0.25, 2.35), bty = 'n',
        ylim = c(0, 1), main = header, xlab = '', ylab = 'Proportion' )
  res_tab <- table( q_dat$result ) / nrow(q_dat)
  names( res_tab ) <- switch( with_mab[i], c( 'With MAB', 'Without MAB' ), c( 'Without MAB', 'With MAB' ) )
  res_tab <- res_tab[ order( names(res_tab) ) ]
  barplot( res_tab, add = TRUE, border = mycol[2:1], col = mycol[2:1] )
  abline( h = seq(0, 1, 0.2), lty = 3, col = 'lightgrey' )
  dev.off()
  
}
#dev.off()

### Grouped plots for Valentina
group1 <- c( 3, 8, 9, 16 )
group2 <- c( 1, 2, 4, 12, 13, 14 )
group3 <- c( 5, 6, 7 )

# Group 1
pdf( 'Experiment_1/Group_1.pdf' )
par( mfrow = c(2, 2) )
for ( i in group1 ) {
  
  if ( i %in% c(11, 15) ) { next } # Spoiled responses
  q_dat <- q1_dat[[i]]
  header <- ifelse( i == 1, 'Cannot-find-the-value-of-x question', q_dat$question[1] ) # To resolve issues from the apostrophe in original text
  plot( 0, 0, type = 'n', xaxt = 'n', yaxt = 'n', xlim = c(0.25, 2.35), bty = 'n',
        ylim = c(0, 1), main = header, xlab = '', ylab = 'Proportion' )
  res_tab <- table( q_dat$result ) / nrow(q_dat)
  names( res_tab ) <- switch( with_mab[i], c( 'With MAB', 'Without MAB' ), c( 'Without MAB', 'With MAB' ) )
  res_tab <- res_tab[ order( names(res_tab) ) ]
  barplot( res_tab, add = TRUE, border = mycol[2:1], col = mycol[2:1] )
  abline( h = seq(0, 1, 0.2), lty = 3, col = 'lightgrey' )
  
}
dev.off()

# Group 2
pdf( 'Experiment_1/Group_2.pdf' )
par( mfrow = c(3, 3) )
for ( i in group2 ) {
  
  if ( i %in% c(11, 15) ) { next } # Spoiled responses
  q_dat <- q1_dat[[i]]
  header <- ifelse( i == 1, 'Cannot-find-the-value-of-x question', q_dat$question[1] ) # To resolve issues from the apostrophe in original text
  plot( 0, 0, type = 'n', xaxt = 'n', yaxt = 'n', xlim = c(0.25, 2.35), bty = 'n',
        ylim = c(0, 1), main = header, xlab = '', ylab = 'Proportion' )
  res_tab <- table( q_dat$result ) / nrow(q_dat)
  names( res_tab ) <- switch( with_mab[i], c( 'With MAB', 'Without MAB' ), c( 'Without MAB', 'With MAB' ) )
  res_tab <- res_tab[ order( names(res_tab) ) ]
  barplot( res_tab, add = TRUE, border = mycol[2:1], col = mycol[2:1] )
  abline( h = seq(0, 1, 0.2), lty = 3, col = 'lightgrey' )
  
}
dev.off()

# Group 3
pdf( 'Experiment_1/Group_3.pdf' )
par( mfrow = c(2, 2) )
for ( i in group3 ) {
  
  if ( i %in% c(11, 15) ) { next } # Spoiled responses
  q_dat <- q1_dat[[i]]
  header <- ifelse( i == 1, 'Cannot-find-the-value-of-x question', q_dat$question[1] ) # To resolve issues from the apostrophe in original text
  plot( 0, 0, type = 'n', xaxt = 'n', yaxt = 'n', xlim = c(0.25, 2.35), bty = 'n',
        ylim = c(0, 1), main = header, xlab = '', ylab = 'Proportion' )
  res_tab <- table( q_dat$result ) / nrow(q_dat)
  names( res_tab ) <- switch( with_mab[i], c( 'With MAB', 'Without MAB' ), c( 'Without MAB', 'With MAB' ) )
  res_tab <- res_tab[ order( names(res_tab) ) ]
  barplot( res_tab, add = TRUE, border = mycol[2:1], col = mycol[2:1] )
  abline( h = seq(0, 1, 0.2), lty = 3, col = 'lightgrey' )
  
}
dev.off()

### Individual plots
drop_ind <- !exp1_dat$corrupt
exp1_dat_trim <- exp1_dat[!(exp1_dat$question %in% c('Rhetorical question', 'Can’t-find-the-value-of-x question')), ]
ind1_wrong <- split( exp1_dat_trim$result[drop_ind] != exp1_dat_trim$expected[drop_ind], 
                     exp1_dat_trim$ID[drop_ind] )
count_wrong1 <- sapply( ind1_wrong, sum )

pdf( 'Experiment_1/Num_Incorrect.pdf' )
barplot( table( count_wrong1 ), col = mycol[3], yaxt = 'n', 
         ylim = c(0, 35), main = 'Number of Incorrect Answers for Experiment 1' )
axis( 2, at = seq(0, 35, 5) )
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
  
  bar <- boxplot( result ~ sub_question, data = q_dat, plot=FALSE )
  boxplot( result ~ sub_question, data = q_dat, outline=FALSE, main = main_question, 
           xlab = '', ylab = 'Score', border = use_col, col = mycol[9],
           ylim = c(0, 10) )
  points( jitter(bar$group), bar$out, col = use_col[bar$group] )
  #boxplot( result ~ sub_question, data = q_dat, main = main_question,
  #         xlab = '', ylab = 'Score', border = use_col, col = mycol[9],
  #         ylim = c(0, 10), pch = 20 )
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

### Examine data
### Merged visualisations

# Merge indices
merge1 <- c( 1, 3, 4, 6, 8, 11, 13, 14 )
merge2 <- c( 9, 12 )
merge3 <- c( merge1, 15 )

# Create merged datasets
type_fun <- function( x ) {
  type <- sub( '.*\n', '', x$question )
  mis_inds <- which( substr( type, 1, 12 ) == 'misalignment' )
  ra_inds <- which( substr( type, 1, 15 ) == 'right alignment' )
  LLP_inds <- which( substr( type, 1, 12 ) == 'alignment on' )
  type[mis_inds] <- 'misalignment'
  type[ra_inds] <- 'right alignment'
  type[LLP_inds] <- 'alignment on LLP che'
  x$type <- type
  x
}

q2_type <- lapply( q2_dat, type_fun )

merge_dat_1 <- do.call( 'rbind', q2_type[merge1] )
merge_dat_2 <- do.call( 'rbind', q2_type[merge2] )
merge_dat_3 <- do.call( 'rbind', q2_type[merge3] )

# Store IDs that are Valentina flagged as incorrect > 40% and 35% of the time
wrong_40 <- c( 48, 54, 55, 58, 60, 62, 64, 73, 74, 76, 77, 22, 80, 82, 93, 95, 
               104, 107, 111, 112, 114, 115, 117, 119 )
wrong_35 <- c( 48, 54, 55, 58, 60, 62, 64, 73, 74, 76, 77, 22, 80, 82, 93, 95, 
               104, 107, 111, 112, 114, 115, 117, 119, 53, 57, 61, 69, 70, 72, 
               75, 83, 85, 94, 100, 105, 113, 118, 121 )

# Plot merge dats as they are, and then removing incorrect IDs
plot_merged <- function(x, title) {
  bar <- boxplot( result ~ type, data = x, plot=FALSE )
  boxplot( result ~ type, data = x, outline=FALSE, main = title, 
           ylim=c(min(c(bar$stats ,bar$out)), max(c(bar$stats, bar$out))),
           ylab = 'Score', xlab = '' )
  points( jitter(bar$group), bar$out )
}

pdf( 'Experiment_2/Merged_Plots.pdf', width = 15 )
plot_merged( merge_dat_1, 'Merged Dataset 1' )
plot_merged( merge_dat_2, 'Merged Dataset 2' )
plot_merged( merge_dat_3, 'Merged Dataset 3' )
dev.off()

merged_dat_1_40 <- filter( merge_dat_1, !(ID %in% wrong_40) )
merged_dat_2_40 <- filter( merge_dat_2, !(ID %in% wrong_40) )
merged_dat_3_40 <- filter( merge_dat_3, !(ID %in% wrong_40) )

pdf( 'Experiment_2/Merged_Plots_Incorrect_40.pdf', width = 15 )
plot_merged( merged_dat_1_40, 'Merged Dataset 1 (more than 40% incorrect removed)' )
plot_merged( merged_dat_2_40, 'Merged Dataset 2 (more than 40% incorrect removed)' )
plot_merged( merged_dat_3_40, 'Merged Dataset 3 (more than 40% incorrect removed)' )
dev.off()

merged_dat_1_35 <- filter( merge_dat_1, !(ID %in% wrong_35) )
merged_dat_2_35 <- filter( merge_dat_2, !(ID %in% wrong_35) )
merged_dat_3_35 <- filter( merge_dat_3, !(ID %in% wrong_35) )

pdf( 'Experiment_2/Merged_Plots_Incorrect_35.pdf', width = 15 )
plot_merged( merged_dat_1_35, 'Merged Dataset 1 (more than 35% incorrect removed)' )
plot_merged( merged_dat_2_35, 'Merged Dataset 2 (more than 35% incorrect removed)' )
plot_merged( merged_dat_3_35, 'Merged Dataset 3 (more than 35% incorrect removed)' )
dev.off()

##### Experiment 3
### Missing data
exp3_dat[is.na( exp3_dat$result ), c('ID', 'question')] # No missing

### Barplots split by question
q3_dat <- split( exp3_dat, exp3_dat$question )

#pdf( 'Experiment_3/Bar_All.pdf' )
for ( i in seq_along(q3_dat) ) {
  
  pdf( paste0('Experiment_3/Bar_', i, '.pdf') )
  q_dat <- q3_dat[[i]]
  plot( 0, 0, type = 'n', xaxt = 'n', yaxt = 'n', xlim = c(0.25, 2.35), bty = 'n',
        ylim = c(0, 1), main = q_dat$question[1], xlab = '', ylab = 'Proportion' )
  use_col <- mycol[2:1]
  tab_res <- table( q_dat$result ) / nrow(q_dat)
  names( tab_res ) <- c( 'With MAB', 'Without MAB' )
  barplot( tab_res, add = TRUE, border = use_col, col = use_col )
  abline( h = seq(0, 1, 0.2), lty = 3, col = 'lightgrey' )
  dev.off()
  
}
#dev.off()

### Individual plots
ind3_wrong <- split( as.character(exp3_dat$result) != as.character(exp3_dat$expected), 
                     exp3_dat$ID )
count_wrong3 <- sapply( ind3_wrong, sum )

pdf( 'Experiment_3/Num_Incorrect.pdf' )
barplot( table( count_wrong3 ), col = mycol[3], yaxt = 'n', 
         ylim = c(0, 60), main = 'Number of Incorrect Answers for Experiment 3' )
axis( 2, at = seq(0, 60, 10) )
abline( h = seq(0, 60, 10), lty = 3, col = 'lightgrey' )
dev.off()

### Grouped plots for Valentina
group1 <- 1:5
group2 <- 2:5

# Group 1
pdf( 'Experiment_3/Group_1.pdf' )
par( mfrow = c(3, 3) )
for ( i in group1 ) {
  
  q_dat <- q3_dat[[i]]
  plot( 0, 0, type = 'n', xaxt = 'n', yaxt = 'n', xlim = c(0.25, 2.35), bty = 'n',
        ylim = c(0, 1), main = q_dat$question[1], xlab = '', ylab = 'Proportion' )
  use_col <- mycol[2:1]
  tab_res <- table( q_dat$result ) / nrow(q_dat)
  names( tab_res ) <- c( 'With MAB', 'Without MAB' )
  barplot( tab_res, add = TRUE, border = use_col, col = use_col )
  abline( h = seq(0, 1, 0.2), lty = 3, col = 'lightgrey' )
  
}
dev.off()

# Group 2
pdf( 'Experiment_3/Group_2.pdf' )
par( mfrow = c(2, 2) )
for ( i in group2 ) {
  
  q_dat <- q3_dat[[i]]
  plot( 0, 0, type = 'n', xaxt = 'n', yaxt = 'n', xlim = c(0.25, 2.35), bty = 'n',
        ylim = c(0, 1), main = q_dat$question[1], xlab = '', ylab = 'Proportion' )
  use_col <- mycol[2:1]
  tab_res <- table( q_dat$result ) / nrow(q_dat)
  names( tab_res ) <- c( 'With MAB', 'Without MAB' )
  barplot( tab_res, add = TRUE, border = use_col, col = use_col )
  abline( h = seq(0, 1, 0.2), lty = 3, col = 'lightgrey' )
  
}
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

