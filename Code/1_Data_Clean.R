##### Read in and Clean Data
##### Daniel Dempsey

### Set working directory and load libraries
setwd( 'Data' )
library( readr )
library( tidyr )

### Read in data
# First read in and store the experiment type column indices
mab_dat_type <- read.csv( 'Raw_Data/data_FINAL cleaned.csv', nrows = 1, header = FALSE )
col_sep_index <- c( which( !is.na(mab_dat_type) ), ncol(mab_dat_type)+1 )
col_seq_extract <- Map( function(x, y) { c(1, y:(x-1)) }, 
                        y = col_sep_index[-4], x = col_sep_index[-1] )

# Read in each experiment and store into different files
mab_read <- function( x, y, z = Inf ) {
  read_csv( 'Raw_Data/data_FINAL cleaned.csv', skip = y, 
            col_select = x, n_max = z, na = 'MISSING DATA' )
}

mab_dat <- Map( mab_read, x = col_seq_extract, y = 1, z = 145 )

### Convert from wide format to long
mab_dat_long <- lapply( mab_dat, pivot_longer, cols = !`Participant \nnumber`, 
                        names_to = 'question', values_to = 'result' )

# Fix ID column names
mab_dat_long <- lapply( mab_dat_long, function(x) { colnames(x)[1] <- 'ID';x } )

### Append Expected Value data
# Prepare the expected data
mab_expected <- Map( mab_read, x = col_seq_extract, y = 146, z = 1 )
num_participants <- length( unique(mab_dat_long[[1]]$ID) )
mab_expected_extended <- Map( function(x, y) { rep(as.matrix(x)[-1], y) }, 
                              x = mab_expected, y = num_participants )

# Append expected values
mab_dat_long_expected <- Map( function(x, y) { cbind(x, expected = y) }, 
                              mab_dat_long, mab_expected_extended )

### Specify question groupings in second experiment
# Find the groupings
question_dat <- split( mab_dat_long_expected[[2]]$question, 
                       mab_dat_long_expected[[2]]$ID )[[1]]
groups <- sub( "\\\n.*", "", question_dat )
group_labels <- rep( cumsum( !( duplicated( groups ) ) ), num_participants )

# Append the groupings
mab_dat_long_expected[[2]] <- cbind( mab_dat_long_expected[[2]], 
                                     groups = group_labels )

### Apply corrections that were listed in the comments of the raw data set
### Experiment 1 corrections:
# 1: "[Unheard echo question \n(wh-to-LLP/buone)] Gesture here was not performed properly. We think that this polluted the data."
# Apply a 'corrupt' label for the data
corrupt <- rep( FALSE, nrow(mab_dat_long_expected[[1]]) )
corrupt[ which(mab_dat_long_expected[[1]]$question == 'Unheard echo question \n(wh-to-LLP/buone)') ] <- TRUE
mab_dat_long_expected[[1]] <- cbind( mab_dat_long_expected[[1]], corrupt = corrupt )

# 2: "[Exclamative] Participants 3, 10 and 29 should have ‘Frase 1’ not ‘Frase 2’ on the basis of their comments. We think it is a fieldworker’s mistake."
# Alter the data accordingly
change_ind <- ( mab_dat_long_expected[[1]]$ID %in% c( 3, 10, 29 ) ) & 
  ( mab_dat_long_expected[[1]]$question == 'Exclamative' )
mab_dat_long_expected[[1]]$result[change_ind] <- 'Frase 1'

### Experiment 2 corrections
# 1: [Disapproval echo question (wh-to-LLP/tutto)\nspeech+gesture] Participants 15 and 39 in column BB should both have 10 and not 0. We think it is a fieldworker’s mistake.
# Alter the data accordingly
change_ind <- ( mab_dat_long_expected[[2]]$ID %in% c( 15, 39 ) ) & 
  ( mab_dat_long_expected[[2]]$question == 'Disapproval echo question (wh-to-LLP/tutto)\nspeech+gesture' )
mab_dat_long_expected[[2]]$result[change_ind] <- 10

# 2: [Unheard echo question\nspeech+gesture] Here the gesture was somewhat wrongly performed as in column I and it might have polluted the data.
# Apply a 'corrupt' label for the data
corrupt <- rep( FALSE, nrow(mab_dat_long_expected[[2]]) )
corrupt[ which(mab_dat_long_expected[[2]]$question == 'Unheard echo question\nspeech+gesture') ] <- TRUE
mab_dat_long_expected[[2]] <- cbind( mab_dat_long_expected[[2]], corrupt = corrupt )

# 3: [Unheard echo question\nche LLP & Unheard echo question\nright alignment--whole question] From the comment we think that the fieldworker inverted participant 59 BF with BG and participant 113 BF with BG.
# Swap values
swap_ind1 <- ( mab_dat_long_expected[[2]]$ID %in% c( 59, 113 ) ) & 
  ( mab_dat_long_expected[[2]]$question == 'Unheard echo question\nche LLP' )
swap_ind2 <- ( mab_dat_long_expected[[2]]$ID %in% c( 59, 113 ) ) & 
  ( mab_dat_long_expected[[2]]$question == 'Unheard echo question\nright alignment--whole question' )
val1 <- mab_dat_long_expected[[2]]$result[swap_ind1]
val2 <- mab_dat_long_expected[[2]]$result[swap_ind2]
mab_dat_long_expected[[2]]$result[swap_ind2] <- val1
mab_dat_long_expected[[2]]$result[swap_ind1] <- val2

# 4: [Surprise-disapproval question\ngesture+speech & Surprise-disapproval question\nright alignment] N.B. We think that the fieldworker inverted BJ with BI for participant 14.
# Swap values
swap_ind1 <- ( mab_dat_long_expected[[2]]$ID == 14 ) & 
  ( mab_dat_long_expected[[2]]$question == 'Surprise-disapproval question\ngesture+speech' )
swap_ind2 <- ( mab_dat_long_expected[[2]]$ID == 14 ) & 
  ( mab_dat_long_expected[[2]]$question == 'Surprise-disapproval question\nright alignment' )
val1 <- mab_dat_long_expected[[2]]$result[swap_ind1]
val2 <- mab_dat_long_expected[[2]]$result[swap_ind2]
mab_dat_long_expected[[2]]$result[swap_ind2] <- val1
mab_dat_long_expected[[2]]$result[swap_ind1] <- val2

### No Experiment 3 corrections

### Write csv files of new processed data
filenames <- paste0( 'Processed_Data/Exp', 1:3, '_dat.csv' )
Map( write_csv, x = mab_dat_long_expected, file = filenames )

