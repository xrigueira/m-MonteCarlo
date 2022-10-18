# This file calculates the final depth, which is a combination
# of the magnitud depth and the shape depth

global_outdec <- function(mts, shape_depth, magnitude_depth) {
    
    # Get the time stamps as character
    time_stamps <- as.character(mts$time)
    
    ms_depth <- c()
    counter <- 1
    for (i in time_stamps) {

        sha_depth <- shape_depth[[i]]
        mag_depth <- magnitude_depth[[i]]

        # Here is where we can make a few tweaks to change the weight of each depth
        tot_depth <- mag_depth + sha_depth

        ms_depth[[counter]] <- tot_depth

        counter <- counter + 1

    }

    # Add the names and sort the list of depths
    names(ms_depth) <- time_stamps
    ms_depth <- sort(unlist(ms_depth))

    return(ms_depth)

}
