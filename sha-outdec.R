# This file does the shape outleir deteciton

# This function is a sorter
bully <- function(list, order) {

    sorted_list <- c()
    counter <- 1

    for (i in order) {

        value <- list[i]

        sorted_list[[counter]] <- value

        counter <- counter + 1
    }

    return(unlist(sorted_list))
}

shape_outdec <- function(mts) {

    sha_result <- outlier_detection(mts$data)

    # Apply the outlying order to the time stamps
    ordered_dates <- bully(as.character(mts$time), sha_result$Indexes)

    sha_depth <- sha_result$Depths
    names(sha_depth) <- ordered_dates

    return(sha_depth)

}