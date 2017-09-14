#' Assign schools to a synthetic population.
#' 
#' @param people data frame of synthetic people
#' @param schools list with public and private school data frames 
#' @param weightSchools Function to weight the schools 
#' @param distFun Function to compute distance between schools and agents 
#' 
#' @references See PUMS CODEBOOK:  http://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2010-2014.pdf
#' @return a column of school assignments by school ID 
assign_schools <- function(people, schools, weightSchools = weight_dists, distFun = haversine) {
  # Verify we have the necessary variables for assigning 
  impt_vars <- c("latitude", "longitude", "SCH", "SCHG", "AGEP", "place_id")
  stopifnot(all(impt_vars %in% colnames(people)))
  people <- people[, impt_vars]
  
  # Verify the school and age variables are numerics 
  people$SCH <- as.numeric(as.character(people$SCH))
  people$SCHG <- as.numeric(as.character(people$SCHG))
  people$AGEP <- as.numeric(as.character(people$AGEP))
  
  # Create new variables for age, grade, county, and state. 
  # School Enrollment: 1: NO, 2: PUBLIC, 3: PRIVATE 
  # Grade: 01-14. Pre-Kindergarten-12th grade. 15-16 (College)
  # Lee: Removed age restriction, there seems to be no limit on 
  # how old you must be to enter high school 
  people$age <- ifelse(people$AGEP >= 3, 1, 0)
  people$grade <- ifelse(people$SCHG >= 1 & people$SCHG <= 14, people$SCHG , 0)
  people$co <- substr(people$place_id, 3, 5)
  people$st <- substr(people$place_id, 1, 2)
  
  # Assign the people to schools, as necessary. Note that to run ddply, 
  # we need to have the data-frame ordered. To solve this, we preserve 
  # the original ordering and return the assigned ID's in this order.  
  people_ord <- with(people, order(SCH, SCHG, grade, age, co))
  original_order <- order(people_ord)
  people <- people[people_ord, ]
  
  school_assignments <- plyr::ddply(people, .variables = c('SCH', 'SCHG', 'grade', 'age', 'co'), 
                              .fun = assign_schools_inner, schools = schools, 
                              weightSchools = weightSchools, distFun = distFun)
  school_ids <- school_assignments$ids[original_order]
  
  return(school_ids)
}

#' Function which assigns schools 
#' @param df subset of people split so all age, grade, SCH, and county should be the same in the df
#' @param schools list of schools, one data frame of private and one of public 
#' @param weightSchools Function to weight the schools 
#' @param distFun Function to compute distance between schools and agents 
#' 
#' @return column of school ID assignments
assign_schools_inner <- function(df, schools, weightSchools, distFun) {
  # Check for the conditions of a school age child 
  if (df$SCH[1] < 2 | df$age[1] == 0 | df$grade[1] < 1 | df$grade[1] > 14 | 
        is.na(df$SCH[1]) | is.na(df$SCHG[1])| is.na(df$age[1]) | 
        is.na(df$grade[1])) {
    
    # If not a school age child, then we return NA
    ids <- rep(NA, nrow(df))
  } else {
    # Subset schools with appropriate schools and grade, get 
    # the physical distance between schools and child, and 
    # generate probabilities for each child attending a school 
    # using a combination of distance and school size.
    schools_sub <- subset_schools(df, schools)
    dist_mat <- get_dists(df, schools_sub, dist = distFun)
    weight_mat <- weightSchools(dist_mat, schools_sub)
    
    # Use these weights to assign schools 
    school_inds <- apply(weight_mat, 1, function(row) sample(1:nrow(schools_sub), size = 1, prob = row))
    ids <- as.character(schools_sub[school_inds, 'ID'])
  }
  stopifnot(length(ids) == nrow(df))
  return(data.frame(ids = ids, stringsAsFactors = FALSE))
}

#' Weight school assignment probabilities  
#'
#' @param dist_mat a m x n matrix where m is the number of people and 
#' n is the number of schools
#' @param schools data frame of schools
#' @return m x n matrix of probabilities.  Each row should sum to 1
weight_dists <- function(dist_mat, schools){
  m <- nrow(dist_mat)
  n <- ncol(dist_mat)
  
  students <- as.numeric(as.character(schools$Students))
  stopifnot(length(students) == n)
  
  student_weight <- ceiling(students/100)
  student_weight <- ifelse(is.na(student_weight), 1, student_weight)
  
  weights <- t(t((1-dist_mat)) * 1000 * student_weight)
  weights <- weights / rowSums(weights)
  weights <- ifelse(is.na(weights), 0.0001, weights)
  weights <- weights / rowSums(weights)
  stopifnot(dim(weights) == c(m,n))
  return(weights)
}


#' Weight school assignment probabilities  
#'
#' @param dist_mat a m x n matrix where m is the number of people and 
#' n is the number of schools
#' @param schools data frame of schools
#' @return m x n matrix of probabilities.  Each row should sum to 1
weight_dists2 <- function(dist_mat, schools){
  m <- nrow(dist_mat)
  n <- ncol(dist_mat)
  
  students <- as.numeric(as.character(schools$Students))
  stopifnot(length(students) == n)
  
  student_weight <- ceiling(students / 50)
  student_weight <- ifelse(is.na(student_weight), 1, student_weight)
  weights <-  exp( 1 + 1 / (dist_mat ))
  weights <- t(t(weights)  * student_weight)
  weights <- weights / rowSums(weights)
  weights <- ifelse(is.na(weights), 0.0001, weights)
  weights <- weights / rowSums(weights)
  stopifnot(dim(weights) == c(m,n))
  return(weights)
}

#' Weight school assignment probabilities, distance only
#'
#' @param dist_mat a m x n matrix where m is the number of people and 
#' n is the number of schools
#' @param schools data frame of schools
#' @return m x n matrix of probabilities.  Each row should sum to 1
weight_dists_D <- function(dist_mat, schools) {
  m <- nrow(dist_mat)
  n <- ncol(dist_mat)
  weights <-  exp( 1 + 1 / (dist_mat / 20 ))
  weights <- weights / rowSums(weights)
  weights <- ifelse(is.na(weights), 0.0001, weights)
  weights <- weights / rowSums(weights)
  
  stopifnot(dim(weights) == c(m,n))
  return(weights)
}

#' Weight school assignment probabilities by capacity only
#'
#' @param dist_mat a m x n matrix where m is the number of people and 
#' n is the number of schools
#' @param schools data frame of schools
#' @return m x n matrix of probabilities.  Each row should sum to 1
weight_dists_C <- function(dist_mat, schools){
  m <- nrow(dist_mat)
  n <- ncol(dist_mat)
  
  students <- as.numeric(as.character(schools$Students))
  stopifnot(length(students) == n)
  
  student_weight <- students
  student_weight <- ifelse(is.na(student_weight), 1, student_weight)
  student_weight <- exp(student_weight)
  
  weights <- matrix(students, nrow = m, ncol = n, byrow = TRUE)
  weights <- weights/rowSums(weights)
  weights <- ifelse(is.na(weights), 0.0001, weights)
  stopifnot(dim(weights) == c(m,n))
  return(weights)
}

#' Get the distances between the schools and the people.  
#'
#' @param df data frame of people
#' @param schools dataframe of subsetted schools
#' @param dist "haversine"
#' @return distance between two points on the globe
get_dists <- function(df, schools, dist){
  m <- nrow(df) # number of people
  n <- nrow(schools) # number of schools
  
  # Lee: I'm guessing this check if there is 
  # private schools or public. Let's make this 
  # explicit in the code.
  # SKG:  It's supposed to be even more generic than that.  
  # If we have lat/lon, then use that.  if not, then just use the 
  # schools in the county (or equivalent).
  if (!any(grepl("Lat", colnames(schools)))){
    return(matrix(0, nrow = m, ncol = n))
  }
  
  # Lee: We should explain what's happening here, hard to parse
  # SKG: We extract the x and y coordinates of m the people and the n schools.  
  # We then find the haversine distance (distance on a great sphere) between each pair of people and schools.  
  # The people are the rows and the schools are the columns after we transpose due to a quirk in the 'apply' function.
  dist_mat <- apply(df, 1, function(row){
    x1 <- as.numeric(as.character(rep(row['longitude'], n)))
    stopifnot(!is.null(row['longitude']))
    y1 <- as.numeric(as.character(rep(row['latitude'], n)))
    x2 <- as.numeric(as.character(schools$Long))
    y2 <- as.numeric(as.character(schools$Lat))
    dists <- dist(x1, y1, x2, y2)
    return(dists)
  })
  
  if (m == 1) { # seems mismatched because we transpose
    dist_mat <- matrix(dist_mat, ncol=1)
  } else if (n == 1) {
    dist_mat <- matrix(dist_mat, nrow=1)
  }
  stopifnot(dim(t(dist_mat)) == c(m,n))
  return(t(dist_mat))
}

#' Get the haversine distance between two points (x1, y1) and (x2, y2) scaled between 0 and 1.
#' 
#' @param x1 longitude of object 1 (vector)
#' @param y1 latitude of object 1 (vector)
#' @param x2 longitude of object 2 (vector)
#' @param y2 latitude of object 2 (vector)
#' @references http://andrew.hedges.name/experiments/haversine/
#' @return numeric 
haversine <- function(x1, y1, x2, y2){
  dx <- x2 - x1 
  dy <- y2 - y1 
  a <- (sin(dy/2))^2 + cos(y1) * cos(y2) * (sin(dx/2))^2
  d <- atan2( sqrt(a), sqrt(1-a))
  d <- (d / pi)  # scales between 0 and 1
  stopifnot( all(d >= 0 ))
  stopifnot( all( d <= 1))
  return(d)
}

#' Subset the schools to that of the county
#' 
#' @param df subset of people split so all age, grade, 
#' SCH, and county should be the same in the df
#' @param schools list of schools, one data frame of private 
#' and one of public
#' @return dataframe of subsetted schools to the county and public 
#' or private
subset_schools <- function(df, schools){
  sch <- df$SCH[1]
  co <- df$co[1]
  st <- df$st[1]
  grade <- df$SCHG[1] - 2
  stopifnot(sch %in% c(2,3)) #2 is public, 3 is private
  school_type <- ifelse(sch == 2, "public", "private")
  school <- schools[school_type][[1]]

  
  # TODO:  FIX formatting of state or county number of schools
  # have to at least be in the right state
  school <- school[as.numeric(as.character(school$StNo)) == as.numeric(st),]
  
  # Lee: Let's try to keep individual lines under 80 characters. This one 
  # is too long and hard to understand 
  inds <- which(as.numeric(as.character(school$CoNo)) == as.numeric(co) & 
                  as.numeric(as.character(school$StNo)) == as.numeric(st))
  
  school_sub <- school[inds, ]
  grade_inds <- which(school_sub$Low <= grade & school_sub$High >= grade | 
                        school_sub$Low == -2 | school_sub$High == -2)
  
  if (length(grade_inds) == 0) {
    if (nrow(school_sub) == 0) {
      school_sub <- school 
    }
    stopifnot(nrow(school_sub) > 0)
    return(school_sub)
  } else {
    return(school_sub[grade_inds,])
  }
}
