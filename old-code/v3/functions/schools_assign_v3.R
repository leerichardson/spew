# Shannon Gallagher August 5, 2014 Experimenting with school data

# NOTE: only works within a state, and preferably within a PUMA or county

# Data from ELSI http://nces.ed.gov/ccd/elsi/tableGenerator.aspx Have to
# cut off last 5 rows of the public schools;

# There are 4,993,750 students enrolled in private school in the US There
# are 49,177,617 students enrolled in public school There are 54,171,367
# students total in the US

assign.schools <- function(syn.p, schools.pub, schools.priv = F, counts.priv, 
                           counts.pub) {
  # FUNCTION: USE RTI method in QUICKSTART V1 to assign a public school ID
  # where appropriate INPUT: synthetic population data frame, public
  # schools data frame, private school counts data frame, public school
  # enrollment counts data frame OUTPUT: list of synthetic population data
  # frame with an extra column, the school ID; updated counts data frames
  # of private schools and public schools get indices of public school,
  # private school, and NA people
  enrollment.inds <- school.type(syn.p$SCH)
  
  # get coordinates of people and schools
  coords.p <- subset(syn.p, select = c(Lat, Long))
  coords.s.pub <- subset(schools.pub, select = c(Lat, Long))
  coords.s.priv <- F
  colnames(coords.s.pub) <- c("Lat", "Long")
  
  # assign school ID (or NA)
  syn.p$SCHOOL_ID <- assign.schools.inner(enrollment.inds, syn.p$SCHG, 
                                          coords.p, coords.s.pub, coords.s.priv, counts.priv, counts.pub, syn.p$geoLevel1)
  return(syn.p$SCHOOL_ID)
}

school.type <- function(SCH) {
  # FUNCTION: separates indices into groups of no school, public school,
  # and private school INPUT: SCH codes (vector) (from 2011 PUMS person
  # data) OUTPUT: list of separated indices
  no.school <- which(SCH %in% c(NA, 1))
  pub.school <- which(SCH == 2)
  priv.school <- which(SCH == 3)
  return(list(no.school = no.school, pub.school = pub.school, priv.school = priv.school))
}

assign.schools.inner <- function(enrollment.inds, SCHG, coords.p, coords.s.pub, 
                                 coords.s.priv, counts.priv, counts.pub, p.COUNTY) {
  # FUNCTION: Assigns proper indices a school ID or NA q INPUT:
  # enrollment.inds (list); syn.p$SCHG (vector); coords.p, coords.s.pub,
  # coords.s.priv (data frames); school.id.pub, school.id.priv (vector);
  # counts.priv, counts.pub (data frames) OUTPUT: school assignments
  # (character vector with NA values)
  school.as <- character(length(SCHG))
  school.as[enrollment.inds$no] <- NA
  if (length(enrollment.inds$pub) != 0) {
    school.as[enrollment.inds$pub] <- assign.pub(SCHG[enrollment.inds$pub], 
                                                 coords.p[enrollment.inds$pub, ], coords.s.pub, counts.pub, p.COUNTY[enrollment.inds$pub])
  }
  if (length(enrollment.inds$priv) != 0) {
    school.as[enrollment.inds$priv] <- assign.priv(SCHG[enrollment.inds$priv], 
                                                   coords.p[enrollment.inds$priv, ], coords.s.priv, counts.priv, 
                                                   p.COUNTY[enrollment.inds$priv])
  }
  return(school.as)
}

assign.pub <- function(SCHG, coords.p, coords.s, counts.pub, county.no.p) {
  # FUNCTION: function that helps assign a public school to a person INPUT:
  # SCHG (vector), coords.p (2 col data frame), coords.s (2 col data
  # frame), counts pub( 3 col data frame- school ID, school tot enrollment,
  # current school count) OUTPUT: Vector of public school assignments
  # (currently U (undergrad), G (grad), NA, or School ID)
  school.id <- character(length(SCHG))
  non.reg.grades.inds <- which(SCHG %in% c(NA, 6, 7))
  reg.grades <- which(SCHG %in% c(1, 2, 3, 4, 5))
  # function to assign college and other grades
  if (length(non.reg.grades.inds) != 0) {
    school.id[non.reg.grades.inds] <- sapply(SCHG[non.reg.grades.inds], 
                                             college.plus)
  }
  if (length(reg.grades) != 0) {
    school.id[reg.grades] <- pub.assign.inner(SCHG[reg.grades], coords.p[reg.grades, 
                                                                         ], coords.s, counts.pub, county.no.p)
  }
  return(school.id)
}



college.plus <- function(SCHG) {
  # Function: Assigns an ID for non regular grades INPUT: one SCHG codes
  # OUTPUT: One ID (NA, U, or G)
  if (SCHG == 6) {
    id <- "U"
  } else if (SCHG == 7) {
    id <- "G"
  } else {
    id <- NA
  }
  return(id)
}

pub.assign.inner <- function(SCHG, coords.p, coords.s, counts.pub, county.no.p) {
  id <- character(length(SCHG))
  test.dist <- character(length(SCHG))
  counts.pub$counts <- 0
  for (i in 1:length(SCHG)) {
    cur.co <- as.integer(as.character(county.no.p[i]))
    person.coords <- unlist(coords.p[i, ])
    counts.pub$County <- as.character(counts.pub$County)
    counts.pub$County <- gsub("=", "", counts.pub$County)
    if (nchar(counts.pub$County[1]) == 5) {
      start.ind <- 3
      stop.ind <- 5
    } else {
      start.ind <- 2
      stop.ind <- 4
    }
    counts.pub$co <- substr(counts.pub$County, start.ind, stop.ind)
    sub.bin <- (as.integer(counts.pub$co) == as.integer(cur.co))
    sub.bin <- ifelse(is.na(sub.bin), FALSE, sub.bin)
    app.school.bins <- get.app.school.bin(SCHG[i], counts.pub)
    app.school.bins <- ifelse(is.na(app.school.bins), FALSE, app.school.bins)
    counts.pub$Tot <- as.integer(as.character(counts.pub$Tot))
    counts.pub$counts <- as.integer(as.character(counts.pub$Tot))
    can.enroll.bins <- ifelse(counts.pub$Tot > counts.pub$counts, T, 
                              F)
    
    can.enroll.bins <- T  #ATTN:  fix this #ifelse(is.na(can.enroll.bins), FALSE, can.enroll.bins)
    bins <- (sub.bin & app.school.bins & can.enroll.bins)
    bins <- ifelse(is.na(bins), FALSE, bins)
    
    # subset schools to choose from, must be of appropriate county and grade
    sub.coords.s <- subset(coords.s, bins)
    sub.counts.pub <- subset(counts.pub, bins)
    # get haversine distance
    dists <- sapply(sub.coords.s, haversine, coords2 = person.coords)
    dist.ord <- order(unlist(dists))
    smallest.dist.inds <- which(dist.ord <= 3)
    # sample from the 3 indices
    id.ind <- try(sample(smallest.dist.inds, 1))
    id[i] <- sub.counts.pub$School.ID[id.ind]
    count.ind <- which(counts.pub$School.ID == id[i])
    counts.pub$counts[count.ind] <- counts.pub$counts[count.ind] + 1
  }
  return(id)
}

priv.assign.inner <- function(SCHG, coords.p, coords.s, counts.priv, county.no.p) {
  counts.priv$counts <- 0
  id <- character(length(SCHG))
  for (i in 1:length(SCHG)) {
    cur.co <- as.integer(county.no.p[i])
    counts.priv$County = gsub("=", "", counts.priv$County)
    sub.bin <- (as.integer(substr(as.character(counts.priv$County), 3, 
                                  5)) == cur.co)
    sub.bin <- ifelse(is.na(sub.bin), FALSE, sub.bin)
    app.school.bins <- get.app.school.bin(SCHG[i], counts.priv)
    app.school.bins <- ifelse(is.na(app.school.bins), FALSE, app.school.bins)
    can.enroll.bins <- T  #ATTN:  needs fixed ifelse(counts.priv$Tot> counts.priv$counts, T, F)
    bins <- (sub.bin & app.school.bins & can.enroll.bins)
    bins <- ifelse(is.na(bins), FALSE, bins)
    if (sum(bins) == 0) {
      bins <- sub.bin
    }
    
    sub.counts.priv <- subset(counts.priv, bins)
    cur.id <- try(sample(sub.counts.priv$Private.School.Name, 1))
    if (class(cur.id) == "try-error") {
      id[i] <- NA
    } else {
      id[i] <- cur.id
      count.ind <- which(counts.priv$Private.School.Name == id[i])
      counts.priv$counts[count.ind] <- counts.priv$counts[count.ind] + 
        1
    }
  }
  return(id)
}

assign.priv <- function(SCHG, coords.p, coords.s, counts.priv, county.no.p) {
  # FUNCTION: function that helps assign a private school to a person
  # INPUT: SCHG (vector), coords.p (2 col data frame), coords.s (F), counts
  # priv( 3 col data frame- school ID, school tot enrollment, current
  # school count) OUTPUT: Vector of private school assignments (currently U
  # (undergrad), G (grad), NA, or School ID)
  school.id <- character(length(SCHG))
  non.reg.grades.inds <- which(SCHG %in% c(NA, 6, 7))
  reg.grades <- which(SCHG %in% c(1, 2, 3, 4, 5))
  # function to assign college and other grades
  if (length(non.reg.grades.inds) != 0) {
    school.id[non.reg.grades.inds] <- sapply(SCHG[non.reg.grades.inds], 
                                             college.plus)
  }
  if (length(reg.grades) != 0) {
    school.id[reg.grades] <- priv.assign.inner(SCHG[reg.grades], coords.p[reg.grades, 
                                                                          ], coords.s, counts.priv, county.no.p)
  }
  return(school.id)
  
  
  
}

haversine <- function(coords1, coords2) {
  # FUNCTION: find the Haversine distance between a pair of points (i.e.,
  # distance over earth's surface) Source:
  # http://www.movable-type.co.uk/scripts/latlong.html INPUT: ind1 integer;
  # coords1- 2 column data frame, first col is latitude, second col is
  # long, coords2- 2 element vectors first is latitude, second is longitude
  # OUTPUT: haversine distance between coords1 and coords2
  x1 <- as.numeric(as.character(coords1[1]))
  y1 <- as.numeric(as.character(coords1[2]))
  x2 <- as.numeric(as.character(coords2[1]))
  y2 <- as.numeric(as.character(coords2[2]))
  a <- sin((x2 - x1)/2)^2 + cos(x1) * cos(x2) * sin((y2 - y1)/2)^2
  b <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6371  #mean radius of earth in km
  d <- R * b
  return(d)
}

get.app.school.bin <- function(grade.code, counts.pub) {
  # FUNCTION: get the binaries of the appropriate school levels INPUT:
  # grade.code (integer), counts.pub data frame with highest and lowest
  # grade OUTPUT: VECTOR of binaries, length of counts.pub # of rows
  if (grade.code == 1) {
    bins <- ifelse(counts.pub$Low <= -1, T, F)
  } else if (grade.code == 2) {
    bins <- ifelse(counts.pub$Low <= 0 & counts.pub$High >= 0, T, F)
  } else if (grade.code == 3) {
    bins <- ifelse(counts.pub$Low <= 4 & counts.pub$High >= 1, T, F)
  } else if (grade.code == 4) {
    bins <- ifelse(counts.pub$Low <= 8 & counts.pub$High >= 5, T, F)
  } else if (grade.code == 5) {
    bins <- ifelse(counts.pub$High >= 12, T, F)
  }
  return(bins)
}

# FUNCTION: high level school assigner INPUTS: OUTPUTS: REQUIREMENTS:

getSchool <- function(syn.p, schools.pub, schools.priv) {
  counts.pub <- subset(schools.pub, select = c("School.ID", "Total.Students", 
                                               "County.Number", "Lowest.Grade", "Highest.Grade"))
  counts.priv <- subset(schools.priv, select = c("Private.School.Name", 
                                                 "Total.Students", "County.Code", "Lowest.Grade", "Highest.Grade"))
  schoolIDs <- assign.schools(syn.p, schools.pub, schools.priv, counts.priv, 
                              counts.pub)
  return(schoolIDs)
} 
