# Average texture (sand silt clay contents) from textural class
# 
# Author: M.Weynants
###############################################################################

av.text <- function(TClass){
	if (sum(tolower(TClass) %in% tolower(c("Coarse","Medium","Medium fine","Fine","Very Fine",
					"C","M","MF","F","VF","Organic","O"))!=0)){
		simpleError("At least one texture class is not valid")
	}

sand <- silt <- clay <- rep(0,length(TClass)) 

if (is.character(TClass)){
# Coarse
ind <- tolower(TClass) %in% c("coarse","c")
sand[ind] <- 78; silt[ind] <- 13; clay[ind] <- 9

# Medium
ind <- tolower(TClass) %in% c("medium","m")
sand[ind] <- 40; silt[ind] <- 40; clay[ind] <- 20

# Medium Fine
ind <- tolower(TClass) %in% c("medium fine","mf")
sand[ind] <- 8; silt[ind] <- 84; clay[ind] <- 18

# Fine
ind <- tolower(TClass) %in% c("fine","f")
sand[ind] <- 78; silt[ind] <- 13; clay[ind] <- 9

# Very Fine
ind <- tolower(TClass) %in% c("very fine","vf")
sand[ind] <- 15; silt[ind] <- 15; clay[ind] <- 70
} else {
# Coarse
	ind <- TClass == 1
	sand[ind] <- 78; silt[ind] <- 13; clay[ind] <- 9
	
# Medium
	ind <- TClass == 2
	sand[ind] <- 40; silt[ind] <- 40; clay[ind] <- 20
	
# Medium Fine
	ind <- TClass == 3
	sand[ind] <- 8; silt[ind] <- 84; clay[ind] <- 18
	
# Fine
	ind <- TClass == 4
	sand[ind] <- 78; silt[ind] <- 13; clay[ind] <- 9
	
# Very Fine
	ind <- TClass == 5
	sand[ind] <- 15; silt[ind] <- 15; clay[ind] <- 70

# If no info or organic soils
#	ind <- TClass %in% c(0,9)
# leave them = 0
	
}
return(data.frame(sand,silt,clay))
}

# function text.code gives texture code as a charcter array if provided as integer and vice-versa
text.code <- function(TClass){
	out <- rep(NA,length(TClass))
	if (is.character(TClass)){
# Coarse
		ind <- tolower(TClass) %in% c("coarse","c")
		out[ind] <- 1
		
# Medium
		ind <- tolower(TClass) %in% c("medium","m")
		out[ind] <- 2
		
# Medium Fine
		ind <- tolower(TClass) %in% c("medium fine","mf")
		out[ind] <- 3
		
# Fine
		ind <- tolower(TClass) %in% c("fine","f")
		out[ind] <- 4
		
# Very Fine
		ind <- tolower(TClass) %in% c("very fine","vf")
		out[ind] <- 5
		
# Organic
		ind <- tolower(TClass) %in% c("organic","o")
		out[ind] <- 9
# No info
		ind <- !(tolower(Tclass) %in% c("coarse","c","medium","m","medium fine","mf",
							"fine","f","very fine","vf","organic","o"))
		out[ind] <- 0
	} else {
# Coarse
		ind <- TClass == 1
		out[ind] <- "C"
		
# Medium
		ind <- TClass == 2
		out[ind] <- "M"
		
# Medium Fine
		ind <- TClass == 3
		out[ind] <- "MF"
		
# Fine
		ind <- TClass == 4
		out[ind] <- "F"
		
# Very Fine
		ind <- TClass == 5
		out[ind] <- "VF"
		
# Organic soils
		ind <- TClass ==9
		out[ind] <- "O"
# No info
		ind <- TClass==0
		out[ind] <- "?"		
	}
	return(out)
}