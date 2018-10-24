# welltrue('A1a1') = 1
# welltrue('D12h8') = 3072 = 4*12*8*8
welltrue <- function(wellName){
  bigRow <- gsub('([a-zA-Z])+.*','\\1',wellName);
  wellName <- chopLeft(wellName,nchar(bigRow));
  bigCol <- gsub('(\\d+).*','\\1',wellName);
  wellName <- chopLeft(wellName,nchar(bigCol));
  smlRow <- gsub('([a-zA-Z]+).*','\\1',wellName);
  wellName <- chopLeft(wellName,nchar(smlRow));
  smlCol <- wellName;
  
  bigRowN <- match(tolower(bigRow),letters) - 1L;
  bigColN <- as.numeric(bigCol) - 1L;
  smlRowN <- match(tolower(smlRow),letters) - 1L;
  smlColN <- as.numeric(smlCol);
  
  return(bigRowN*12*8*8+bigColN*8*8+smlRowN*8+smlColN);
}


wellNum2Let <- function(wellNum, base=1L){ # former oaWellNumToAddr() but +1!!!
  wellNum <- wellNum - base;
  BigRowN <- wellNum %/% (12*8*8); # 768
  BigRow <- LETTERS[BigRowN+1L]
  remainder <- wellNum %% 768; # wellNum - BigRowN*768; # 
  BigColN <- remainder %/% (8*8);
  BigCol <- BigColN + 1L;
  remainder <- remainder %% 64
  SmlRowN <- remainder %/% 8;
  SmlRow <- letters[SmlRowN+1L]
  SmlCol <- remainder - SmlRowN*8 + 1L;
  return(paste0(BigRow,BigCol,SmlRow,SmlCol));
}

oaWellNumToAddr <- function(wellNum){ #
  BigRowN <- wellNum %/% (12*8*8); # 768
  BigRow <- LETTERS[BigRowN+1L]
  remainder <- wellNum %% 768; # wellNum - BigRowN*768; # 
  BigColN <- remainder %/% (8*8);
  BigCol <- BigColN + 1L;
  remainder <- remainder %% 64
  SmlRowN <- remainder %/% 8;
  SmlRow <- letters[SmlRowN+1L]
  SmlCol <- remainder - SmlRowN*8 + 1L;
  return(paste0(BigRow,BigCol,SmlRow,SmlCol));
}