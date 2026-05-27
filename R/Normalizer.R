# FUNCTION: A normalizer for Latin

LemmaNormalizer <- function(vector){

   vector <- gsub('[\\.,:;?!…]','',vector) %>%
      gsub('\\[|\\]|\\{|\\}|\\(|\\)','',.) %>%
      gsub('[-_]',' ',.) %>%
      gsub('(.*)','\\L\\1',., perl = T) %>%
      gsub('æ','ae',.) %>%
      gsub('œ','oe',.) %>%
      gsub('&','et',.) %>%
      gsub('v','u',.) %>%
      gsub('j','i',.) %>%
      gsub('[âàáāä]','a',.) %>%
      gsub('[èêéēë]','e',.) %>%
      gsub('[îìíïī]','i',.) %>%
      gsub('[òôóōö]','o',.) %>%
      gsub('[ûùúüū]','u',.) %>%
      gsub('[ýÿŷȳẏẙỳỹ]','y',.)

   return(vector)
}

PosNormalizer <- function(vector){

   vector <- gsub('PROPN','NOUN',vector)

   return(vector)
}


GenderNormalizer <- function(vector){

   vector <- gsub('Gender=Masc','m',vector) %>%
      gsub('Gender=Fem','f',.) %>%
      gsub('Gender=Neut','n',.)
   vector[is.na(vector)] <- ''
   
   return(vector)
}
