# install.packages(c('udpipe','tidyverse'))

library(udpipe)
library(tidyverse)

source_folder <- '~/@gmail.com/atomiclab/corpora/UD_Latin-LLCT/'
target_folder <- gsub('/$', '-Linking/', source_folder)
system(paste0('mkdir ', target_folder))

## GET UD data
source('R/GetConlluData.R')
ConlluData_df <- GetConlluData(source_folder)

## GET LiLa data
source('~/@gmail.com/atomiclab/lila_db/LilaDB_getData.R')
LilaDB_list <- GetLilaData()
LilaData_df <- LilaDB_list[['LilaData']]
LilaWrData_df <- LilaDB_list[['LilaWrData']]

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
## MATCH data (skip if you are already working on results)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
source('R/MatchData.R')
MatchData(ConlluData_df,LilaWrData_df,target_folder)

source('R/MatchResults.R')
MatchResults_list <- MatchResults(ConlluData_df,LilaData_df,target_folder)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # # 
# MATCH DONE #
# in 12 sec. #
# # # # # # # 
'disambiguate'

## GET LiLa IDs
ConllUlinks <- read.csv(
   paste0(target_folder, 'proielUD_lemma_ids.tsv'),
   sep = '\t'
)
ConllUlinked <- left_join(ConllU, ConllUlinks)
ConllUlinked$lila_id <- gsub(
   'lilaLemma:',
   'http://lila-erc.eu/data/id/lemma/',
   ConllUlinked$lila_id
) %>%
   gsub('lilaIpoLemma:', 'http://lila-erc.eu/data/id/hypolemma/', .)
identical(ConllU$token, ConllUlinked$token)

## GET Citation Hierarchy metadata
## >>>> edit function to split, add and rejoin MISC data
# unique(unlist(str_extract_all(ConllU_df$misc,'\\w+=')))
## NOPE. ONLY 'Ref' AND 'LId' => 'misc' CAN BE FULLY REPLACED

source('./R/ConllU_getCitationHierarchy.R')
ProielSentMetaDF <- read.delim(paste0(
   target_folder,
   'proielUD_sentMetadata.tsv'
))
ConllUlinkedWithMetaDF <- GetCitationHierarchy(ConllUlinked, ProielSentMetaDF)

## SPLIT DF BY doc_id
ConllUlinkedWithMetaLIST <- split(
   ConllUlinkedWithMetaDF,
   ConllUlinkedWithMetaDF$doc_id
)

## save conllu files
lapply(seq_along(ConllUlinkedWithMetaLIST), function(i) {
   cat(
      as_conllu(ConllUlinkedWithMetaLIST[[i]]),
      file = paste0(
         target_folder,
         names(ConllUlinkedWithMetaLIST)[i],
         '.conllu'
      )
   )
})

## create conllup files (lila_ids in extra column)
createConllup <- function(filepath) {
   conllufile <- readLines(filepath)
   conllup_df <- data.frame(line = conllufile, stringsAsFactors = F)
   conllup_df$extra <- str_extract(conllup_df[, 1], 'LId=(.*)\\|.*', group = T)
   conlluplines <- paste0(conllup_df$line, '\t', conllup_df$extra)
   conlluplines <- gsub('\\tNA', '', conlluplines)
   write(conlluplines, gsub('(.*)', '\\1p', filepath))
}

filepaths <- unlist(lapply(seq_along(ConllUlinkedWithMetaLIST), function(i) {
   paste0(target_folder, names(ConllUlinkedWithMetaLIST)[i], '.conllu')
}))
lapply(seq_along(filepaths), function(i) createConllup(filepaths[i]))

## DONE
