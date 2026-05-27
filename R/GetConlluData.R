# FUNCTION: Get conllu data

# ARGUMENTS:
# source_folder <- '~/@gmail.com/atomiclab/corpora/UD_Latin-LLCT/'

GetConlluData <- function(source_folder) {
   
   conllu_files <- dir(source_folder)[str_detect(
      dir(source_folder),
      '\\.conllu'
   )] %>%
      gsub('(.*)', paste0(source_folder, '\\1'), .)
   
   ConllU_list <- lapply(seq_along(conllu_files), function(i) {
      udpipe_read_conllu(conllu_files[i])
   })
   #
   ConllU_df <- do.call(rbind, ConllU_list)
   ConllU_df$token_ctrl <- paste0(ConllU_df$sentence_id,'_',ConllU_df$token_id,'_',ConllU_df$token)

   return(ConllU_df)
}
