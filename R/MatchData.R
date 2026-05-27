# FUNCTION: Match data

# ARGUMENTS:
# ConlluData_df
# LilaData_df
# target_folder

MatchData <- function(ConlluData_df, LilaMatch_df, target_folder) {
    source('./R/Normalizer.R')

    ConlluMatch_df <- ConlluData_df[ConlluData_df$upos != 'PUNCT', ]
    ConlluMatch_df <- ConlluMatch_df[!is.na(ConlluMatch_df$token), ]

    # Prepare match strings from conllu data
    conllu_lemma <- LemmaNormalizer(ConlluMatch_df$lemma)
    conllu_pos <- PosNormalizer(ConlluMatch_df$upos)
    conllu_gender <- GenderNormalizer(str_extract(
        ConlluMatch_df$feats,
        'Gender=\\w+'
    ))

    ConlluMatch_df <- data.frame(
        token_ctrl = ConlluMatch_df$token_ctrl,
        match1 = conllu_lemma,
        match2 = paste0(conllu_lemma, '_', conllu_pos),
        match3 = paste0(conllu_lemma, '_', conllu_pos, '_', conllu_gender),
        stringsAsFactors = F
    )

    # Prepare match strings from lila data
    LilaMatch_df$upostag <- PosNormalizer(LilaMatch_df$upostag)
    LilaMatch_df <- data.frame(
        lila_id = LilaMatch_df$lila_id,
        match1 = LilaMatch_df$wr,
        match2 = paste0(LilaMatch_df$wr, '_', LilaMatch_df$upostag),
        match3 = paste0(
            LilaMatch_df$wr,
            '_',
            LilaMatch_df$upostag,
            '_',
            LilaMatch_df$gen
        ),
        stringsAsFactors = F
    )

    # MATCH 3
    Match3_df <- left_join(
        ConlluMatch_df[,
            colnames(ConlluMatch_df) %in% c('token_ctrl', 'match3')
        ],
        LilaMatch_df[, colnames(LilaMatch_df) %in% c('lila_id', 'match3')],
        by = 'match3',
        relationship = "many-to-many"
    )
    Match3_matches <- Match3_df[!is.na(Match3_df$lila_id), ]
    Match3_matches_table <- as.data.frame(table(Match3_matches$token_ctrl))
    Match3_single <- Match3_matches_table$Var1[Match3_matches_table$Freq == 1]
    Match3_ambiguous <- Match3_matches_table$Var1[Match3_matches_table$Freq > 1]
    Match3_nomatches <- Match3_df$token_ctrl[is.na(Match3_df$lila_id)]
    # sum(length(Match3_single),length(Match3_ambiguous),length(Match3_nomatches))
    Match3_single_df <- Match3_df[Match3_df$token_ctrl %in% Match3_single, ]
    Match3_ambiguous_df <- Match3_df[
        Match3_df$token_ctrl %in% Match3_ambiguous,
    ]

    # MATCH 2
    ConlluMatch_df <- ConlluMatch_df[
        ConlluMatch_df$token_ctrl %in% Match3_nomatches,
    ]
    Match2_df <- left_join(
        ConlluMatch_df[,
            colnames(ConlluMatch_df) %in% c('token_ctrl', 'match2')
        ],
        LilaMatch_df[, colnames(LilaMatch_df) %in% c('lila_id', 'match2')],
        by = 'match2',
        relationship = "many-to-many"
    )
    Match2_matches <- Match2_df[!is.na(Match2_df$lila_id), ]
    Match2_matches_table <- as.data.frame(table(Match2_matches$token_ctrl))
    Match2_single <- Match2_matches_table$Var1[Match2_matches_table$Freq == 1]
    Match2_ambiguous <- Match2_matches_table$Var1[Match2_matches_table$Freq > 1]
    Match2_nomatches <- Match2_df$token_ctrl[is.na(Match2_df$lila_id)]
    # sum(length(Match2_single),length(Match2_ambiguous),length(Match2_nomatches))
    Match2_single_df <- Match2_df[Match2_df$token_ctrl %in% Match2_single, ]
    Match2_ambiguous_df <- Match2_df[
        Match2_df$token_ctrl %in% Match2_ambiguous,
    ]

    # MATCH 1
    ConlluMatch_df <- ConlluMatch_df[
        ConlluMatch_df$token_ctrl %in% Match2_nomatches,
    ]
    Match1_df <- left_join(
        ConlluMatch_df[,
            colnames(ConlluMatch_df) %in% c('token_ctrl', 'match1')
        ],
        LilaMatch_df[, colnames(LilaMatch_df) %in% c('lila_id', 'match1')],
        by = 'match1',
        relationship = "many-to-many"
    )
    Match1_matches <- Match1_df[!is.na(Match1_df$lila_id), ]
    Match1_matches_table <- as.data.frame(table(Match1_matches$token_ctrl))
    Match1_single <- Match1_matches_table$Var1[Match1_matches_table$Freq == 1]
    Match1_ambiguous <- Match1_matches_table$Var1[Match1_matches_table$Freq > 1]
    Match1_nomatches <- Match1_df$token_ctrl[is.na(Match1_df$lila_id)]
    # sum(length(Match1_single),length(Match1_ambiguous),length(Match1_nomatches))
    Match1_single_df <- Match1_df[Match1_df$token_ctrl %in% Match1_single, ]
    Match1_ambiguous_df <- Match1_df[
        Match1_df$token_ctrl %in% Match1_ambiguous,
    ]
    Match1_nomatches_df <- Match1_df[
        Match1_df$token_ctrl %in% Match1_nomatches,
    ]

    # consolidate results
    Match3_single_df$match <- 3
    Match2_single_df$match <- 2
    Match1_single_df$match <- 1
    SingleMatches_df <- rbind(
        Match3_single_df[, c(1, 3:4)],
        Match2_single_df[, c(1, 3:4)],
        Match1_single_df[, c(1, 3:4)]
    )
    AmbiguousMatches_df <- rbind(
        Match3_ambiguous_df[, c(1, 3)],
        Match2_ambiguous_df[, c(1, 3)],
        Match1_ambiguous_df[, c(1, 3)]
    )
    NoMatches_df <- Match1_nomatches_df[, c(1, 3)]
    # sum(length(unique(SingleMatches_df$token_ctrl)),length(unique(AmbiguousMatches_df$token_ctrl)),length(unique(NoMatches_df$token_ctrl)))
    # save tsv in target folder
    write_tsv(SingleMatches_df, paste0(target_folder, 'singleMatches.tsv'))
    write_tsv(
        AmbiguousMatches_df,
        paste0(target_folder, 'ambiguousMatches.tsv')
    )
    write_tsv(NoMatches_df, paste0(target_folder, 'zeroMatches.tsv'))

    return(print('TSV files saved in target folder.'))
}
