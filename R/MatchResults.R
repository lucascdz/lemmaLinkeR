# FUNCTION: Check match results
# ARGUMENTS
# target_folder

MatchResults <- function(ConlluData_df, LilaData_df, target_folder) {
    # get results
    singleMatches_df <- read.csv(
        paste0(target_folder, 'singleMatches.tsv'),
        sep = '\t'
    )
    ambiguousMatches_df <- read.csv(
        paste0(
            target_folder,
            'ambiguousMatches.tsv'
        ),
        sep = '\t'
    )
    zeroMatches_df <- read.csv(
        paste0(target_folder, 'zeroMatches.tsv'),
        sep = '\t'
    )

    # join data ====
    singleMatches_df <- right_join(ConlluData_df, singleMatches_df)
    singleMatches_df <- left_join(singleMatches_df, LilaData_df)
    ambiguousMatches_df <- right_join(ConlluData_df, ambiguousMatches_df)
    ambiguousMatches_df <- left_join(ambiguousMatches_df, LilaData_df)
    zeroMatches_df <- right_join(ConlluData_df, zeroMatches_df)
    zeroMatches_df <- left_join(zeroMatches_df, LilaData_df)

    # create match report ====
    match3count <- length(unique(singleMatches_df$lemma[
        singleMatches_df$match %in% c('3', '3alt')
    ]))
    match2count <- length(unique(singleMatches_df$lemma[
        singleMatches_df$match == 2
    ]))
    match1count <- length(unique(singleMatches_df$lemma[
        singleMatches_df$match == 1
    ]))
    ambiguouscount <- length(unique(ambiguousMatches_df$lemma))
    zeromatchescount <- length(unique(zeroMatches_df$lemma))
    matchname <- gsub('.*/(.*)/$', '\\1', target_folder)
    matchreport <- paste0(
        'Match report for ',
        matchname,
        ': \n\n',
        match3count,
        '\tSingle matches (lemma_pos_gender)\n',
        match2count,
        '\tSingle matches (lemma_pos)\n',
        match1count,
        '\tSingle matches (lemma)\n',
        ambiguouscount,
        '\tAmbiguous matches\n',
        zeromatchescount,
        '\tZero matches\n\n',
        sum(
            match3count,
            match2count,
            match1count,
            ambiguouscount,
            zeromatchescount
        ),
        '\tUnique lemmas total\n\n\n',
        'LilaDB access date: ',
        date(),
        '\n  id_lemma maximum value: ',
        max(as.numeric(gsub(
            '^\\w+:',
            '',
            LilaData_df$lila_id[str_detect(LilaData_df$lila_id, 'lilaLemma')]
        ))),
        '\n  id_ipolemma maximum value: ',
        max(as.numeric(gsub(
            '^\\w+:',
            '',
            LilaData_df$lila_id[str_detect(LilaData_df$lila_id, 'lilaIpoLemma')]
        )))
    )
    write(
        matchreport,
        paste0(target_folder, 'matchReport.txt')
    )

    # create match2 DF for checking ====
    checkSingleMatch2_df <- singleMatches_df[singleMatches_df$match == 2, ]
    checkSingleMatch2_df <- unique(paste0(
        checkSingleMatch2_df$lemma,
        '_',
        checkSingleMatch2_df$upos,
        '_',
        gsub(
            'Gender=',
            '',
            str_extract(checkSingleMatch2_df$feats, 'Gender=\\w+')
        ),
        '_____',
        checkSingleMatch2_df$label,
        '_',
        checkSingleMatch2_df$upostag,
        '_',
        checkSingleMatch2_df$gen
    ))
    checkSingleMatch2_df <- data.frame(
        lila_label = gsub(
            '^(.*)_(.*)_(.*)_____(.*)_(.*)_(.*)',
            '\\4',
            checkSingleMatch2_df
        ),
        lila_pos = gsub(
            '^(.*)_(.*)_(.*)_____(.*)_(.*)_(.*)',
            '\\5',
            checkSingleMatch2_df
        ),
        lila_gen = gsub(
            '^(.*)_(.*)_(.*)_____(.*)_(.*)_(.*)',
            '\\6',
            checkSingleMatch2_df
        ),
        conllu_gender = gsub(
            '^(.*)_(.*)_(.*)_____(.*)_(.*)_(.*)',
            '\\3',
            checkSingleMatch2_df
        ),
        conllu_pos = gsub(
            '^(.*)_(.*)_(.*)_____(.*)_(.*)_(.*)',
            '\\2',
            checkSingleMatch2_df
        ),
        conllu_lemma = gsub(
            '^(.*)_(.*)_(.*)_____(.*)_(.*)_(.*)',
            '\\1',
            checkSingleMatch2_df
        ),
        stringsAsFactors = F
    )
    checkSingleMatch2_df <- checkSingleMatch2_df[
        order(checkSingleMatch2_df$lila_label),
    ]
    checkSingleMatch2_df <- checkSingleMatch2_df[
        order(checkSingleMatch2_df$lila_pos),
    ]
    checkSingleMatch2_df <- checkSingleMatch2_df[
        order(checkSingleMatch2_df$lila_gen),
    ]
    write_tsv(
        checkSingleMatch2_df,
        paste0(target_folder, 'checkSingleMatch2.tsv')
    )

    # create match1 DF for checking ====
    checkSingleMatch1_df <- singleMatches_df[singleMatches_df$match == 1, ]
    checkSingleMatch1_df <- unique(paste0(
        checkSingleMatch1_df$lemma,
        '_',
        checkSingleMatch1_df$upos,
        '_____',
        checkSingleMatch1_df$label,
        '_',
        checkSingleMatch1_df$upostag
    ))
    checkSingleMatch1_df <- data.frame(
        lila_label = gsub(
            '^(\\w+)_(\\w+)_____(\\w+)_(\\w+)',
            '\\3',
            checkSingleMatch1_df
        ),
        lila_pos = gsub(
            '^(\\w+)_(\\w+)_____(\\w+)_(\\w+)',
            '\\4',
            checkSingleMatch1_df
        ),
        conllu_pos = gsub(
            '^(\\w+)_(\\w+)_____(\\w+)_(\\w+)',
            '\\2',
            checkSingleMatch1_df
        ),
        conllu_lemma = gsub(
            '^(\\w+)_(\\w+)_____(\\w+)_(\\w+)',
            '\\1',
            checkSingleMatch1_df
        ),
        stringsAsFactors = F
    )
    checkSingleMatch1_df <- checkSingleMatch1_df[
        order(checkSingleMatch1_df$lila_label),
    ]
    checkSingleMatch1_df <- checkSingleMatch1_df[
        order(checkSingleMatch1_df$conllu_pos),
    ]
    checkSingleMatch1_df <- checkSingleMatch1_df[
        order(checkSingleMatch1_df$lila_pos),
    ]
    write_tsv(
        checkSingleMatch1_df,
        paste0(target_folder, 'checkSingleMatch1.tsv')
    )

    # create ambiguous DF for checking ====

    # token_ctrl
    # lila_id
    # conllu_data=paste0(lemma_pos)
    # lila_data=paste0(label_upostag)
    # dict_data (LS,Faria,Fons,Velez,Cardoso) -> create a list of dictionary folders in conllu_app


    return(print('New files in target folder: (1) matchReport.txt (2) checkSingleMatch1.tsv (3) checkSingleMatch2.tsv'))
}
