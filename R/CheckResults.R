# FUNCTION: Check match results
# ARGUMENTS
# target_folder

CheckResults <- function(ConlluData_df, LilaData_df, target_folder) {
    singleMatches_df <- read_delim(paste0(target_folder, 'singleMatches.tsv'))
    ambiguousMatches_df <- read_delim(paste0(
        target_folder,
        'ambiguousMatches.tsv'
    ))
    zeroMatches_df <- read_delim(paste0(target_folder, 'zeroMatches.tsv'))

    # join data
    singleMatches_df <- right_join(ConlluData_df, singleMatches_df)
    singleMatches_df <- left_join(singleMatches_df, LilaData_df)
    ambiguousMatches_df <- right_join(ConlluData_df, ambiguousMatches_df)
    ambiguousMatches_df <- left_join(ambiguousMatches_df, LilaData_df)
    zeroMatches_df <- right_join(ConlluData_df, zeroMatches_df)
    zeroMatches_df <- left_join(zeroMatches_df, LilaData_df)

    # create match report
    match3count <- length(unique(singleMatches_df$lemma[
        singleMatches_df$match == 3
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
        '\tUnique lemmas total\n'
    )
    write(
        matchreport,
        paste0(target_folder, 'matchreport.txt')
    )

    # create match2 pairs for checking
    match2checkpairs_df <- singleMatches_df[singleMatches_df$match == 2, ]
    match2checkpairs_df <- unique(paste0(
        match2checkpairs_df$lemma,
        '_',
        match2checkpairs_df$upos,
        '_',
        gsub(
            'Gender=',
            '',
            str_extract(match2checkpairs_df$feats, 'Gender=\\w+')
        ),
        '_____',
        match2checkpairs_df$label,
        '_',
        match2checkpairs_df$upostag,
        '_',
        match2checkpairs_df$gen
    ))
    match2checkpairs_df <- data.frame(
        lila_label = gsub(
            '^(.*)_(.*)_(.*)_____(.*)_(.*)_(.*)',
            '\\4',
            match2checkpairs_df
        ),
        lila_pos = gsub(
            '^(.*)_(.*)_(.*)_____(.*)_(.*)_(.*)',
            '\\5',
            match2checkpairs_df
        ),
        lila_gen = gsub(
            '^(.*)_(.*)_(.*)_____(.*)_(.*)_(.*)',
            '\\6',
            match2checkpairs_df
        ),
        conllu_gender = gsub(
            '^(.*)_(.*)_(.*)_____(.*)_(.*)_(.*)',
            '\\3',
            match2checkpairs_df
        ),
        conllu_pos = gsub(
            '^(.*)_(.*)_(.*)_____(.*)_(.*)_(.*)',
            '\\2',
            match2checkpairs_df
        ),
        conllu_lemma = gsub(
            '^(.*)_(.*)_(.*)_____(.*)_(.*)_(.*)',
            '\\1',
            match2checkpairs_df
        ),
        stringsAsFactors = F
    )
    match2checkpairs_df <- match2checkpairs_df[
        order(match2checkpairs_df$lila_label),
    ]
    match2checkpairs_df <- match2checkpairs_df[
        order(match2checkpairs_df$lila_pos),
    ]
    match2checkpairs_df <- match2checkpairs_df[
        order(match2checkpairs_df$lila_gen),
    ]
    write_tsv(
        match2checkpairs_df,
        paste0(target_folder, 'checkMatch2pairs.tsv')
    )

    # create match1 pairs for checking
    match1checkpairs_df <- singleMatches_df[singleMatches_df$match == 1, ]
    match1checkpairs_df <- unique(paste0(
        match1checkpairs_df$lemma,
        '_',
        match1checkpairs_df$upos,
        '_____',
        match1checkpairs_df$label,
        '_',
        match1checkpairs_df$upostag
    ))
    match1checkpairs_df <- data.frame(
        lila_label = gsub(
            '^(\\w+)_(\\w+)_____(\\w+)_(\\w+)',
            '\\3',
            match1checkpairs_df
        ),
        lila_pos = gsub(
            '^(\\w+)_(\\w+)_____(\\w+)_(\\w+)',
            '\\4',
            match1checkpairs_df
        ),
        conllu_pos = gsub(
            '^(\\w+)_(\\w+)_____(\\w+)_(\\w+)',
            '\\2',
            match1checkpairs_df
        ),
        conllu_lemma = gsub(
            '^(\\w+)_(\\w+)_____(\\w+)_(\\w+)',
            '\\1',
            match1checkpairs_df
        ),
        stringsAsFactors = F
    )
    match1checkpairs_df <- match1checkpairs_df[
        order(match1checkpairs_df$lila_label),
    ]
    match1checkpairs_df <- match1checkpairs_df[
        order(match1checkpairs_df$conllu_pos),
    ]
    match1checkpairs_df <- match1checkpairs_df[
        order(match1checkpairs_df$lila_pos),
    ]
    write_tsv(
        match1checkpairs_df,
        paste0(target_folder, 'checkMatch1pairs.tsv')
    )

    # list df
    MatchesDF_list <- list(
        singleMatches = singleMatches_df,
        ambiguousMatches = ambiguousMatches_df,
        zeroMatches = zeroMatches_df
    )
    print('IMPORTANT ADVISE')
    print('There are new files in target folder:')
    print('- matchreport.txt')
    print('- checkMatch1pairs.tsv')

    return(MatchesDF_list)
}
