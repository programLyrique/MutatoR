# mutation results for R.methodsS3 remain stable

    {
      "outcome": "OK",
      "summary": "generated=1197 | tested=10 | killed=3 | hanged=0 | survived=7 | mutation_score=30 | mutation_score_ci=10.7791267406301,60.3221852538855 | confidence=0.95",
      "mutants": "030.setMethodS3.R_030.setMethodS3.R_096.R | R/030.setMethodS3.R | SURVIVED | 130 | 3 | 135 | 26 | 'private' -> 'NULL'\n030.setMethodS3.R_030.setMethodS3.R_276.R | R/030.setMethodS3.R | SURVIVED | 251 | 5 | 251 | 42 | '[\t\n\f\r ]*$' -> 'NA_character_'\n030.setMethodS3.R_030.setMethodS3.R_237.R | R/030.setMethodS3.R | SURVIVED | 228 | 5 | 228 | 42 | '' -> 'NA_character_'\n030.setMethodS3.R_030.setMethodS3.R_069.R | R/030.setMethodS3.R | KILLED | 115 | 5 | 120 | 5 | '<-' -> '<deleted>'\n000.R_000.R_062.R | R/000.R | SURVIVED | 65 | 11 | 65 | 23 | 'base' -> 'NULL'\nisGenericS3.R_isGenericS3.R_155.R | R/isGenericS3.R | SURVIVED | 179 | 1 | 179 | 36 | 'FALSE' -> 'NA'\n010.setGenericS3.R_010.setGenericS3.R_107.R | R/010.setGenericS3.R | SURVIVED | 152 | 7 | 152 | 249 | '' -> 'NA_character_'\nisGenericS3.R_isGenericS3.R_106.R | R/isGenericS3.R | SURVIVED | 117 | 3 | 122 | 3 | 'if' -> '<deleted>'\nisGenericS3.R_isGenericS3.R_006.R | R/isGenericS3.R | KILLED | 42 | 13 | 42 | 23 | '1L' -> 'NA_integer_'\nzzz.R_zzz.R_028.R | R/zzz.R | KILLED | 18 | 5 | 18 | 26 | '1' -> 'NA_real_'"
    }

# mutation results for forcats remain stable

    {
      "outcome": "OK",
      "summary": "generated=788 | tested=10 | killed=6 | hanged=0 | survived=4 | mutation_score=60 | mutation_score_ci=31.2673769733658,83.1819670293764 | confidence=0.95",
      "mutants": "match.R_match.R_015.R | R/match.R | KILLED | 29 | 3 | 29 | 40 | '<-' -> '<deleted>'\nfct.R_fct.R_022.R | R/fct.R | KILLED | 49 | 7 | 49 | 25 | 'length(invalid) > 0' -> '!length(invalid) > 0'\nlump.R_lump.R_087.R | R/lump.R | KILLED | 191 | 7 | 191 | 19 | 'i + 1' -> 'NULL'\nreorder.R_reorder.R_021.R | R/reorder.R | SURVIVED | 73 | 21 | 77 | 7 | 'Use {.code .na_rm = FALSE} to preserve NAs.' -> 'NULL'\nlump.R_lump.R_046.R | R/lump.R | SURVIVED | 142 | 3 | 142 | 44 | 'check_string' -> '<deleted>'\nrelevel.R_relevel.R_012.R | R/relevel.R | KILLED | 50 | 3 | 58 | 3 | '<-' -> '<deleted>'\nlump.R_lump.R_121.R | R/lump.R | SURVIVED | 228 | 10 | 228 | 14 | '0' -> 'NULL'\nlump.R_lump.R_033.R | R/lump.R | KILLED | 121 | 5 | 121 | 30 | '/' -> '*'\nlvls.R_lvls.R_021.R | R/lvls.R | SURVIVED | 50 | 3 | 56 | 3 | '<-' -> '<deleted>'\nlvls.R_lvls.R_071.R | R/lvls.R | KILLED | 105 | 3 | 113 | 3 | '<-' -> '<deleted>'"
    }

# mutation results for jsonlite remain stable

    {
      "outcome": "OK",
      "summary": "generated=2258 | tested=10 | killed=4 | hanged=0 | survived=6 | mutation_score=40 | mutation_score_ci=16.8180329706236,68.7326230266342 | confidence=0.95",
      "mutants": "asJSON.complex.R_asJSON.complex.R_006.R | R/asJSON.complex.R | KILLED | 4 | 3 | 4 | 31 | '<-' -> '<deleted>'\nis.recordlist.R_is.recordlist.R_011.R | R/is.recordlist.R | SURVIVED | 12 | 5 | 12 | 55 | '!(is.namedlist(i) || is.null(i))' -> '(is.namedlist(i) || is.null(i))'\nnum_to_char.R_num_to_char.R_002.R | R/num_to_char.R | KILLED | 3 | 7 | 3 | 19 | 'is.na(digits)' -> '!is.na(digits)'\nnum_to_char.R_num_to_char.R_032.R | R/num_to_char.R | SURVIVED | 28 | 29 | 28 | 41 | '!is.finite(x)' -> 'is.finite(x)'\napply_by_pages.R_apply_by_pages.R_062.R | R/apply_by_pages.R | SURVIVED | 26 | 3 | 29 | 3 | 'FUN' -> '<deleted>'\nbase64.R_base64.R_056.R | R/base64.R | SURVIVED | 57 | 3 | 60 | 3 | '<-' -> '<deleted>'\nsimplify.R_simplify.R_130.R | R/simplify.R | KILLED | 115 | 9 | 115 | 20 | '1' -> 'NA_real_'\nbase64.R_base64.R_007.R | R/base64.R | KILLED | 24 | 3 | 24 | 31 | '.Call' -> '<deleted>'\njson_gzip.R_json_gzip.R_009.R | R/json_gzip.R | SURVIVED | 37 | 21 | 37 | 46 | 'gzip' -> 'NULL'\nasJSON.sf.R_asJSON.sf.R_056.R | R/asJSON.sf.R | SURVIVED | 41 | 3 | 41 | 13 | 'return' -> '<deleted>'"
    }

# mutation results for lumberjack remain stable

    {
      "outcome": "OK",
      "summary": "generated=654 | tested=10 | killed=8 | hanged=0 | survived=2 | mutation_score=80 | mutation_score_ci=49.0162471536642,94.3317848545625 | confidence=0.95",
      "mutants": "run.R_run.R_031.R | R/run.R | SURVIVED | 32 | 20 | 32 | 34 | '!is.null(label)' -> 'is.null(label)'\nexpression_logger.R_expression_logger.R_045.R | R/expression_logger.R | KILLED | 85 | 9 | 85 | 49 | 'write.csv' -> '<deleted>'\nlumberjack.R_lumberjack.R_043.R | R/lumberjack.R | KILLED | 100 | 3 | 102 | 3 | '<-' -> '<deleted>'\nutils.R_utils.R_022.R | R/utils.R | KILLED | 23 | 3 | 34 | 3 | 'if' -> '<deleted>'\nlumberjack.R_lumberjack.R_002.R | R/lumberjack.R | KILLED | 22 | 1 | 22 | 20 | '__log__' -> 'NULL'\nsimple.R_simple.R_035.R | R/simple.R | KILLED | 73 | 9 | 73 | 59 | 'write.csv' -> '<deleted>'\nlumberjack.R_lumberjack.R_077.R | R/lumberjack.R | KILLED | 124 | 8 | 124 | 21 | 'is.null(store)' -> '!is.null(store)'\nfiledump.R_filedump.R_038.R | R/filedump.R | SURVIVED | 86 | 9 | 86 | 56 | 'write.csv' -> '<deleted>'\nlumberjack.R_lumberjack.R_107.R | R/lumberjack.R | KILLED | 209 | 9 | 209 | 29 | 'is.function(log$stop)' -> '!is.function(log$stop)'\nrun.R_run.R_004.R | R/run.R | KILLED | 4 | 4 | 4 | 17 | 'data' -> 'NA_character_'"
    }

# mutation results for nanotime remain stable

    {
      "outcome": "OK",
      "summary": "generated=3429 | tested=10 | killed=8 | hanged=0 | survived=2 | mutation_score=80 | mutation_score_ci=49.0162471536642,94.3317848545625 | confidence=0.95",
      "mutants": "nanoduration.R_nanoduration.R_361.R | R/nanoduration.R | KILLED | 337 | 1 | 340 | 12 | 'nanoduration' -> 'NA_character_'\nnanotime.R_nanotime.R_749.R | R/nanotime.R | KILLED | 755 | 39 | 755 | 53 | '0L' -> 'NA_integer_'\nnanotime.R_nanotime.R_847.R | R/nanotime.R | KILLED | 938 | 1 | 938 | 69 | 'nano_wday' -> 'NULL'\nnanotime.R_nanotime.R_144.R | R/nanotime.R | KILLED | 241 | 1 | 241 | 71 | 'nanotime' -> 'NULL'\nnanoival.R_nanoival.R_307.R | R/nanoival.R | KILLED | 375 | 1 | 378 | 12 | 'nanoival' -> 'NA_character_'\nnanotime.R_nanotime.R_105.R | R/nanotime.R | KILLED | 216 | 33 | 216 | 79 | '1e+09' -> 'NULL'\nnanoival.R_nanoival.R_382.R | R/nanoival.R | SURVIVED | 426 | 1 | 429 | 12 | 'ANY' -> 'NULL'\nnanotime.R_nanotime.R_793.R | R/nanotime.R | SURVIVED | 783 | 1 | 783 | 45 | 'nanotime' -> 'NA_character_'\nnanoival.R_nanoival.R_412.R | R/nanoival.R | KILLED | 450 | 15 | 450 | 88 | 'operations are possible only for numeric, logical or complex types' -> 'NULL'\nnanoperiod.R_nanoperiod.R_705.R | R/nanoperiod.R | KILLED | 735 | 1 | 735 | 76 | 'nanoperiod' -> 'NA_character_'"
    }

# mutation results for oRaklE remain stable

    {
      "outcome": "OK",
      "summary": "generated=10185 | tested=10 | killed=0 | hanged=0 | survived=10 | mutation_score=0 | mutation_score_ci=2.77555756156289e-15,27.7532799862889 | confidence=0.95",
      "mutants": "combine_models.R_combine_models.R_236.R | R/combine_models.R | SURVIVED | 130 | 3 | 130 | 44 | '<-' -> '<deleted>'\nlong_term_future.R_long_term_future.R_219.R | R/long_term_future.R | SURVIVED | 104 | 9 | 104 | 140 | '/models/longterm/best_lm_model' -> 'NA_character_'\ncombine_models_future.R_combine_models_future.R_237.R | R/combine_models_future.R | SURVIVED | 128 | 7 | 129 | 105 | '==' -> '!='\nget_macro_economic_data.R_get_macro_economic_data.R_037.R | R/get_macro_economic_data.R | SURVIVED | 55 | 7 | 55 | 32 | 'res_pop$status_code == 502' -> '!res_pop$status_code == 502'\nlong_term_lm.R_long_term_lm.R_306.R | R/long_term_lm.R | SURVIVED | 182 | 3 | 182 | 58 | '-' -> '+'\nlong_term_future_data.R_long_term_future_data.R_094.R | R/long_term_future_data.R | SURVIVED | 60 | 7 | 60 | 89 | '1' -> 'NA_real_'\nfill_missing_data.R_fill_missing_data.R_156.R | R/fill_missing_data.R | SURVIVED | 99 | 3 | 142 | 3 | '<-' -> '<deleted>'\nmid_term_future.R_mid_term_future.R_238.R | R/mid_term_future.R | SURVIVED | 129 | 3 | 129 | 24 | '<-' -> '<deleted>'\ndecompose_load_data.R_decompose_load_data.R_552.R | R/decompose_load_data.R | SURVIVED | 232 | 3 | 241 | 3 | 'MW' -> 'NULL'\ncombine_models.R_combine_models.R_704.R | R/combine_models.R | SURVIVED | 280 | 3 | 314 | 41 | '[MW]\n' -> 'NULL'"
    }

# mutation results for prettyunits remain stable

    {
      "outcome": "OK",
      "summary": "generated=1078 | tested=10 | killed=9 | hanged=0 | survived=1 | mutation_score=90 | mutation_score_ci=59.5849973204762,98.2123786904927 | confidence=0.95",
      "mutants": "rounding.R_rounding.R_051.R | R/rounding.R | KILLED | 47 | 13 | 47 | 22 | 'digits < 0' -> '!digits < 0'\nsizes.R_sizes.R_100.R | R/sizes.R | KILLED | 68 | 5 | 68 | 46 | '|' -> '&'\nsizes.R_sizes.R_061.R | R/sizes.R | KILLED | 41 | 5 | 41 | 57 | '1L' -> 'NULL'\nrounding.R_rounding.R_024.R | R/rounding.R | KILLED | 28 | 5 | 30 | 5 | '<-' -> '<deleted>'\nnumbers.R_numbers.R_014.R | R/numbers.R | SURVIVED | 17 | 5 | 17 | 94 | 'p' -> 'NA_character_'\ntime.R_time.R_058.R | R/time.R | KILLED | 29 | 5 | 67 | 5 | '<-' -> '<deleted>'\nnumbers.R_numbers.R_219.R | R/numbers.R | KILLED | 102 | 5 | 102 | 43 | '!neg' -> 'neg'\ntime.R_time.R_009.R | R/time.R | KILLED | 11 | 5 | 16 | 5 | '86400000' -> 'NA_real_'\ntime-ago.R_time-ago.R_134.R | R/time-ago.R | KILLED | 50 | 3 | 57 | 3 | '%2ds' -> 'NA_character_'\nsizes.R_sizes.R_063.R | R/sizes.R | KILLED | 42 | 5 | 42 | 34 | '/' -> '*'"
    }

# mutation results for scales remain stable

    {
      "outcome": "OK",
      "summary": "generated=4720 | tested=10 | killed=7 | hanged=0 | survived=3 | mutation_score=70 | mutation_score_ci=39.6778147461145,89.2208732593699 | confidence=0.95",
      "mutants": "breaks-log.R_breaks-log.R_200.R | R/breaks-log.R | KILLED | 158 | 7 | 158 | 31 | '0' -> 'NULL'\npal-dichromat.R_pal-dichromat.R_018.R | R/pal-dichromat.R | SURVIVED | 30 | 5 | 30 | 75 | '}' -> 'NA_character_'\ncolour-ramp.R_colour-ramp.R_057.R | R/colour-ramp.R | KILLED | 71 | 5 | 71 | 80 | 'lab' -> 'NA_character_'\npal-grey.R_pal-grey.R_005.R | R/pal-grey.R | SURVIVED | 12 | 3 | 16 | 3 | '255' -> 'NA_real_'\nlabel-number.R_label-number.R_309.R | R/label-number.R | KILLED | 488 | 3 | 488 | 50 | '1e+06' -> 'NULL'\ntransform-numeric.R_transform-numeric.R_204.R | R/transform-numeric.R | KILLED | 205 | 5 | 205 | 48 | '1' -> 'NA_real_'\nminor_breaks.R_minor_breaks.R_064.R | R/minor_breaks.R | SURVIVED | 75 | 5 | 81 | 5 | 'if' -> '<deleted>'\nlabel-ordinal.R_label-ordinal.R_041.R | R/label-ordinal.R | KILLED | 107 | 3 | 108 | 66 | '<-' -> '<deleted>'\nlabel-number.R_label-number.R_410.R | R/label-number.R | KILLED | 533 | 1 | 551 | 1 | '9' -> 'NA_real_'\ncolour-manip.R_colour-manip.R_185.R | R/colour-manip.R | KILLED | 232 | 3 | 232 | 26 | 'UseMethod' -> '<deleted>'"
    }

# mutation results for stringr remain stable

    {
      "outcome": "OK",
      "summary": "generated=1260 | tested=10 | killed=7 | hanged=0 | survived=3 | mutation_score=70 | mutation_score_ci=39.6778147461145,89.2208732593699 | confidence=0.95",
      "mutants": "match.R_match.R_008.R | R/match.R | SURVIVED | 54 | 20 | 54 | 70 | '{.arg pattern} must be a regular expression.' -> 'NA_character_'\nword.R_word.R_038.R | R/word.R | KILLED | 46 | 3 | 46 | 22 | '<-' -> '<deleted>'\nreplace.R_replace.R_012.R | R/replace.R | KILLED | 108 | 3 | 111 | 3 | '!missing(replacement) && is_replacement_fun(replacement)' -> '!(!missing(replacement) && is_replacement_fun(replacement))'\nmodifiers.R_modifiers.R_125.R | R/modifiers.R | SURVIVED | 257 | 15 | 257 | 32 | 'options' -> 'NA_character_'\nlocate.R_locate.R_012.R | R/locate.R | KILLED | 79 | 13 | 84 | 5 | 'TRUE' -> 'NULL'\ncase.R_case.R_047.R | R/case.R | KILLED | 115 | 3 | 127 | 3 | '(?<=\\p{L})(?=\\p{N})' -> 'NA_character_'\nview.R_view.R_036.R | R/view.R | SURVIVED | 105 | 27 | 105 | 63 | 'stringr_boundary' -> 'NA_character_'\nflatten.R_flatten.R_048.R | R/flatten.R | KILLED | 67 | 3 | 67 | 55 | ', ' -> 'NA_character_'\nutils.R_utils.R_017.R | R/utils.R | KILLED | 48 | 7 | 48 | 19 | 'is.matrix(to)' -> '!is.matrix(to)'\nsub.R_sub.R_004.R | R/sub.R | KILLED | 67 | 3 | 71 | 3 | 'stri_sub' -> '<deleted>'"
    }

