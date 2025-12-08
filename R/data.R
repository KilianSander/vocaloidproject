#' Dictionary for the Project
#'
#' This is the dictionary used for internationalization.
#' It is an `i18n_dict` class object (see [psychTestR::i18n_dict]).
#' The dictionary contains German (`de` and `de_f`) and Japanese (`ja`)
#' as well as English (`en`) translations.
#'
#' @details
#' The dictionary includes the Goldsmiths Musical Sophistication Index
#' (Gold-MSI) as well as general questionnaire formulations.
#'
#' Everything regarding Gold-MSI is taken from Müllensiefen et al. (2014),
#' Sadakata et al. (2022), and Schaal et al. (2014).
#' Some general formulations in English and German are taken from `psyquest`
#' (Höger et al., 2024).
#'
#' @references
#' Höger, F., Frieler, K., Pausch, V., Sander, K., & Ruth, N. (2024).
#' *psyquest* (Version 1.5.9) \[R package\].
#' <https://github.com/klausfrieler/psyquest>
#'
#' Müllensiefen, D., Gingras, B., Musil, J., & Stewart, L. (2014).
#' The musicality of non-musicians: An index for assessing musical
#' sophistication in the general population. *PLoS ONE*, *9*(2), Article e89642.
#' <https://doi.org/10.1371/journal.pone.0089642>
#'
#' Sadakata, M., Yamaguchi, Y., Ohsawa, C., Matsubara, M., Terasawa, H.,
#' Von Schnehen, A., Müllensiefen, D., & Sekiyama, K. (2022).
#' The Japanese translation of the Gold-MSI: Adaptation and validation of the
#' self-report questionnaire of musical sophistication. *Musicae Scientiae*,
#' *27*(3), 798–810. <https://doi.org/10.1177/10298649221110089>
#'
#' Schaal, N. K., Bauer, A.-K. R., & Müllensiefen, D. (2014).
#' Der Gold-MSI: Replikation und Validierung eines Fragebogeninstrumentes zur
#' Messung Musikalischer Erfahrenheit anhand einer deutschen Stichprobe.
#' *Musicae Scientiae*, *18*(4), 423–447.
#' <https://doi.org/10.1177/1029864914541851>
#'
#' @docType data
#' @name vocaloidproject_dict
#'
"vocaloidproject_dict"

#' Data frame of the dictionary for the project
#'
#' This is the project dictionary as a data.frame.
#' For details on the dictionary, see [vocaloidproject_dict].
#'
#' @docType data
#' @name vocaloidproject_dict_df
#'
"vocaloidproject_dict_df"

#' Stimulus coding for International Experiment 1
#'
#' Stimuli for International Experiment 1 (also referred to as Experiment 4)
#' are 30 excerpts in 4 conditions.
#' The original file names followed the naming scheme `EXCERPT_CONDITION.mp3`,
#' where `EXCERPT` is a number from 1 to 30 denoting the excerpt and
#' `CONDITION` is a string denoting the condition.
#' For use in the experiment file names were re-coded to `CEE.mp3`, where
#' `C` denotes the condition and `EE` is a width-two integer denoting the
#' excerpt (i.e., Excerpt 2 is represented by `02`).
#'
#' @docType data
#' @name exp4_international1_stimuli
#'
"exp4_international1_stimuli"

#' Scoring Maps
#'
#' This list contains scoring maps (i.e., data frames) for some questionnaires.
#'
#' @docType data
#' @name scoring_maps
#'
"scoring_maps"
