#' Dictionary for the Project
#'
#' This is the dictionary used for internationalization.
#' It is an `i18n_dict` class object (see [psychTestR::i18n_dict]).
#' The dictionary contains German (`de` and `de_f`) and Japanese (`ja`)
#' as well as some English (`en`) translations.
#'
#' @details
#' The dictionary includes the Goldsmiths Musical Sophistication Index
#' (Gold-MSI),
#' an adapted version of the Animism Scale for Adults (ASA),
#' the Basic Empathy Scale (BES),
#' as well as general questionnaire formulations.
#'
#' Everything regarding Gold-MSI is taken from Müllensiefen et al. (2014),
#' Sadakata et al. (2022), and Schaal et al. (2014).
#' The adapted version of the ASA is based on 池内 (2010).
#' The BES is in the variant as validated by Heynen et al. (2016).
#' Some general formulations in English and German are taken from `psyquest`
#' (Höger et al., 2024).
#'
#' @references
#' Heynen, E. J. E., Van Der Helm, G. H. P., Stams, G. J. J. M., &
#' Korebrits, A. M. (2016). Measuring empathy in a german youth prison:
#' A validation of the German version of the Basic Empathy Scale (BES) in
#' a sample of incarcerated juvenile offenders.
#' *Journal of Forensic Psychology Practice*, *16*(5), 336--346.
#' <https://doi.org/10.1080/15228932.2016.1219217>
#'
#' Höger, F., Frieler, K., Pausch, V., Sander, K., & Ruth, N. (2024).
#' *psyquest* (Version 1.5.9) \[R package\].
#' <https://github.com/klausfrieler/psyquest>
#'
#' 池内裕美. (2010). 成人のアニミズム的思考: 自発的喪失としてのモノ供養の心理.
#' *社会心理学研究*, *25*(3), 167--177.
#' <https://doi.org/10.14966/jssp.KJ00006203282>
#' \[Ikeuchi, H. (2010). Animistic thinking in adults:
#' The memorial service for dolls as a voluntary loss.
#' *The Japanese Journal of Social Psychology*, *25*(3), 167--177.\]
#'
#' Müllensiefen, D., Gingras, B., Musil, J., & Stewart, L. (2014).
#' The musicality of non-musicians: An index for assessing musical
#' sophistication in the general population. *PLOS ONE*, *9*(2), Article e89642.
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
