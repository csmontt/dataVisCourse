#' Label occupations in Pooley´s dataset
#'
#' Just works for pooley's data on codefa and occ varaibles
#' @param df nombre dataframe (dataframe)
#' @param variable variable a recodificar (character)
#' @param newvar nombre nueva varaible recodificada(character)
#' @return df con variable recodificada
#' obtenido
#' @export

# read https://stackoverflow.com/questions/2641653/pass-a-data-frame-column-name-to-a-function

labelOcc <- function(df, variable, newvar){
        df <- df %>%
                mutate(var = fct_recode(df[[variable]],
                            "Professional / Higher managerial" = "1",
                            "Intermediate managerial" = "3",
                            "Skilled non manual" = "4",
                            "Skilled manual" = "5",
                            "Semi-skilled" = "7",
                            "Unskilled" = "8",
                            "Landed farmer" = "2", 
                            "Skilled/semi skilled agriculture" = "6",
                            "Agricultural labourer" = "9"))
        # reorder levels so agricultural occupations are  together
        df[[newvar]] <- fct_relevel(df$var,
                         c('Professional / Higher managerial', 
                           'Intermediate managerial', 
                           'Skilled non manual',
                           "Skilled manual",
                           "Semi-skilled",
                           "Unskilled",
                           "Landed farmer",
                           "Skilled/semi skilled agriculture",
                           "Agricultural labourer")) 
        df <- df %>% dplyr::select(-var)
        
       return(df)
}






