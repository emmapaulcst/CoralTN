allDuplicated <- function(data){
  front <- duplicated(data)
  back <- duplicated(data, fromLast = TRUE)
  all_dup <- front + back > 0 
  # On pose la question "est-ce que front + back strictement supérieur à 0 ?"
  # TRUE = 1 et FALSE = 0 => F+F = 0 / F+T = 1 / T+T = 2
  # Du coup R nous répond TRUE c'est supérieur ou FALSE c'est inférieur
  # Sinon ça ne nous retournerai que des chiffres
  return(all_dup)
}