
#' Accumulated Lactate to Lactic Energy Conversion
#'
#'  Converts accumulated lactate (mmol/L) to lactic energy (J/kg), considering the accumulation
#'  of 1 mmol L-1 of blood lactate corresponds to an energy expenditure from the
#'  anaerobic lactic metabolism equivalent to the consumption of 3 mL O2/kg of body mass for males and
#'  2.7 mL O2/kg of body mass for females and considering an O2 energy equivalent of 20.9 J/mL. For more details,
#'  see Briand et al. (2025) and di Prampero et al. (1981).
#'
#' @param acc_lactate A numeric value representing accumulated lactate in mmol/L.
#' @param sex A character corresponding to the athlete sex (default is `"male"`, other option is `"female"`).
#'
#' @returns A numeric value representing lactic energy in J/kg.
#' @export
#'
#' @examples
#'
#' acc_lactate <- 10  # Example accumulated lactate in mmol/L
#' convert_acc_lactate_to_lactic_energy(acc_lactate)
#'
#'
convert_acc_lactate_to_lactic_energy <- function(acc_lactate, sex = "male"){

  if(sex == "male"){
    acc_lactate_o2_eq <- 3
  }
  if(sex == "female"){
    acc_lactate_o2_eq <- 2.7
  }

  o2_energy_equivalent <- 20.9

  lactic_energy <- acc_lactate * acc_lactate_o2_eq * o2_energy_equivalent

  return(lactic_energy)
}

#' Lactic Energy to Accumulated Lactate Conversion
#'
#'  Converts lactic energy (J/kg) to accumulated lactate (mmol/L), considering the accumulation
#'  of 1 mmol L-1 of blood lactate corresponds to an energy expenditure from the
#'  anaerobic lactic metabolism equivalent to the consumption of 3 mL O2/kg of body mass for males and
#'  2.7 mL O2/kg of body mass for females and considering an O2 energy equivalent of 20.9 J/mL. For more details,
#'  see Briand et al. (2025) and di Prampero et al. (1981).
#'
#' @param lactic_energy A numeric value representing lactic energy in J/kg.
#' @param sex A character corresponding to the athlete sex (default is `"male"`, other option is `"female"`).
#'
#' @returns A numeric value representing accumulated lactate in mmol/L.
#' @export
#'
#' @examples
#'
#' lactic_energy <- 1000  # Example lactic energy in J/kg
#' convert_lactic_energy_to_acc_lactate(lactic_energy)
#'
#'
convert_lactic_energy_to_acc_lactate <- function(lactic_energy, sex = "male"){

  if(sex == "male"){
    acc_lactate_o2_eq <- 3
  }
  if(sex == "female"){
    acc_lactate_o2_eq <- 2.7
  }

  o2_energy_equivalent <- 20.9

  acc_lactate <- lactic_energy / (acc_lactate_o2_eq * o2_energy_equivalent)

  return(acc_lactate)

}


#' Aerobic Power to VO2 Conversion
#'
#' Converts aerobic power (W/kg) to VO2 (mL/kg/min), considering the energy equivalent
#' of oxygen of 20.9 mL O2/kg/min. For more details, see Briand et al. (2025) and di Prampero (1981).
#'
#' @param aerobic_power A numeric value representing aerobic power in W/kg.
#'
#' @returns A numeric value representing VO2 in mL/kg/min.
#' @export
#'
#' @examples
#' aerobic_power <- 20  # Example aerobic power in W/kg
#' convert_aerobic_power_to_vo2(aerobic_power)
#'
#'
convert_aerobic_power_to_vo2 <- function(aerobic_power){

  o2_energy_equivalent <- 20.9

  vo2 <- aerobic_power * 60 / o2_energy_equivalent

  return(vo2)

}


#' VO2 to Aerobic Power Conversion
#'
#' Converts VO2 (mL/kg/min) to aerobic power (W/kg), considering the energy equivalent
#' of oxygen of 20.9 mL O2/kg/min. For more details, see Briand et al. (2025) and di Prampero (1981).
#'
#' @param vo2 A numeric value representing VO2 in mL/kg/min.
#'
#' @returns A numeric value representing aerobic power in W/kg.
#' @export
#'
#' @examples
#' vo2 <- 50  # Example VO2 in mL/kg/min
#' convert_vo2_to_aerobic_power(vo2)
#'
#'
convert_vo2_to_aerobic_power <- function(vo2){

  o2_energy_equivalent <- 20.9

  aerobic_power <- vo2 * o2_energy_equivalent/60

  return(aerobic_power)
}


