
# Fancy transformations for ggplot
signpower <- function(pow) { 
  scales::trans_new("signpow", 
                    transform = function(x) sign(x) * abs(x)^(1/pow), 
                    invers = function(x) sign(x) * x^pow,
                    domain = c(-Inf, Inf))
}

signlog10 <- function() { 
  scales::trans_new("signlog10", 
                    transform = function(x) sign(x) * log10(abs(x)), 
                    invers = function(x) sign(x) * 10^x, 
                    domain = c(-Inf, Inf))
}
