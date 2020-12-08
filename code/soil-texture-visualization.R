# Soil texture diagrams
## Generate function for plot where you can pass state to filter
tt.plot <- function(data,state){
  # Filter the data to the chosen state and for the relevant variables
  text <- data %>%
    filter(State==state) %>%
    select("SAND"=Sand,
           "SILT"=Silt,
           "CLAY"=Clay,
           SOMnew) %>%
    drop_na() %>%
    as.data.frame()
  
  # Generate the plot
  plot <- TT.plot(class.sys = "USDA-NCSS.TT",
                  class.p.bg.col = TRUE,
                  tri.data=text,
                  pch=20,
                  z.name="SOMnew",
                  main=state
  )
  
  rm(text)
  return(plot)
}

## Illinois
tt.plot(som.yearmax, "Illinois")
## Indiana
tt.plot(som.yearmax, "Indiana")
## Iowa
tt.plot(som.yearmax, "Iowa")
## Minnesota
tt.plot(som.yearmax, "Minnesota")
## Missouri
tt.plot(som.yearmax, "Missouri")
## Nebraska
tt.plot(som.yearmax, "Nebraska")
## Ohio
tt.plot(som.yearmax, "Ohio")
