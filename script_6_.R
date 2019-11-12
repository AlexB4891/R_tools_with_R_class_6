tabla <- read.csv("presentacion_4/saiku-export.csv")

names(tabla) <- c("activ","provin","tipo_c","grupo_e",
                  "gran_c","clase","anio","mes","estado",
                  "compras_t","ventas_t","impuesto_c")


library(tidyverse)



combinaciones <- cross2(names(tabla),names(tabla),.filter = `==`) %>% 
  map(set_names,c("x","y")) %>% 
  map(as_tibble) %>% 
  reduce(rbind) 


set.seed(1234)

ind <- sample(1:nrow(tabla),2)

filtradas <-  combinaciones %>%
    split(1:nrow(.)) %>% 
    map(~{
      
      
      
      valor_1 <- tabla[[.x$x]][ind[1]]
      valor_2 <- tabla[[.x$y]][ind[1]]
      
      str_c("tabla %>% 
              filter(",.x$x," == '",valor_1,"',",
                       .x$y," == '",valor_2,"')") %>% 
        rlang::parse_expr(.) %>% 
        eval
    })
    
  

combinaciones %>% 
  rownames_to_column %>% 
  filter(x %in% c("provin","activ"),
         y %in% c("provin","activ"))


aa <- filtradas %>% 
  keep(names(.) %in% c("1","12")) 

aa %>% map(nrow)

aa[["1"]]$provin %>% janitor::tabyl(.)
aa[["1"]]$activ %>% janitor::tabyl(.)
aa[["12"]]$provin %>% janitor::tabyl(.)
aa[["12"]]$activ %>% janitor::tabyl(.)


tabla %>% 
  mutate_at(.vars = c(10:12),
            .funs=funs(log(.)))

tabla %>% 
  mutate_at(.vars = vars("ventas_t","compras_t"),
            .funs=funs(log(.)))

tabla %>% 
  mutate_at(.vars = vars("ventas_t","compras_t"),
            .funs=funs(l =log(.)))

