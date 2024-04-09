# Custom theme for Slidy presentations
theme_slidy_presentation <- function() {
  theme_minimal(base_size = 18) + 
    theme(
      text = element_text(family = "Arial", color = "#333333"),
      plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
      plot.subtitle = element_text(size = 20, hjust = 0.5),
      plot.caption = element_text(size = 16),
      axis.title = element_text(size = 20, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(size = 18),
      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 18),
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.major = element_line(color = "#cccccc"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), # Remove plot border
      panel.background = element_rect(fill = "white", color = NA)
    )
}

  theme_my<-function(size=16) {
  theme_minimal() +
    theme(text=element_text(size=size),
          plot.title=element_text(face="bold", size=size+2),
          axis.title=element_text(size=size-1),
          legend.position="bottom",
          legend.title=element_blank(),
          panel.grid.major=element_line(color="gray50"),
          panel.grid.minor=element_blank())}

theme_set(theme_my())  

my_smooth <- function(data,mapping,...){
  ggplot(data=data,mapping=mapping)+
    geom_point(...,size=1.5)+
    geom_smooth(...,method="lm",se=FALSE)}

my_density <- function(data,mapping,...){
  ggplot(data=data,mapping=mapping)+
    geom_density(...,lwd=1)}
