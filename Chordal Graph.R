library(circlize)

par(mar = c(2, 2, 3, 10))  


data_matrix <- matrix(c(
  0, 0, 0,    
  0, 0, 0,     
  0, 0, 28,   
  55, 0, 0,   
  0, 52, 0,    
  0, 0, 27,   
  45, 0, 0,    
  0, 48, 0,    
  0, 0, 45    
), nrow = 9, ncol = 3, byrow = TRUE)

rownames(data_matrix) <- c("1_LDOC", "2_LDOC", "3_LDOC", 
                           "1_SLDOC", "2_SLDOC", "3_SLDOC",
                           "1_RDOC", "2_RDOC", "3_RDOC")
colnames(data_matrix) <- c("1", "2", "3")

colors <- c("#FF6B6B", "#FFA8A8", "#FF5252", 
            "#4ECDC4", "#88D9D3", "#1AABA0",  
            "#45B7D1", "#8BD1E5", "#1A9DC2")  
names(colors) <- rownames(data_matrix)


chordDiagram(data_matrix, 
             grid.col = colors,
             transparency = 0.3,
             annotationTrack = c("name", "grid"),
             preAllocateTracks = list(track.height = 0.2))


title("")


legend(x = 1.15, y = 0.9, 
       legend = c("1 LDOC (0%)", "2 LDOC (0%)", "3 LDOC (28%)",
                  "1 SLDOC (55%)", "2 SLDOC (52%)", "3 SLDOC (27%)",
                  "1 RDOC (45%)", "2 RDOC (48%)", "3 RDOC (45%)"),
       fill = colors, 
       cex = 0.65,  
       border = NA,
       bty = "n",
       xpd = TRUE)  
