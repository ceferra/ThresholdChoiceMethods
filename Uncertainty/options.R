
#PLOTS<-FALSE

####### OUTPUT DRAWING OPTIONS
PLOTS_PER_PAGE <- 4 # 0   # if 0 it doesn't reset the page 
PLOTS_PER_PAGE_VERTICAL <- FALSE  # especially when plotting two or three figures in a column

PDF <- FALSE       # Generate PDF
PDFheight= 7 # 7 by default, so 14 makes it double higher than wide, 5 makes letters bigger (in proportion) for just one plot
PDFwidth= 7 # 7 by default
# 5 x 4 for 1
# 12 x 4 for 3 (ICML) in vertical (with PLOTS_PER_PAGE_VERTICAL TRUE)
# 6 x 6 for 1 (ICML) # One plot one picture!
# 12 x 12 for 6 (ICML)
# 4 x 12 for 2 in wide (ICML)
# 4 x 12 for 3 in wide

PNG <- FALSE     # Generate PNG (if TRUE, the PDF is not generated)
PNGres <- 100       # Resolution as a factor of PDFheight and PDFweight


MARGINSNOTITLE <- c(5,4,1,1)  # Only used when we set NOTITLE true
NOTITLE <- FALSE # Sets main="" and changes margin par(mar=MARGINSNOTITLE)
if (PLOTS) par(mar=c(5.1,4.1,4.1,2.1)) # default margins: par()$mar


CLOSEPDF <- TRUE


SET_YLIM <- TRUE    # Set a fixed height (y-axis) of the plots # NOT USED
YLIM <-1 #1 # 0.5 #1 #0.8        # Height (y-axis) (originally if SET_YLIM is TRUE but now it is always considered)


DRAW_CLASSIFIER <- TRUE # densities

DRAW_ROC_CURVE <- FALSE
TYPE_ROC_CURVE <- "o" # "o" line and circle/point/cross, "l" only lines, "o" only the circle point
PCH_ROC_CURVE <- 4 # NOT APPLICABLE. # NA_integer_ # 15 # NA_integer_: nothing, 1: empty circle, 2: empty triangle, 15: solid square
COLOUR_ROC_CURVE <- "black" # "orange" 
LWD_ROC_CURVE <-  2 # 1: thin, 2:thick 
LTY_ROC_CURVE <- 1 # lty=1 (continuous), lty=2 (dashed)
# PCH_ROC_CURVE <-  0 # 0: none. 
LWD_ROC_HULL <-  2 # 1: thin, 2:thick 
LTY_ROC_HULL <- 2 # lty=1 (continuous), lty=2 (dashed)
PCH_ROC_HULL <-  15 # 1: empty circle, 2: empty triangle, 15: solid square  # IT DOES NOT WORK
COLOR_ROC_DIAGONAL <- "grey"
LWD_ROC_DIAGONAL <-  1 # 1: thin, 2:thick 

# Names of x-axis and y-axis
COSTRATIO <- "cost"
SKEW <- "skew"
LOSS <- "loss"

# Names for titles
EXPECTEDLOSS <- "loss" # "minimum loss" # "expected loss" # "loss" # minimum expected loss
BYCOST <- "by cost"
BYSKEW <- "by skew"
BETA22 <- "(Beta(2,2))"


DRAW_OPTIMAL_CURVES <- FALSE # cost curves CESAR
LTY_OPTIMAL_CURVES <- 2 # lty=1 (continuous), lty=2 (dashed)
PCH_OPTIMAL_CURVES <-  15 # NA_integer_ # 15 # NA_integer_: nothing, 1: empty circle, 2: empty triangle, 15: solid square 
LWD_OPTIMAL_CURVES <-  2 # 1: thin, 2:thick 
COLOUR_OPTIMAL_CURVES <-  "black" # "darkgreen" # "brown" # "black" # 
DRAW_OPTIMAL_CURVES_AGAIN <- FALSE # cost curves (second time and stronger)
COLOUR_OPTIMAL_CURVE_AGAIN <-  "black" # "darkgreen" # "brown" # "black" # 
LWD_OPTIMAL_CURVE_AGAIN <-  2 # 1: thin, 2:thick 

DRAW_ORANGE <- FALSE # ORANGE uniform line.
DRAW_BETA_CURVES <- FALSE  #

RESOLUTION_EMPIRICAL_CURVES <- 1000 #1000 #300
ITERATIONS_BETA_NOISE<- 10 #number of iterations employed in beta noise


DRAW_NATURAL_CURVE <- TRUE
THRESHOLD_CHOICE_METHOD <- 1
# 1 = RATE-DRIVEN BUT VERY COMPLICATED (OLD, PREVIOUSLY KNOWN AS NATURAL CURVE), 
# 2 = PROBABILISTIC CURVE (SCORE-DRIVEN),
# 3 = QUANT CURVE (OBSOLETE)
# 4 = SCORE-UNIFORM
# 5 = RATE-UNIFORM (NOT YET IMPLEMENTED)
# 6 = RATE-DRIVEN (METHOD R1, CLOSEST RATE TO C/Z)   # Not implemented for Z
# 7 = RATE-DRIVEN (METHOD R3, INTERPOLATING)   # Not implemented for Z

DRAW_VERTICAL_RATES <- TRUE

COLOUR_NATURAL_CURVES <- "brown"
LWD_NATURAL_CURVES <-  1 # 1: thin, 2:thick 
PCH_NATURAL_CURVES <- 20 # NA_integer_ # 15 # NA_integer_: nothing, 1: empty circle, 2: empty triangle, 15: solid square, 20: solid circle 
LTY_NATURAL_CURVES <- 1 # lty=1 (continuous), lty=2 (dashed)

NATTHRESFUNZFACTOR <- FALSE
NATTHRESFUNZACCUMULATE <- TRUE
# if FALSE & FALSE, it is very asymmetric. Wrong
# if TRUE & FALSE, it matches the way old Brier curve were made but gives wrong area
# if FALSE & TRUE, it gives correct results (1/12, 7/12)


LEGEND<-FALSE # plots the inp vector at the plot
COSTLINESREP  <- FALSE   # TRUE: Does not eliminate repeated lines. Calculates the red line correctly.
                        # FALSE: Reduces the number of lines (eliminates repeated ones), but it computes the red line in a wrong way if there are ties.

DRAW_COST_LINES <- TRUE  # Cost lines CESAR
DRAW_COST_LINES_AGAIN <- FALSE  # Cost lines
COLOUR_COST_LINES <- "grey"#"black"  ## "blue" # "darkblue" # "darkmagenta" 


DRAW_NONOPTIMALROC_CURVES <- FALSE  # ROC-like curve (it doesn't make sense)

DRAW_LOSSLINE <- FALSE # Average of cost lines (LOSS LINE, red line)
WEIGHTED_LOSSLINE <- FALSE # Weighted Average of cost lines (LOSS LINE, red line)
COLOUR_LOSSLINE <- "red" 
LWD_LOSSLINE <-  2 # 1: thin, 2:thick 
LTY_LOSSLINE <- 1 # lty=1 (continuous), lty=2 (dashed)

DRAW_COSTSQUARE <- FALSE  # The square (grey) CESAR

BRIERCURVES_WITH_FACTOR <- TRUE  # TRUE: The old (wrong) way for skews. FALSE: the right way.
DRAW_PROB_CURVES <- FALSE  # Brier curves
LWD_PROB_CURVES <-  2 # 1: thin, 2:thick
PCH_PROB_CURVES <-  1 # 3 +, 4 x, NA_integer_ # 15 # NA_integer_: nothing, 1: empty circle, 2: empty triangle, 15: solid square   
TYPE_PROB_CURVES <- "o" # "o" line and circle, "l" only lines, "o" only the circle point
DRAW_PROB_CURVES_AGAIN <- FALSE  # Brier curves (again)
COLOUR_PROB_CURVE_AGAIN <- "brown" # "darkmagenta" # "black" # "darkblue" # "darkmagenta" 
LWD_PROB_CURVE_AGAIN <-  2 # 1: thin, 2:thick

DRAW_PROB_CURVES_PERCLASS <- FALSE # Draw prob (Brier) curves (positive and negative) separately.


PLOT_SPACE <- TRUE # Must be true except when putting several curves together on the same graph CESAR
PLOT_ROCSPACE <- TRUE # Same for ROC space


OVERLAP_PROB_OPTIMAL <- TRUE  # Overlap Brier curves and cost curves on the same plot. If no Brier curves, this should be ENABLED.
OVERLAP_NONOPTIMAL <- TRUE # Overlap previous plot with cost lines


DRAW_COSTRATIO_PLOTS <- TRUE
DRAW_SKEW_PLOTS <- FALSE


ALPHA_PLOT <- 2 # 0.01 (logloss?)
BETA_PLOT <- 2 # 0.01 (logloss?)


DRAW_HIST<-FALSE



if (!PLOTS)
{
  DRAW_COST_LINES<-FALSE
  PLOT_SPACE<-FALSE
  DRAW_COSTRATIO_PLOTS <- FALSE
}

############# END OUTPUT OPTIONS ##############
###############################################
