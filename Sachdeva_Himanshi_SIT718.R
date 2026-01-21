# Question 2
# ClothProduction


library(lpSolveAPI)

clothProduction <- make.lp(12, 9) # initialise 12 constaints and 9 variables

lp.control(clothProduction, sense= "maximize")  # Set control parameters: "minimize" or "maximize" 

#  Setting the objective function 
set.objfn(clothProduction, c(25,	10,	5,	22,	7,	2,	27,	12, 7))

# updating the left hand side of constaints one-by-one
set.row(clothProduction, 1, c(1,1,1,0,0,0,0,0,0), indices = c(1:9))
set.row(clothProduction, 2, c(0,0,0,1,1,1,0,0,0), indices = c(1:9))
set.row(clothProduction, 3, c(0,0,0,0,0,0,1,1,1), indices = c(1:9))

#Cotton Constraints
set.row(clothProduction, 4, c(0.45,-0.55,-0.55,0,0,0,0,0,0), indices = c(1:9)) #Spring
set.row(clothProduction, 5, c(0,0,0,0.55,-0.45,-0.45,0,0,0), indices = c(1:9)) #Autumn
set.row(clothProduction, 6, c(0,0,0,0,0,0,0.70,-0.30,-0.30), indices = c(1:9)) #Winter

#Wool Constraints
set.row(clothProduction, 7, c(-0.30,0.70,-0.30,0,0,0,0,0,0), indices = c(1:9)) #Spring
set.row(clothProduction, 8, c(0,0,0,-0.40,0.60,-0.40,0,0,0), indices = c(1:9)) #Autumn
set.row(clothProduction, 9, c(0,0,0,0,0,0,-0.50,0.50,-0.50), indices = c(1:9))

#Silk Constraints
set.row(clothProduction, 10, c(-0.01,-0.01,0.99,0,0,0,0,0,0), indices = c(1:9)) #Spring
set.row(clothProduction, 11, c(0,0,0,-0.02,-0.02,0.98,0,0,0), indices = c(1:9)) #Autumn
set.row(clothProduction, 12, c(0,0,0,0,0,0,-0.03,-0.03,0.97), indices = c(1:9)) #Winter

# updating the right hand side of constaints 
set.rhs(clothProduction, c(3200,3800,4200,0,0,0,0,0,0,0,0,0))

# updating the symbols
set.constr.type(clothProduction, c("<=",	"<=",	"<=", ">=",	">=",	">=", ">=",	">=",	">=", ">=",	">=",	">="))

set.type(clothProduction, c(1:9),"int")

set.bounds(clothProduction, lower = rep(0, 9), upper = rep(Inf, 9)) #Non-negativity constraints

# write.lp(clothProduction, filename="test.lp")  Use write.lp to print out larger LPs. 
#  It produces a text file, which you can examine with any text editor.

solve(clothProduction) # http://lpsolve.sourceforge.net/5.5/solve.htm

objvalue<-get.objective(clothProduction)
objvalue
solution<-get.variables(clothProduction)
solution

##############################################

# Question 3
# Game Theory

# Player I game #

library(lpSolveAPI)

player1 <- make.lp(0, 7)

lp.control(player1, sense= "maximize")  

set.objfn(player1, c(0, 0, 0, 0, 0, 0, 1)) # x1, x2, x3, x4, x5, x6, v

add.constraint(player1, c(0, 0, 0, -75, 0, 75, 1), "<=", 0)
add.constraint(player1, c(0, 0, -75, 0, 75, 0, 1), "<=", 0)
add.constraint(player1, c(0, 75, 0, 0, -75, 0, 1), "<=", 0)
add.constraint(player1, c(75, 0, 0, 0, 0, -75, 1), "<=", 0)
add.constraint(player1, c(0, -75, 75, 0, 0, 0, 1), "<=", 0)
add.constraint(player1, c(-75, 0, 0, 75, 0, 0, 1), "<=", 0)
add.constraint(player1, c(1, 1, 1, 1, 1, 1, 0), "=", 1)

set.bounds(player1, lower = c(0, 0, 0, 0, 0, 0, -Inf))

RowNames <- c("WRB", "WBR", "RWB","RBW", "BRW", "BWR", "Probability")

ColNames <- c("WRB", "WBR", "RWB","RBW", "BRW", "BWR", "v")

dimnames(player1) <- list(RowNames, ColNames)

player1

solve(player1) # http://lpsolve.sourceforge.net/5.5/solve.htm

get.objective(player1)

get.variables(player1)

get.constraints(player1)



# Player II game #

library(lpSolveAPI)

player2 <- make.lp(0, 7)

lp.control(player2, sense= "minimize")  

set.objfn(player2, c(0, 0, 0, 0, 0, 0, 1)) # y1, y2, y3, y4, y5, y6, v

add.constraint(player2, c(0, 0, 0, 75, 0, -75, 1), ">=", 0)
add.constraint(player2, c(0, 0, 75, 0, -75, 0, 1), ">=", 0)
add.constraint(player2, c(0, -75, 0, 0, 75, 0, 1), ">=", 0)
add.constraint(player2, c(-75, 0, 0, 0, 0, 75, 1), ">=", 0)
add.constraint(player2, c(0, 75, -75, 0, 0, 0, 1), ">=", 0)
add.constraint(player2, c(75, 0, 0, -75, 0, 0, 1), ">=", 0)
add.constraint(player2, c(1, 1, 1, 1, 1, 1, 0), "=", 1)

set.bounds(player2, lower = c(0, 0, 0, 0, 0, 0, -Inf))

RowNames <- c("WRB", "WBR", "RWB","RBW", "BRW", "BWR", "Probability")

ColNames <- c("WRB", "WBR", "RWB","RBW", "BRW", "BWR", "v")

dimnames(player2) <- list(RowNames, ColNames)

player2

solve(player2) # http://lpsolve.sourceforge.net/5.5/solve.htm

get.objective(player2)

get.variables(player2)

get.constraints(player2)

