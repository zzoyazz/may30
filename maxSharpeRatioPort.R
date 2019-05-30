#**********************************************************
# Reference: Maximum Sharpe ratio portfolio
# https://systematicinvestor.wordpress.com/?s=portfolio
# **********************************************************


###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Create Efficient Frontier
#******************************************************************     
# create sample historical input assumptions
ia = aa.test.create.ia()
names(ia)
ia$n
# create long-only, fully invested efficient frontier
n = ia$n        

# 0 <= x.i <= 1
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(diag(n), type='>=', b=0, constraints)
constraints = add.constraints(diag(n), type='<=', b=1, constraints)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)        

# create efficient frontier
ef = portopt(ia, constraints, 50, 'Efficient Frontier') 

#*****************************************************************
# Create Plot
#******************************************************************     
# plot efficient frontier
plot.ef(ia, list(ef), transition.map=F)  

# find maximum sharpe portfolio
max(portfolio.return(ef$weight,ia) /  portfolio.risk(ef$weight,ia))

# plot minimum variance portfolio
weight = min.var.portfolio(ia,constraints)  
points(100 * portfolio.risk(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='red')
portfolio.return(weight,ia) /  portfolio.risk(weight,ia)

# plot maximum Sharpe or tangency portfolio
weight = max.sharpe.portfolio()(ia,constraints) 
points(100 * portfolio.risk(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='orange')
portfolio.return(weight,ia) /  portfolio.risk(weight,ia)

plota.legend('Minimum Variance,Maximum Sharpe','red,orange', x='topright')  