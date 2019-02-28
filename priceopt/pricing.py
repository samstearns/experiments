# python optimization

# TODO: Fix class example
# TODO: scenario 2
# additional prices
# TODO: move into classes?

##############################################################################################
# Simulation Coefficients
INTERCEPT = 1
PRICE_COEFFICIENT = -0.2
VARIABLE_COST = 0.50
GAIN = 0.25
LOSS = 0.1
MEMORY = 1

NUM_PERIODS = 12

GRID_SIZE = 12
INCREMENT = 0.25
STARTING = 2.50

WINTER = 0
SPRING = 0.3
SUMMER = 0.5
FALL = 0.1

COL_PRICE = 0

seasonal = [WINTER, WINTER, WINTER, SPRING, SPRING, SPRING, SUMMER, SUMMER, SUMMER, FALL, FALL, FALL]

############################################################################################################
# global variables

# for each month
#    for each reference price:
#       the maximum profit
#       the optimal price
#       next month reference price

# 3-d array of profits, at each month, price, reference price
profitArray = [0]*NUM_PERIODS

# array of dicts, containing the max profit and optimal price for each reference price. 
optimalPriceInfo = [0]*NUM_PERIODS

############################################################################################################
# helper functions

# gets the reference price, given the column in the profit table
# 0-column contains the current price
def getReferencePrice(profitCol):
    ref_price = STARTING + (profitCol-1)*INCREMENT
    return ref_price
    
def getFutureMaxProfit(current_period, price):
    if current_period < NUM_PERIODS:
        # get the future profits in the next period, where the reference price = current price
        # TODO:
        d = optimalPriceInfo[current_period]
        future_profit = d[price][0]
        #future_profit = 0
    else:
        # in the last period. no future profits
        future_profit = 0
    return future_profit

# Given a period, calculate the profit under each price / reference price combination
def calcProfits(current_period):
    monthProfitTable = []
    for i in range(1, GRID_SIZE + 1):
        price = STARTING + INCREMENT * (i-1)
        profitRow = []
        profitRow.append(price)

        for j in range(1, GRID_SIZE + 1):
            ref_price = STARTING + INCREMENT * (j-1)

            if ref_price > price:
                rp_coefficient = GAIN
            else:
                rp_coefficient = LOSS

            # Sales = Intercept + Price Coeff.(Price) + (If RP > Price, Gain Coeff., Loss Coeff.)(Ref.Price - Price)
            unit_sales = INTERCEPT + seasonal[current_period-1] + PRICE_COEFFICIENT * price + rp_coefficient * (ref_price - price)

            current_profit = (price - VARIABLE_COST) * unit_sales
            future_profit = getFutureMaxProfit(current_period, price)
        
            profitRow.append(current_profit + future_profit)
            #print "i = %d, j = %d, Price = %2f, RP = %2f. Profit = %f" % (i, j, price, ref_price, profit)

            #add the row
        monthProfitTable.append(profitRow)

    return monthProfitTable

# for a given period, returns an hashtable of optimal prices for each reference price
def maxProfitInfo(current_period):
    profitDict = {}
    for col in range(1, GRID_SIZE+1):
        maxProfit = -100000
        optimalPrice = -1
        for row in range(0, GRID_SIZE):
            if profitArray[current_period-1][row][col] > maxProfit:
                maxProfit = profitArray[current_period-1][row][col]
                optimalPrice = profitArray[current_period-1][row][COL_PRICE]
        profitDict[getReferencePrice(col)] = [maxProfit, optimalPrice]

    return profitDict

def printResults(starting_ref_price):
    ref_price = starting_ref_price

    #print "Max Profit: %5.2d" % (optimalPriceInfo[0][ref_price][1])
    for period in range(1, NUM_PERIODS+1):
        opt_price = optimalPriceInfo[period-1][ref_price][1]
        print "Period %2d: Reference Price = %5.2f, Optimal Price = %5.2f" % (period, ref_price, opt_price)       
        # update reference price and move to next
        ref_price = opt_price


############################################################################################################
# Control FLOW
############################################################################################################

period = NUM_PERIODS
while (period > 0):
    # calculate profit possibilities
    profitArray[period-1] = calcProfits(period)
    # calculate max prices
    optimalPriceInfo[period-1] = maxProfitInfo(period)
    #work backwards
    period -=1 

# Print Results
start_ref_price = 2.5
printResults(start_ref_price)
