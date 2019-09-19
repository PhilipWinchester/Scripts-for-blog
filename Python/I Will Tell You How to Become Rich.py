# -*- coding: utf-8 -*-
import pandas as pd
import numpy as np
# So that the whole np array is shown when printed 
import sys
np.set_printoptions(threshold=sys.maxsize)    

# Reading Index Values
Index_Values = pd.read_excel('/Users/philipwinchester/Documents/Blog/Blog/FTSE model/historic-ftse-index-values.xlsx', sheet_name='Index Values', parse_cols = "B:L", 
    skiprows = 15, header = 1, skipfooter=2)
    
Gold_Price = pd.read_excel('/Users/philipwinchester/Documents/Blog/Blog/FTSE model/Gold.xlsx',sheet_name='Daily', parse_cols = "A:B")

Nikkei225_Index = pd.read_excel('/Users/philipwinchester/Documents/Blog/Blog/FTSE model/nikkei-225-index-historical-chart-data.xlsx',sheet_name='Sheet 1', parse_cols = "A:B")

# Converting some indecies into lists
FTSE100_Index = Index_Values['FTSE 100'].tolist()
FTSE350_Index = Index_Values['FTSE 350'].tolist()
Gold_Value = Gold_Price["Pound sterling"].tolist()[::-1]
Nikkei225_Value = Nikkei225_Index["value"].tolist()[::-1]

def zerolistmaker(n):
    """Function which returns a list with n 0's"""
    listofzeros = [0] * n
    return listofzeros

def Investment_Function(Index_List, Start_Money, Cost_Per_Transaction, Sell_Number, Buy_Number):
    """Function which takes an Index List, how much money you have to start with (float), the Cost per transaction (float), how many ups it takes for you to sell (int) 
    and how many downs it takes for you to buy (int)
    Returns a matrix. 
    First colum: Index
    Second column: Units held at each time, 
    Third column: Value held at each time
    Fourth column: An explenation of what is happening"""
    
    # Making the units and value lists, filling with 0's.
    Units = zerolistmaker(len(Index_List))
    Value = zerolistmaker(len(Index_List))
    Exp = zerolistmaker(len(Index_List))
    
    # Caluclating how many units we start with filling the first entries in the lists
    Start_Units = Start_Money/Index_List[-1]
    Units[-1] = Start_Units
    Value[-1] = Start_Money
    Exp[-1] = "Start"
    
    # "In" indicates if we are invested our not. "Count" will indicate how many ups/downs we have had in sucession
    In = True
    Count = 0
    
    # Working our way up the Index_List to work out when to buy and sell
    for i in range(-2,-len(Index_List)-1,-1):
        # We are In, Index goes up and we are below the selling point, so we dont sell
        if In and Index_List[i] > Index_List[i+1] and Count < (Sell_Number-1):
            Count += 1
            Units[i] = Units[i+1]
            Value[i] = Units[i]*Index_List[i]
            Exp[i] = "Up, but hold"
        # We are In, Index goes up and we hit the selling point, so we sell
        elif In and Index_List[i] > Index_List[i+1] and Count == (Sell_Number-1):
            Count = 0
            Units[i] = 0
            Value[i] = Units[i+1]*Index_List[i] - Cost_Per_Transaction
            # Returns 0 if we have run out of money
            if Value[i] < 0:
                Value[i] = 0
                Units[i] = 0
                return(np.column_stack((Index_List, Units, Value, Exp)))
            In = False
            Exp[i] = "Sell"
        # We are In, Index goes down, so we dont sell
        elif In and Index_List[i] < Index_List[i+1]:
            Count = 0
            Units[i] = Units[i+1]
            Value[i] = Units[i]*Index_List[i]
            Exp[i] = "Down, so hold"
        # We are not In, Index goes down and we we are bellow the buying point, so we don't buy
        elif not(In) and Index_List[i] < Index_List[i+1] and Count < (Buy_Number-1):
            Count += 1
            Value[i] = Value[i+1]
            Exp[i] = "Down, but don't buy"
        # We are not In, Index goes down and we hit the buying point, so we buy
        elif not(In) and Index_List[i] < Index_List[i+1] and Count == (Buy_Number-1):
            Count = 0
            Units[i] = (Value[i+1] - Cost_Per_Transaction)/Index_List[i]
            Value[i] = Value[i+1] - Cost_Per_Transaction
            # Returns 0 if we have run out of money
            if Value[i] < 0:
                Value[i] = 0
                Units[i] = 0
                return(np.column_stack((Index_List, Units, Value, Exp)))
            In = True
            Exp[i] = "Buy"
        # We are not In, Index goes up, so we don't buy
        elif not(In) and Index_List[i] > Index_List[i+1]:
            Count = 0
            Value[i] = Value[i+1]
            Exp[i] = "Up, so don't buy"
        # If the Index does not change, just copy what we had before
        elif Index_List[i] == Index_List[i+1]:
            Value[i] = Value[i+1]
            Units[i] = Units[i+1]
            Exp[i] = "Index unchanged"
        else:
            print("Something funny is happening at i =" + str(i))

    # Returning the matrix with Index Value, Units, Value and the explanation
    return(np.column_stack((Index_List, Units, Value, Exp)))
    
def Max_Ups(Index_List):
    """Function which takes a list of numbers and return the the maximum numbers of sucessive ups working from right to left"""  
    Ans = 0
    Compar = 0
    
    # Working our way up the Index_List
    for i in range(-2,-len(Index_List)-1,-1):         
        if Index_List[i] > Index_List[i+1]:
            Compar += 1
        else:
            Ans = max(Compar, Ans)
            Compar = 0
            
    Ans = max(Compar, Ans)
    return(Ans)
    
def Max_Downs(Index_List):
    """Function which takes a list of numbers and return the the maximum numbers of sucessive downs working from right to left"""
    Ans = 0
    Compar = 0
    
    # Working our way up the Index_List
    for i in range(-2,-len(Index_List)-1,-1):         
        if Index_List[i] < Index_List[i+1]:
            Compar += 1
        else:
            Ans = max(Compar, Ans)
            Compar = 0
            
    Ans = max(Compar, Ans)
    return(Ans)
    
from pandas import DataFrame

def DF_Builder(Index_List, Start_Money, Cost_Per_Transaction):
    """Function which takes and Index_List, Start_Money and Cost_Per_Transaction
    Returns a Dataframe 
    Column one and two: All pairs of possible Sell_Value and Buy_Value
    Column three: The Final value we would have adopting that strategy
    Column four: How much better (%) each strategy does compaired to Buy and Leave"""
     
    # Finding the maximum numbers for Sell_Number and Buy_Number, taking one less than the actual max
    Max_Buy_Number = Max_Downs(Index_List) - 1
    Max_Sell_Number = Max_Ups(Index_List) - 1
    
    # Making the lists for Sell_Number and Buy_Number
    Sell_Number_List = zerolistmaker((Max_Buy_Number)*(Max_Sell_Number))
    Buy_Number_List = zerolistmaker((Max_Buy_Number)*(Max_Sell_Number))
    
    # Filling the Sell and Buy lists, taking 1 as the minumum
    i = 0
    for Buy in range(1,Max_Buy_Number+1):
        for Sell in range(1,Max_Sell_Number+1):
            Sell_Number_List[i] = Sell
            Buy_Number_List[i] = Buy
            i += 1
    
    Final_Value_List = zerolistmaker(len(Sell_Number_List))
    Performance_List = zerolistmaker(len(Sell_Number_List))
    
    # Calculating the Final_Value and Performance for each pair of Sell_Value and Buy_Value
    for i in range(len(Final_Value_List)):
        Final_Value_List[i] = Investment_Function(Index_List, Start_Money, Cost_Per_Transaction, Sell_Number_List[i], Buy_Number_List[i])[0,2]
        Performance_List[i] = Final_Value_List[i].astype(np.float)/(Start_Money*Index_List[1]/Index_List[-1]) - 1
    
    # Constructing the DataFrame which we output
    Data = {'Sell_Number': Sell_Number_List, 'Buy_Number': Buy_Number_List, 'Final_Value': Final_Value_List, "Performance (%)": Performance_List}
    
    return(DataFrame(Data, columns = ['Sell_Number', 'Buy_Number', 'Final_Value', "Performance (%)"])) 


import matplotlib.pyplot as plt
import seaborn as sns

def Make_Heatmap(Index_List, Start_Money, Cost_Per_Transaction, Index_Name):
    """ Funtion which takes and Index_List, Start_Money, Cost_Per_Transaction and Index_Name and returns a heatmap of performance"""
    plt.figure(figsize=(9,9))
    Pivot_Table = DF_Builder(Index_List, Start_Money, Cost_Per_Transaction).pivot('Sell_Number','Buy_Number','Performance (%)')
    print(type(Pivot_Table))
    plt.xlabel('Sell_Number', size = 15)
    plt.ylabel('Buy_Number', size = 15)
    plt.title('Performance against Buy and Hold ' + Index_Name , size = 15) 
    sns.heatmap(Pivot_Table, annot=True, fmt=".1f", linewidths=.5, square = True, cmap = "YlGnBu")
    return(plt.show())

import datetime as datetime

def Plot_Comparison(Dates, Index_List, Start_Money, Cost_Per_Transaction, Sell_Number, Buy_Number, Index_Name, Min_Date = '1900-01-01', Max_Date = '2020-01-01',New_Calculation=True, MaxY = False, MinY = False):
    """Function which takes an Index_List, stratgy and compaires the two in a plot
    There is also the option to restrivt the dates we look at
    MaxY restricts the Y-axis"""
    
    # Seeing if there are any restrictions on dates
    if New_Calculation:
        if pd.to_datetime(Min_Date) in Dates and pd.to_datetime(Max_Date) in Dates:
            Lower_Index = Dates.index(pd.to_datetime(Min_Date))
            Upper_Index = Dates.index(pd.to_datetime(Max_Date))
            Dates = Dates[Upper_Index: Lower_Index+1]
            Index_List = Index_List[Upper_Index: Lower_Index+1]
        elif pd.to_datetime(Min_Date) in Dates:
            Lower_Index = Dates.index(pd.to_datetime(Min_Date))
            Dates = Dates[: Lower_Index+1]
            Index_List = Index_List[: Lower_Index+1]
        elif pd.to_datetime(Max_Date) in Dates:
            Upper_Index = Dates.index(pd.to_datetime(Max_Date))
            Dates = Dates = Dates[Upper_Index :]
            Index_List = Index_List[Upper_Index :]
        
    Dates = pd.to_datetime(Dates)
    
    # Bulding the dateframe we will input to our plot
    DF = pd.DataFrame()
    DF[Index_Name] = np.asarray(Index_List)*Start_Money/Index_List[-1]
    DF['Strategy(Buy:'+ str(Buy_Number)+ ' Sell: '+str(Sell_Number) +')' ] = Investment_Function(Index_List, Start_Money, Cost_Per_Transaction, Sell_Number, Buy_Number)[:,2]
    DF = DF.set_index(Dates)

    # Making the plot
    fig, ax = plt.subplots()
    axes = plt.gca()
    
    # If we are doing a new calculation, we set the range of dates equal to what was calcualted above. 
    # If no new calculation, we use the full calcualtion, but only view the range as given to the function
    if New_Calculation:
        axes.set_xlim([Dates[-1],Dates[0]])
    elif not(New_Calculation):
        axes.set_xlim([pd.to_datetime(Min_Date),pd.to_datetime(Max_Date)])
        
    if type(MaxY) == type(1.0) and type(MinY):
        axes.set_ylim([MinY,MaxY])
    
    
    fig.subplots_adjust(bottom=0.15)
    plt.xticks(rotation=90)
    plt.xlabel("Year")
    plt.ylabel("Value of investment")
    plt.plot(DF)
    ax.legend([Index_Name, 'Strategy (Sell: '+ str(Sell_Number)+ ' Buy: '+str(Buy_Number) +')'])
    return(plt.show())
    
def Investment_Function2(Index_List, Start_Money, Cost_Per_Transaction, Sell_Percentage, Buy_Percentage, Lookback):
    """Function which takes an Index_List, the money we start with, the transaction cost and our strategy. 
    Sell_Percentage is float >0, Buy_Percentage is float <0. 
    Returns a matrix with 5 columns
    Column 1: The index
    Column 2: Number of units held
    Column 3: Value of investment
    Column 4: What the comparsion is
    Column 5: An explanation of what is happening"""
    
    # Making the units and value lists, filling with 0's.
    Units = zerolistmaker(len(Index_List))
    Value = zerolistmaker(len(Index_List))
    Exp = zerolistmaker(len(Index_List))
    Comparison = zerolistmaker(len(Index_List))
    
    # Caluclating how many units we start with filling the first entries in the lists
    Start_Units = Start_Money/Index_List[-1]
    Units[-1] = Start_Units
    Value[-1] = Start_Money
    Exp[-1] = "Start"
    Comparison[-1] = 1.0 
    
    # "In" indicates if we are invested our not.
    In = True
    
    # Working our way up the Index_List to work out when to buy and sell
    for i in range(-2,-len(Index_List)-1,-1):
        # We haven't gone far enough to start looking back
        if i>=-Lookback:
            Units[i] = Start_Units
            Value[i] = Start_Units*Index_List[i]
            Exp[i] = "Not gone far enough"
            Comparison[i] = 1.0
        # We can start building our comparison list
        else:
            Comparison[i] = Index_List[i]/Index_List[i+Lookback]
            # We are In and Index has done well enough over the Lookback, so we sell
            if In and Comparison[i] >= (Sell_Percentage+1):
                In = False
                Units[i] = 0
                Value[i] = Index_List[i]*Units[i+1] - Cost_Per_Transaction
                # Returns 0 if we have run out of money
                if Value[i] < 0:
                    Value[i] = 0
                    Units[i] = 0
                    return(np.column_stack((Index_List, Units, Value, Comparison,Exp)))
                    Exp[i] = "Run out of money"
                Exp[i] = "Index has done well, sell"
            # We are In and Index has not done well enough over the Lookback, so we do nothing
            elif In and Comparison[i] < (Sell_Percentage+1):
                Units[i] = Units[i+1]
                Value[i] = Units[i]*Index_List[i]
                Exp[i] = "Index has not done well enough to sell"
            # We are not In and Index has done poorly enough over the Lookback, so we buy
            elif not(In) and Comparison[i] <= (Buy_Percentage+1):
                In = True
                Units[i] = (Value[i+1]-Cost_Per_Transaction)/Index_List[i]
                Value[i] = Value[i+1] - Cost_Per_Transaction
                # Returns 0 if we have run out of money
                if Value[i] < 0:
                    Value[i] = 0
                    Units[i] = 0
                    return(np.column_stack((Index_List, Units, Value, Comparison,Exp)))
                    Exp[i] = "Run out of money"
                Exp[i] = "Index has done badly enough, se we buy"
            # We are not In and Index has not done poorly enough over the Lookback, so we do nothing
            elif not(In) and Comparison[i] > (Buy_Percentage+1):
                Units[i] = 0
                Value[i] = Value[i+1]
                Exp[i] = "Index has not done badly enough to buy"
            else:
                print("Something strange has happened")
                
    # Returning the matrix 
    return(np.column_stack((Index_List, Units, Value, Comparison,Exp)))


def Max_Sell_Percentage_Func(Index_List, Lookback):
    """Function which takes an Index_List, Lookback and returns the maximum Sell_Percentage we could consider"""
    Comparison = zerolistmaker(len(Index_List))
    
    for i in range(-1,-len(Index_List)-1,-1):
        if i>=-Lookback:
            Comparison[i] = 1.0
        else:
            Comparison[i] = Index_List[i]/Index_List[i+Lookback]
    
    return(max(Comparison)-1)
    
def Max_Buy_Percentage_Func(Index_List, Lookback):
    """Function which takes an Index_List, Lookback and returns the maximum Buy_Percentage we could consider"""
    Comparison = zerolistmaker(len(Index_List))
    
    for i in range(-1,-len(Index_List)-1,-1):
        if i>=-Lookback:
            Comparison[i] = 1.0
        else:
            Comparison[i] = Index_List[i]/Index_List[i+Lookback]
    
    return(min(Comparison)-1)
    
def DF_Builder2(Index_List, Start_Money, Cost_Per_Transaction, Lookback, Scenario_Number):
    """Function which takes and Index_List, Start_Money and Cost_Per_Transaction
    Returns a Dataframe 
    Column 1 and 2: All pairs of possible Sell_Percenrage and Buy_Percentage
    Column three: The Final value we would have adopting that strategy
    Column four: How much better (%) each strategy does compaired to Buy and Leave"""

    # Finding the maximum numbers for Sell_Percentage and Buy_Percentage
    Max_Buy_Percentage = Max_Buy_Percentage_Func(Index_List, Lookback)
    Max_Sell_Percentage = Max_Sell_Percentage_Func(Index_List, Lookback)
    
    # Making the lists for Sell_Percentage and Buy_Percentage
    Sell_Percentage_List = zerolistmaker(Scenario_Number*Scenario_Number)
    Buy_Percentage_List = zerolistmaker(Scenario_Number*Scenario_Number)
    
    # Finding the intervals between strategies
    Sell_Interval = (Max_Sell_Percentage)/(Scenario_Number - 1)
    Buy_Interval = (Max_Buy_Percentage )/(Scenario_Number - 1)
      
    # Filling the Sell and Buy lists
    i = 0
    for Buy in range(0,Scenario_Number):
        for Sell in range(0,Scenario_Number):
            Sell_Percentage_List[i] = Max_Sell_Percentage - Sell * Sell_Interval
            Buy_Percentage_List[i] = Max_Buy_Percentage - Buy * Buy_Interval
            i += 1
        
    Final_Value_List = zerolistmaker(len(Sell_Percentage_List))
    Performance_List = zerolistmaker(len(Sell_Percentage_List))
    
    # Calculating the Final_Value and Performance for each pair of Sell_Percentage and Buy_Percentage
    for i in range(len(Final_Value_List)):
        Final_Value_List[i] = Investment_Function2(Index_List, Start_Money, Cost_Per_Transaction, Sell_Percentage_List[i], Buy_Percentage_List[i], Lookback)[0,2]
        Performance_List[i] = Final_Value_List[i].astype(np.float)/(Start_Money*Index_List[1]/Index_List[-1]) - 1
    
    # Constructing the DataFrame which we output
    Data = {'Sell_Percentage': [ '%.2f' % elem for elem in Sell_Percentage_List ], 'Buy_Percentage': [ '%.2f' % elem for elem in Buy_Percentage_List ], 'Final_Value': Final_Value_List, "Performance (%)": Performance_List}
    
    return(DataFrame(Data, columns = ['Sell_Percentage', 'Buy_Percentage', 'Final_Value', "Performance (%)"]))

def Make_Heatmap2(Index_List, Start_Money, Cost_Per_Transaction, Lookback, Scenario_Number, Index_Name):
    """ Funtion which takes and Index_List, Start_Money, Cost_Per_Transaction and Index_Name and returns a heatmap of performance"""
    plt.figure(figsize=(9,9))
    Pivot_Table = DF_Builder2(Index_List, Start_Money, Cost_Per_Transaction, Lookback, Scenario_Number).pivot('Sell_Percentage','Buy_Percentage','Performance (%)')
    plt.xlabel('Sell_Percentage', size = 15)
    plt.ylabel('Buy_Percentage', size = 15)
    plt.title('Performance against Buy and Hold ' + Index_Name , size = 15) 
    sns.heatmap(Pivot_Table, annot=True, fmt=".1f", linewidths=.5, square = True, cmap = "YlGnBu")
    return(plt.show())
    
def Plot_Comparison2(Dates, Index_List, Start_Money, Cost_Per_Transaction, Sell_Percentage, Buy_Percentage, Lookback, Index_Name, Min_Date = '1900-01-01', Max_Date = '2020-01-01', New_Calculation = True):
    """Function which takes an Index_List, stratgy and compaires the two in a plot
    There is also the option to restrivt the dates we look at"""
    
    
    # Seeing if we have any restriction on dates, only if we want to do new calcualtion
    if New_Calculation:
        if pd.to_datetime(Min_Date) in Dates and pd.to_datetime(Max_Date) in Dates:
            Lower_Index = Dates.index(pd.to_datetime(Min_Date))
            Upper_Index = Dates.index(pd.to_datetime(Max_Date))
            Dates = Dates[Upper_Index: Lower_Index+1]
            Index_List = Index_List[Upper_Index: Lower_Index+1]
        elif pd.to_datetime(Min_Date) in Dates:
            print('hej')
            Lower_Index = Dates.index(pd.to_datetime(Min_Date))
            Dates = Dates[: Lower_Index+1]
            Index_List = Index_List[: Lower_Index+1]
        elif pd.to_datetime(Max_Date) in Dates:
            print('hej')
            Upper_Index = Dates.index(pd.to_datetime(Max_Date))
            Dates = Dates = Dates[Upper_Index :]
            Index_List = Index_List[Upper_Index :]
        
    Dates = pd.to_datetime(Dates)
    
    DF = pd.DataFrame()
    DF[Index_Name] = np.asarray(Index_List)*Start_Money/Index_List[-1]
    DF['Strategy(Buy:'+ str(Buy_Percentage)+ ' Sell: '+str(Sell_Percentage) +')' ] = Investment_Function2(Index_List, Start_Money, Cost_Per_Transaction, Sell_Percentage, Buy_Percentage, Lookback)[:,2]
    DF = DF.set_index(Dates)

    fig, ax = plt.subplots()
    axes = plt.gca()
    
    # If we are doing a new calculation, we set the range of dates equal to what was calcualted above. 
    # If no new calculation, we use the full calcualtion, but only view the range as given to the function
    if New_Calculation:
        axes.set_xlim([Dates[-1],Dates[0]])
    elif not(New_Calculation):
        axes.set_xlim([pd.to_datetime(Min_Date),pd.to_datetime(Max_Date)])
    
    fig.subplots_adjust(bottom=0.15)
    plt.xticks(rotation=90)
    plt.xlabel("Year")
    plt.ylabel("Value of investment")
    plt.plot(DF)
    ax.legend([Index_Name, 'Strategy (Sell: '+ str(Sell_Percentage)+ ' Buy: '+str(Buy_Percentage) +')'])
    return(plt.show())


# --- prints ---

#Plot_Comparison(Nikkei225_Index['date'].tolist()[::-1][:6496], Nikkei225_Value[:6496], 10000, 1, 2, 3, 'Nikkei 225')
#Plot_Comparison(Gold_Price['Date'].tolist()[::-1], Gold_Value, 1000, 0.1, 2, 3, 'Gold')
#Make_Heatmap(Index_List = Nikkei225_Value[:6496], Start_Money = 10000, Cost_Per_Transaction = 1, Index_Name = 'Nikkei 225 Index')
#Make_Heatmap(Index_List = Gold_Value, Start_Money = 10000, Cost_Per_Transaction = 1, Index_Name = 'Gold Price')

#print(Investment_Function(FTSE100_Index, 10000, 1, 2, 2))

#print(DF_Builder2(FTSE100_Index, 10000, 1, 30, 10))
#Make_Heatmap2(FTSE100_Index, 10000, 1, 30, 10, 'FTSE 100 Index')
#Make_Heatmap2(Nikkei225_Value[:6496], 10000, 1, 30, 10, 'Nikkei 225 Index')

#print(Make_Heatmap(Index_List = FTSE100_Index, Start_Money = 10000, Cost_Per_Transaction = 1, Index_Name = 'FTSE 100 Index'))
#print(DF_Builder(FTSE100_Index, Start_Money = 10000.0, Cost_Per_Transaction = 1.0))



#Plot_Comparison2(Nikkei225_Index['date'].tolist()[::-1][:6496], Nikkei225_Value[:6496], 10000, 1, 0.04, -0.04, 30, 'Nikkei 225') #, '2018-01-02', '2019-03-11', New_Calculation = False)
Plot_Comparison2(Index_Values['Date'].tolist(), FTSE100_Index, 10000, 1, 0.04, -0.04, 30, 'FTSE 100')
#Plot_Comparison(Index_Values['Date'].tolist(), FTSE100_Index, 10000, 1, 8, 8, 'FTSE 100')
#print(Index_Values['Date'].tolist().index(pd.to_datetime('2018-01-02')))
#print(Index_Values['Date'].tolist()[300:356+1])

