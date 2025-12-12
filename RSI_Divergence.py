import pandas as pd
import matplotlib.pyplot as plt
import talib as ta
import random
import numpy as np
from scipy.signal import find_peaks 
from scipy import stats

from statsmodels.graphics.factorplots import interaction_plot
from statsmodels.formula.api import ols
import statsmodels.api as sm

random.seed(42)
spy = pd.read_csv("spy_daily_2020.csv")

# remove the 'vwap' column
spy = spy.drop(columns=['vwap'])

# convert timestamps
spy['timestamp'] = pd.to_datetime(spy['timestamp'], utc=True)
spy['timestamp'] = spy['timestamp'].dt.tz_convert('America/New_York')

def calculate_signals(group):
    group['RSI'] = ta.RSI(group['close'], timeperiod=14) # using 14-day for standard divergence
    group['MA'] = ta.EMA(group['close'], timeperiod=50)
    group['MA_change'] = (group['open'] - group['MA']) / group['open']
    return group

def find_divergences(group, peak_distance):

    print(f'Analyzing {group['symbol'].iloc[0]} for divergences:')

    # Bearish Divergence: look for peaks in the 'high' price
    price_peak_indices, _ = find_peaks(group['high'], distance=peak_distance)
    rsi_peak_indices, _ = find_peaks(group['RSI'], distance=peak_distance)

    # Bullish Divergence: look for peaks on the negative data, which are the valleys
    price_valley_indices, _ = find_peaks(-group['low'], distance=peak_distance)
    rsi_valley_indices, _ = find_peaks(-group['RSI'], distance=peak_distance)

    bullish_divergences = []
    bearish_divergences = []

    # check for Bullish Divergence (price: lower low, RSI: higher low)
    for i in range(1, len(price_valley_indices)):
        current_valley_idx = price_valley_indices[i]
        prev_valley_idx = price_valley_indices[i-1]

        current_price_low = group['low'][current_valley_idx]
        prev_price_low = group['low'][prev_valley_idx]
        
        current_rsi_low = group['RSI'][current_valley_idx]
        prev_rsi_low = group['RSI'][prev_valley_idx]

        # price makes a LOWER LOW, but RSI makes a HIGHER LOW
        if (current_price_low < prev_price_low) and (current_rsi_low > prev_rsi_low):
            print(f'BULLISH Divergence found at {group['timestamp'][current_valley_idx].date()}')
            bullish_divergences.append(prev_valley_idx)
            bullish_divergences.append(current_valley_idx)

    # check for Bearish Divergence (price: higher high, RSI: lower high)
    for i in range(1, len(price_peak_indices)):
        current_peak_idx = price_peak_indices[i]
        prev_peak_idx = price_peak_indices[i-1]

        current_price_high = group['high'][current_peak_idx]
        prev_price_high = group['high'][prev_peak_idx]
        
        current_rsi_high = group['RSI'][current_peak_idx]
        prev_rsi_high = group['RSI'][prev_peak_idx]

        # price makes a HIGHER HIGH, but RSI makes a LOWER HIGH
        if (current_price_high > prev_price_high) and (current_rsi_high < prev_rsi_high):
            print(f'BEARISH Divergence found at {group['timestamp'][current_peak_idx].date()}')
            bearish_divergences.append(prev_peak_idx)
            bearish_divergences.append(current_peak_idx)
    
    return bullish_divergences, bearish_divergences    

def compare_divergence_random(divergences_list, group, N):
    divergence_future_returns, random_future_returns = [], []
    for i in range(1,len(divergences_list),2):
        idx, rand_idx = divergences_list[i], random.randint(0, len(group)-N-1)
        if divergences_list[i] + N < len(group):
            divergence_future_returns.append(group.iloc[idx+N]['close']/group.iloc[idx]['close']-1)
            random_future_returns.append(group.iloc[rand_idx+N]['close']/group.iloc[rand_idx]['close']-1)
    # H0: avg(divergence_future_returns) - avg(random_future_returns) <= 0
    ts, p_val = stats.ttest_ind(divergence_future_returns, random_future_returns, equal_var=False, alternative='greater')
    print(len(divergence_future_returns))
    return np.average(divergence_future_returns), np.average(random_future_returns), p_val

def plot_divergence_results(group, bullish_divergences, bearish_divergences, peak_distance):
    # create two plots stacked on top of each other, sharing the same X-axis (date)
    fig, (ax_price, ax_rsi) = plt.subplots(2, 1, figsize=(16, 10), sharex=True)

    # plot 1: price
    ax_price.plot(group['timestamp'], group['close'], label=f'{group['symbol'].iloc[0]} Close Price', color='blue')
    ax_price.set_title(f'{group['symbol'].iloc[0]} Price and Divergences')
    ax_price.set_ylabel("Price (USD)")
    ax_price.legend()
    ax_price.grid(True)

    # plot 2: RSI
    ax_rsi.plot(group['timestamp'], group['RSI'], label=f'RSI ({peak_distance})', color='purple')
    ax_rsi.axhline(70, color='red', linestyle='--', alpha=0.2, label='Overbought (70)')
    ax_rsi.axhline(30, color='green', linestyle='--', alpha=0.2, label='Oversold (30)')
    ax_rsi.set_title('Relative Strength Index (RSI)')
    ax_rsi.set_ylabel('RSI Value')
    ax_rsi.set_xlabel('Date')
    ax_rsi.legend()
    ax_rsi.grid(True)

    # adding divergence lines
    if bullish_divergences:
        bull_div_points = group.iloc[bullish_divergences]
        ax_price.plot(bull_div_points['timestamp'], bull_div_points['low'], color='green', linestyle='--', marker='o')
        ax_rsi.plot(bull_div_points['timestamp'], bull_div_points['RSI'], color='green', linestyle='--', marker='o')

    if bearish_divergences:
        bear_div_points = group.iloc[bearish_divergences]
        ax_price.plot(bear_div_points['timestamp'], bear_div_points['high'], color='red', linestyle='--', marker='o')
        ax_rsi.plot(bear_div_points['timestamp'], bear_div_points['RSI'], color='red', linestyle='--', marker='o')

    plt.tight_layout()
    plt.show()

spy = spy.groupby('symbol', as_index=False).apply(calculate_signals)
spy = spy.dropna() # drop rows with NaN

symbol_to_analyze = 'AAPL'
group = spy[spy['symbol'] == symbol_to_analyze].copy()
group = group.reset_index(drop=True) # reset the index so we can use integer locations (iloc)
peak_distance = 10 # peak/valleys must be 15 days apart to only find major turning points
bullish_divergences, bearish_divergences = find_divergences(group, peak_distance)
plot_divergence_results(group, bullish_divergences, bearish_divergences, peak_distance)
print(compare_divergence_random(bullish_divergences, group, 10)) # get price N days after divergence & compare with price N days after random day