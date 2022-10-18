import os

from checkGaps import checkGaps
from normalizer import normalizer
from joiner import joiner
from mfilterer import mfilterer


# Define the data we want to study
files = [f for f in os.listdir("Database") if os.path.isfile(os.path.join("Database", f))]

varNames = [i[0:-4] for i in files] # extract the names of the variables

# Define the time dram we want to use (a: months, b: weeks, c: days)
timeFrame = 'a'
timeStep = '15 min'


if __name__ == '__main__':

    for varName in varNames:

        # Fill in the gaps in the time series
        checkGaps(File=f'{varName}.txt', timestep=timeStep)
        print('[INFO] checkGaps() DONE')

        # Normalize the data. See normalizer.py for details
        normalizer(File=f'{varName}_full.csv', timeframe=timeFrame, timestep=timeStep)
        print('[INFO] normalizer() DONE')
        
    
    # Join the normalized databases
    joiner()
    print('[INFO] joiner() DONE')

    # Filter out those months or weeks or days (depending on the desired
    # time unit) with too many NaN in several variables and iterate on the rest
    mfilterer(File='data_joi.csv', timeframe=timeFrame, timestep=timeStep)
    print('[INFO] filterer() DONE')