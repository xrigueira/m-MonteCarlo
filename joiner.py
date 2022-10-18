import os
import numpy as np
import pandas as pd

"""This function joins several csv files which contain time series of the
same lenght into a unique csv file."""

# NEEDSWORK: this function is too dependent on the file names. This can be improved I think

def joiner():
    
    # Set the names of the files which contain the data
    amonio = "Amonio_nor.csv"
    conductividad = "Conductividad_nor.csv"
    nitratos = "Nitratos_nor.csv"
    oxigeno = "Oxigeno disuelto_nor.csv"
    ph = "pH_nor.csv"
    temperatura = "Temperatura_nor.csv"
    turbidez = "Turbidez_nor.csv"

    # Read the files and store them as a pandas database
    df = pd.read_csv(f'Database/{amonio}', delimiter=';', parse_dates=['date'], index_col=['date'])
    dfConductividad = pd.read_csv(f'Database/{conductividad}', delimiter=';', parse_dates=['date'], index_col=['date'])
    dfNitratos = pd.read_csv(f'Database/{nitratos}', delimiter=';', parse_dates=['date'], index_col=['date'])
    dfOxigeno = pd.read_csv(f'Database/{oxigeno}', delimiter=';', parse_dates=['date'], index_col=['date'])
    dfph = pd.read_csv(f'Database/{ph}', delimiter=';', parse_dates=['date'], index_col=['date'])
    dfTemperatura = pd.read_csv(f'Database/{temperatura}', delimiter=';', parse_dates=['date'], index_col=['date'])
    dfTurbidez = pd.read_csv(f'Database/{turbidez}', delimiter=';', parse_dates=['date'], index_col=['date'])


    # Extract the desired columns
    conductividad = dfConductividad['value']
    nitratos = dfNitratos['value']
    oxigeno = dfOxigeno['value']
    ph = dfph['value']
    temperatura = dfTemperatura['value']
    turbidez = dfTurbidez['value']

    # Insert the new columns in the database
    df.insert(1, "conductivity", conductividad, True)
    df.insert(2, "nitrates", nitratos, True)
    df.insert(3, "oxygen", oxigeno, True)
    df.insert(4, "pH", ph, True)
    df.insert(5, "temperature", temperatura, True)
    df.insert(6, "turbidity", turbidez, True)

    cols = list(df.columns.values.tolist())
    cols = [i.replace("value", "ammonium") for i in cols]
    df.to_csv(f'Database/data_joi.csv', sep=';', encoding='utf-8', index=True, header=cols)
