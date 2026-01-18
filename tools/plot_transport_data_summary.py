#!/usr/bin/env python3

# Author: Jannis Teunissen
# Purpose: plot the _summary.txt file from a simulation

import numpy as np
import pandas as pd
import argparse
import matplotlib.pyplot as plt

p = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter)
p.add_argument("summary_file", type=str, nargs='+',
               help="File <simulation>_summary.txt")
p.add_argument("-SI_field", action='store_true',
               help="Use electric field in V/m rather than Td")
p.add_argument('-loglog', action='store_true',
               help='Use log-log scale for the second panel instead of linear')
p.add_argument('-savefig', type=str,
               help='Save figure using this filename')


args = p.parse_args()

all_data = [pd.read_csv(f, sep=r'\s+') for f in args.summary_file]

if args.SI_field:
    for x in all_data:
        x.set_index('E[V/m]', inplace=True)
        x.drop(columns='E/N[Td]', inplace=True)
else:
    for x in all_data:
        x.set_index('E/N[Td]', inplace=True)
        x.drop(columns='E[V/m]', inplace=True)

ax = all_data[0].plot(subplots=True, layout=(-1, 2),
                      sharex=True, figsize=(10, 10))
for x in all_data[1:]:
    x.plot(subplots=True, layout=(-1, 2), sharex=True, figsize=(10, 10), ax=ax)

if args.loglog:
    # all_data[0].plot(..., subplots=True) returns an array-like of Axes
    for a in np.ravel(ax):
        a.set_xscale('log')
        a.set_yscale('log')

if args.savefig is not None:
    plt.savefig(args.savefig, bbox_inches='tight', dpi=200)
    print(f'Saved {args.savefig}')
else:
    plt.show()
