# Network of evolving trading strategies 

Inspired by Tom Schaul's thesis on evolving a compact, concept-based Sokoban solver. 

Generates strategies of the form (AND (indicator & args) (AND (NOT (indicator & args)) (OR (...) (...)))) and simulates them on EURUSD, 1M candles. Merges branches from existing strategies with likelihood 1/(market exposure)^2

Work in progress. Next few updates will include:
* significantly faster simulation by generating table of returns at each candle at various trailing distances
* connecting to InteractiveBroker API for real-time testing and trading
* Adding neural networks to the agnts' toolbelts

## Installation

Be sure install https://github.com/sbhaaf/clj-ta-lib for the technical indicator library. Other deps in project.clj

## Usage

Not currently in format supporting uberjar, but could be easily. 
Open in nREPL and spin up workers to start testing strategies.
