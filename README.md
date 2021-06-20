# Plutus Rating Project

An on-chain rating system for smart contracts on Cardano.

This is a self-learning project about designing and writing smart contracts in Cardano.

## Idea

- Rating system allowing people to rate smart contracts, allowing those contracts to gain reputation
- Ratings processed and stored on-chain for transparency and trust
  - Each rating as eUTXO
  - Script adress parameterized by the script to be rated
  - Actions to add, update and delete ratings
  - Ratings holding information about:
    - Who submitted the rating
    - Score between 1 and 5 
- Evaluation and overview created off-chain for efficiency
  - Analysis of all ratings submitted for a smart contract
  - Validation of ratings by weighting them with the amount of ADA hold by the rating person. For example to prevent bot-ratings. Similar to voting power in Catalyst.



## References

This project is based on the learning material of the Plutus Pioneers Program.
- https://github.com/input-output-hk/plutus
- https://github.com/input-output-hk/plutus-pioneer-program
