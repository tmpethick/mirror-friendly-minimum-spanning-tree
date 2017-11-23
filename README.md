# Mirror Friendly Minimum Spanning Tree

This project uses [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

## Execution

```
stack build && stack exec -- optim-mirror-friendly-st "./data/test01.uwg" "./data/test02.uwg" "./data/TestFile1.uwg"
```

## Improvements
Verify that the integer solution which the linear program solver outputs actually is a valid solution, that satisfy all the equations. 
