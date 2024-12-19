# HTPL-thesis

HTPL is a High-level Trust Policy Language designed for use with P3KI Core.

The project has been built using Haskell Stack. As such the following commands are applicable. 


## Run:
HTPL has a simple interface, needing a filepath and the two identities to query trust between.
```
stack run file id1 id2
```


## Build:
```
stack build
```


### Examples: 
```
stack run app/Programs/crewChange.htpl captain tmp_worker
```
Returns the result:
```
SuperPolicy:{Access: vessel, Certificate: _}, {Access: port, Certificate: _}
```

More example programs can be found in the app/Programs/ folder. 

## Install to path
```
stack install
```

Installs HTPL on path. Can be executed using the command:
```
HTPL app/Programs/crewChange.htpl captain tmp_worker
```

## Test
To execute the tests run the command:
```
stack test
```