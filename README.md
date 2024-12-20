# HTPL-thesis

HTPL is a High-level Trust Policy Language designed for use with P3KI Core.
The project has been built using Haskell Stack. As such the following commands are applicable. 


## Run:
HTPL has a simple interface, needing a filepath and the two identities to query trust between.
HTPL will evaluate the network and calculate the trust between the identities. 
```
stack run file.htpl id1 id2
```

## Build:
```
stack build
```

## Install to path:
```
stack install
```
HTPL can then be used with the command:
```
HTPL file.htpl id1 id2
```

## Test:
To execute the tests run the command:
```
stack test
```

## Examples: 
Example programs can be found in the app/Programs/ folder.

```
stack run app/Programs/crewChange.htpl captain tmp_worker
SuperPolicy:{Access: vessel, Certificate: _}, {Access: port, Certificate: _}
```

```
stack run app/Programs/cadetAccessAlternative.htpl captain engine_cadet
SuperPolicy:{Access: _, Equipment: tools[electric], Navigation: _}, {Access: ship[engine].fuzeBox, Equipment: _, Navigation: _}
```

```
stack run app/Programs/trust.htpl id1 id2
SuperPolicy:{Access: gate, Certificate: Pilot, Music: _, Tag: aspect1}
```

```
stack run app/Programs/lowHighAccess.htpl captain second_engineer
SuperPolicy:{Access: bridge.close, Navigation: course.check}
```

```
stack run app/Programs/buoyAlternative.htpl captain buoy1
SuperPolicy:{Nav: revoked}, {Nav: buoy[Capesize]}
```
