

# Function Composition


disclaimer about unfinished work


Goal is to show how to architect applications at scale by keeping them composable. 
can easily add new functionality on existing data, but not with new data.
show problem as wanting to compose functions on new data, but can't get signatures to match.
need to thing not just what application is during design, but would it could be. 


f :: A -> B -> C
g :: X -> A
h :: X -> A

how to get B?
need to create 
m :: Y -> B but how? not open to change.

making same mistakes at scale as in single codebase

abstractions have a multiplicate complexity effect

currently solving these issues with social solutions, but would prefer technical ones. 

ideally, would have access to data or able to re-derive datqa

actually inverted problem. now each service always needs to add functions for new service or new functionality. Why graphql is popular, but solving wrong problem. Problem is not querying, problem is way we are architecting the applications. 
powerful/arbitrary querying only hides problem.

like expression problem. software isn't dynamic becasuse change is hard. handling change should be part of software itself

**data availability**

haskell top-level state function, need state to have data.

## Outline


Introduce function composition.
Talk about category theory.


point-free programming.
how you can write any function and combine them only via composition.
simple, but powerful.
goals: be able to extend system. 


smart functions, dumb data.
functions are what matter, determine what program is/does. 
constraining functions to particular data makes your programs fragile. 
functions should always be generic, work for broadest range of data as possible = more powerful. 


compare functions to services. same thing, but shared.
service = collection of shared functions.
same problem as OOP. service controls data
the expression problem ? 
encapsulation is a programming tool like a brick wall, and should only be used to model actual brick walls. 
everything should be open and laws and invariants should be defined and agreed upon. 
type-safety, precision.



where are the microservices? Well, they are there, but not really needed anymore.
We can really just distribute the functions and get rid of the SOA model.

