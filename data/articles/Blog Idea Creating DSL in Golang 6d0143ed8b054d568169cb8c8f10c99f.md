# Blog Idea: Creating DSL in Golang

Go is a programming language. It's old-fashioned but popular. Simple yet complex. 

Use web server as example? Or maybe recreate original etl worker, or some kind of similar data layer.

Use ETL as example.

note: wouldn't consider this article in a unityped language because dynamic invariants are really just part of the application code, and we are talking not at the application code level, but in the behavior and structure of the application code itself. code is mutable, so anyone that thinks they're more clever than the author will break everything. 

Goal is to provide an UX - like interface for programmers to build applications with, but using code. Need to enforce boundaries. easy to do with UI (or CLI), but can also be done at code level. 

justify creating dsl and enforcing invariants in code

treating code as product, with interface, careful to think about how it will be used

using method parameters

```go
func AddResource(
          server *Server,
          method string,
          route string,
          templates []string,
          handler gin.HandlerFunc,
          pageData interface{},
) {
```

to create a safe, well-defined interface. Note, you cannot do:

```go
type Resource struct {
          method string
          route string
          templates []string
          handler gin.HandlerFunc,
}

func AddResource(
          server *Server,
          resource Resource,                                                
          pageData interface{},
) {                                                    
```

Because `Resource` can be created with zero values by default if not specified. There's no invariant of having required fields in a Golang struct. Function parameters are always implicitly required, and therefore you can invariants where things must be present and defined. 

The weakest form of this invariant is simply making the programmer think about setting the value. You can do this with an interface. They at least have to define a method and think about what to do. This is almost equivalent, but having to create methods is annoying and you can still leave out a value pretty easy since this would be defined in a separate package and isn't occurring at the interface. location in code matters. how far away or indirect things are. 

define invariants. look up definition of invariant and other usages. think of funny sentences

You could invariantly vary in variants 

**Also...** 

Explain options

Code generation (spend most of time here?), lack of generics

Interfaces (not row polymorphic, can't upcast) (eh)

first class functions as way to get past types, functional programming ftw

Good old CSP continuation passing style

closures. (with first class functions, use example of stream reader that takes in a value from a stream and returns true or false, true if that value should be emitted or false if it should be surpressed. Can create different combinators then upon this pattern using closures like AfterN(i int) or FastForward(i int) or EveryOther (storing boolean state)

DSL then is easier since you can just use functions as an interface, as opposed to find other ways which would be hard to define. 

more powerful == more convoluted

add runtime checks to cover everything else. And then everythings covered!

**list of invariants, and how to enforce them in Golang.**