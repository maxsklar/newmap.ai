# NewMap.AI

Newmap.ai is a new programming language and database combo currently under development meant to rethink from the ground up how engineers and scientists build data stores, algorithms, and artificial intelligence.

## Current Status

Currently newmap.ai is under development and has no demo or alpha version. The design behind the data model and syntax are still in flux. Watch for this to change in the coming months. Contact Max Sklar for questions, and if you dare dive into the code, check out the unit tests to get a flavor of what is going on. There is also a mailing list and slack for those interested.

### Use of scala and sbt

The current iteration of newmap runs on scala and sbt as an interpreted language. There is no data storage yet. Once a core set of features are developed, the plan is to write as much of the internals as possible into newmap.ai code. Once the remaining scala is a small size, it can be ported to other languages.

## How to Run

Compile the code:
sbt compile

Run newmap command line:
sbt run

Run unit tests:
sbt test

## Strategy and Philosophy

The strategy behind newmap.ai development will the implementation of several features which when combined bring tremendous power.

### Versioning
Developers are used to versioning their code, but not versioning their database. Imagine if each iteration of your data has its own versioning hash. This allows more careful development from the perspective of the data scientist which is often improving their dataset and algorithm in tandem. It also means that more and more code that was once compiled can be included in the database, which along with validation makes launching updates much easier.

### World class validation with strong typing
This stands in stark contrast to NoSQL document where everything goes, and traditional tabular data where every row must be the same type. We include algebraic data types, allowing developers and admins to be much more expressive about what’s allowed and what’s not. This feature opens the door for algorithm and join recommendations on datasets which can be more closely analyzed through its metadata and contents.


### Maps and arrays
The key-value store is central to algorithmic development, but newmap.ai brings this to a whole new level with a categorization and type system of maps and structures. This includes pattern matching and levels of constraint making infinite loops and other errors less likely. It also makes it easier to generalize and optimize algorithms.
This also uses ideas in category theory where maps have “preservation rules” and are required to preserve some structure.

### Command-Based Architecture
Each collection and data type starts from a ground state and grows through a series of “commands”. This helps with versioning and is a good synthesis between pure functional and imperative programming. This also bridges the divide between database languages and general server languages. Code in newmap.ai can work as both database commands, and as a model for data in memory.

### Additionally
Uses ideas from functional programming, category theory, formal verification, aspect-oriented programming, and version repositories.

## Future Projects

The following will one day be possible with newmap.ai:

### ML Models based on metadata
Build and suggest statistical models based on properties of the dataset.

### Automatic data struct upgrades
Because there is strong verification onboard, imagine having code that is constantly finding upgrades to the way it stores and serves data. These upgrades can be found through the internet, or through an algorithm that searches the solution space.

### Universal Concept Map
Once probabilistic programming is a part of newmap.ai, we can generate a collection to model the space of “concepts” known to humans. This is an approach to natural language understanding which

## Code Design Principles

### Untagged Objects vs Tagged Objects

### NewMapType
NewMapType is a representation of a type in the type system. Ultimately, the information in NewMapType should specify everything that we need to know about the type of an object.

NewMapType can also be turned into an Untagged Object.
