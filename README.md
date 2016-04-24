# Note chain

A decentralized note taking tool inspired by the git version control system, Content is stored in a merkle tree data structure.

## Basic usage

##### Creating a new note

Run the command `./obj/note_chain note new`, this will open the vim editor. once you close vim the node will be saved.

##### Listing nodes

Run `./obj/note_chain note list` to list all of the notes in the current branch

##### Creating a new branch

Run `./obj/note_chain branch new <name>` to create a copy of the current branch

##### Changing branches

Run `./obj/note_chain branch checkout <name>` to change to a diffrent branch

## Prerequisities

GNAT Ada compiler with Ada 2012 support and the Aunit test framework

### Building

In the project root call the compiler `gnatmake -P note_chain.gpr` or use `gprbuild`

## Running the tests

The executable `./obj/test` contains the compiled unit test


## Deployment

Add additional notes about how to deploy this on a live system

## Built With

* Ada programming language
* GPS IDE

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## License

This project is licensed under the GNU GENERAL PUBLIC LICENSE - see the [LICENSE](LICENSE) file for details

