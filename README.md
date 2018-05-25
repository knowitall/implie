# IMPLIE

IMPLIE (IMPLicit relation Information Extraction) is a program that extracts binary relations from English sentences
where the relationship between the two entities is not explicitly stated in the text.  IMPLIE supports the following
target relations out-of-the-box: *has nationality*, *has job title*, *has province*, *has city*, and *has religion*.
However, other relations can be supported by providing a list of keywords for a new target relations.   This is 
possible because IMPLIE uses a target independent syntactic language model.

For example, consider the following sentence:

    French journalist Paul Legall reported that all three hostages arrived safely at Athens International Airport.
  
The out-of-the-box IMPLIE system extracts the following binary relations:

    (French journalist Paul Legall; has nationality; French)
    (journalist Paul Legall; has job title; journalist)
    (Athens International Airport; has city; Athens)

## Building
IMPLIE uses Java 8 and the sbt build system.  Install [sbt](http://www.scala-sbt.org/release/tutorial/Setup.html).  Enter the root directory of this repository and
run command

    sbt compile
    
This will take a while the first time since it will download and resolve all the dependencies.

## Using IMPLIE as a Dependency
We haven't released IMPLIE to maven, but it can be released locally.  From this repository run the following commands to
release the project locally.

    sbt
    publish-local

Then IMPLIE can locally be added as a dependency in sbt by adding the following to the libraryDependencies.

    libraryDependencies ++= Seq(
        // other dependencies...
        "edu.washington.cs.knowitall.implie" %% "implie" % "1.0.0-SNAPSHOT",
        // other dependencies...
    )

## Citing IMPLIE

@article{soderlandlanguage,
  title={A Language Model for Extracting Implicit Relations},
  author={Soderland, Stephen and Kim, Gene L and Hawkins, Natalie}
}

## Contributors
* Gene Kim
* Natalie Hawkins
