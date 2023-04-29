### The Billion Dollar App

This series of tutorials roughly follows the MDN local library tutorial but translated into Haskell.
Along the way, we'll take detours to explore the quirks and oddities of the Haskell programming language.

### Wait, But Why?

If all turing-complete languages are equivalent, then why would we choose Haskell?

Web development is not the typical domain that Haskell is introduced for.
A big use-case of Haskell is in implementing compilers and interpreters.
My early days of haskell were spent learning about writing parsers for simple expression languages.

Another big use-case of Haskell is as a testing ground for programming language research.
In fact, this is closely tied to its historical mission to be a programming lingua franca for academia.
We see a lot of Haskell features leak into more "mainstream" languages.

### But it worked on _MY_ machine!

Managing software dependencies is a challenge that many software projects have to grapple with.
There's general language dependencies, system dependencies and project-specific dependencies.
For this tutorial series we'll use a combination of Nix and Docker to streamline our dev-dependencies.

Haskell is a highly customizable language. A project may need its own version of the GHC compiler,
or a different set of standard library functions. We use Cabal to specify the haskel-specific constraints of our project.
Nix will then automatically provide us with the appropriate haskell compiler and libraries.

Nix is going to handle all the system dependencies that we'd need.
For example, Nix will fetch all the needed linters and formatters for the file types in our project.

To make the developer experience standard, we'll also provide a docker container that is setup with nix.
It is expected that the reader will use the container as a remote dev container in VS Code.

### Attack of the Clones

Starting out a project with an empty directory can be pretty daunting.
Luckily, Haskell has utilities that can scaffold our project for us.
The standard way to create a haskell project would be to run `cabal init` in the project directory.

However, we'll take an even shorter approach. We'll simply create a project based on a template on github.
[This template](https://srid.ca/haskell-template) will provide us with the nix configuration as well as the necessary files to execute a haskell program.
After creating a clone of the template, we'll rename it to `locallibrary` to match our problem domain.
