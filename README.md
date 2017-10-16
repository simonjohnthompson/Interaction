# Interaction
Lazy interactions in Haskell

Code and slides for the presentation at [ZuriHac2017](https://zurihac.info) and at [Haskell eXchange 2017](https://skillsmatter.com/conferences/8522-haskell-exchange-2017).

## History

These slides and code cover an approach to designing interactions using the behaviour
of lazy IO. This was the default approach in the Miranda programming language, and supported
in Haskell by the `interact` function.

The work was trasnliterated from Miranda (see the file Interaction.rtf for details) and presented 
at ZuriHac 2017, where [Neil Mitchell](http://ndmitchell.com) kindly reminded me that pattern bindings in 
`where` and `let` constructs are automatically irrefutable. The code here reflects that.

After presentation at the Haskell Exchange, the file `Interfaces.hs` was added, exploring the 
standard interfaces (aka classes) to which the `Interact a b` type belongs.