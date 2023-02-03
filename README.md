# Interaction
Lazy interactions in Haskell

Code and slides for presentations at ZuriHac, Haskell eXchange and purecode.

## History

These slides and code cover an approach to designing interactions using the behaviour
of lazy IO. This was the default approach in the Miranda programming language, and supported
in Haskell by the `interact` function.

The work was transliterated from Miranda (see the file Interaction.rtf for details) and presented 
at ZuriHac, where [Neil Mitchell](http://ndmitchell.com) kindly reminded me that pattern bindings in 
`where` and `let` constructs are automatically irrefutable. The code here reflects that.

After presentation at the Haskell Exchange, the file `Interfaces.hs` was added, exploring the 
standard interfaces (aka classes) to which the `Interact a b` type belongs.