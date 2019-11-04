#  Refactored Symbol Table System

Although I have refactored a lot in the Oberon-0 compiler code, extracting subtasks in long functions into their own separate functions, renaming poorly named identifiers, etc... the  symbol table code was an especially important refactoring target because of its pivotal role in the compiler.  This document is intended to explain its current structure and operation.  I hope you will find this implementation much clearer than the original code.

## Motivation
In Niklaus Wirth's original code, and in the "faithful" version of my translation of it to Swift, the symbol table was implemented as a linked list of poorly named `ObjDesc` objects.  Linked lists have poor performance characteristics compared to virtually every other possible data structure, but the choice is justified in *Compiler Construction* as being simple, with the point being made that the book isn't a work on data structures.  The idea of using a simple data structure for teaching purposes makes sense, but unfortunately several issues with the code only serve to severely undermine the instructive intent.

The first problem in the original code is that the symbol table implementation is exposed to the code that uses it, and indeed, the parsing code is itself intimately involved in iterating through and modifying the symbol table internal details, handling next pointers, sentinel values, etc... As a result, a huge cognitive load is placed on the reader, making it hard to reason about parsing logic and symbol table operations separate from each other.   

Poorly named identifiers add to the confusion.   What is `ObjDesc` exactly?  What does `dsc` mean?  Though many of the names are mentioned and sometimes explained in the book, the code, especially code intended for instructional purposes, should be as self-documenting as possible.  A thing's name should tell you fairly accurately what it is and/or what it does, and lot of identifiers, maybe even most, in the original code completely fail to do that.  

Furthermore the original symbol table is used in multiple different ways, making its operation even more confusing.  The same linked list nodes serve as the raw symbol table, and as the scoping mechanism, as well as for describing the parameter lists for procedure definitions, and for field structure of  record types.  This conflation of roles further gets in the way of understanding.

Add to these problems the almost complete absence of comments, and the result is that the code obfuscates more than it clarifies.

My aim in refactoring the symbol table code and structure was to make the roles clearer, and better separate symbol table operational details from parsing code so that the each can be read and understood more clearly.

## The `SymbolInfo` class.

Whereas the original code packed virtually all information about symbols into the symbol table's linked list nodes themselves, that's poor separation of concerns.   The symbol information is the data, and the linked list should have just been the container for storing and looking up that data.  Therefore, the first refactoring was to separate the two.  To that end, I created the `SymbolInfo` class to contain the actual symbol information:

	public final class SymbolInfo: Equatable
	{
		public enum Kind: Int
		{
			case head = 0
			case variable = 1
			case parameter = 2
			case constant = 3
			case field = 4
			case type = 5
			case procedure = 6
			case standardProcedure = 7
			
			case register = 10
			case condition = 11
		}

		public var kind: Kind = .head
		public var level: Int = 0
		public var type: TypeInfo? = nil
		public var name = ""
		public var value: Int = 0
		public weak var owningScope: SymbolScope? = nil
		public var ownedScope: SymbolScope? = nil
	}

As you can see, I've changed what was the symbol's `class` in the original code to `kind`, because in Swift, `class` is a keyword.  In many contexts, using `class` as an identifier requires backticks to distinguish it from the actual keyword.  While it's nice that Swift provides that feature, I found it to be a nussiance, and many simple refactorings in Xcode failed to recogize the backticked `class` as an indentifier. I changed the type of `kind` from an `Int` to an `enum`, which helps with type checking.  Note: The `SymbolInfo.Kind` `enum` is also used as the `Mode` in `RISCOperand` type, which in Wirth's original code was `OSG.Item`.

The `ownedScope` property serves the same purpose as the original `ObjDesc.dsc` property, to reference the scope defined by a procedure.  I think it's better named, though I still kept it a bit general, rather than calling it something more specific like `procedureScope`, because in principle other types of things could have their own scopes.  It is the scope owned by the thing described in the `SymbolInfo` object.

I've also added a convience property,  `owningScope`, to allow easy access from a `SymbolInfo` object the scope in which it is defined.  This property is maintained, but not used for actual parsing or code generation in the existing code. I have used it while debugging, but not for anything else (yet).

One might object that `owningScope` and `ownedScope` behave much like the node pointers in the original linked list implementation, and wonder if this refactoring really improves anything, but there are some major differences from the linked list.  The `owningScope` and `ownedScope` properties don't refer to more `SymbolInfo` objects as they would in a linked list, but rather to `SymbolScope` objects, which are discussed in more detail below, but for now, their important characteristic is that they are just containers for things that happen to be more `SymbolInfo` objects. The `SymbolInfo` objects do not need to know anything about how `SymbolScope` is implemented, and `SymbolScope` couldn't care less about the the internels of the `SymbolInfo` objects it holds, except in relying on the fact that `SymbolInfo` objects have a `name` property that is a `String`.  In fact, `SymbolScope` could be made a generic container, holding some arbitrary type, `T`, with the only requirement that `T` conform to some protocol that provides a `name` property of type, `String`.   Since there is no immediate need to use `SymbolScope` for anything other than `SymbolInfo`, making it generic doesn't make sense at the moment, but the fact that it could be made generic illustrates how it separates container details from the details of the data it contains.

Although `SymbolInfo` is defined as a `class`, it is not intended to be used in a polymorphic way.  The main reason for declaring it as a `class` instead of `struct` is simply that is how one defines reference types in Swift, and we definitely do want reference semantics as it is used in the compiler.  One might make a good case for using a value type instead, especially if the compiler were mutli-threaded, and that might be a further evolution in the future, but since the original code used pointers to dynamically allocated linked list nodes, using `class` allows me to keep the same basic semantics as the original while I refactor other things.

## The `SymbolScope` class

Instances of the `SymbolScope` class serve as scope-limited containers for instances of `SymbolInfo`.   The actual symbol information is no longer stored in a linked list, but rather each `SymbolScope` maintains an array of `SymbolInfo` objects for symbols defined in its scope, though this is an internal detail.  This takes advantage of having built-in dynamic arrays in Swift, whereas Wirth was programming in Oberon, which only has fixed sized arrays, and so this wasn't an option for him, at least not without pontentially wasting a  great deal of memory.  Also Wirth's stated reasoning for using a linked list was its simplicity, but an array is even simpler, and on modern CPUs has at least an order of magnitude better performance because of on-chip memory caches.

One defines a new symbol in a given scope like so:

	let symbolInfo = currentScope.defineSymbol(named: "x", kind: .variable)

As a convenience, the `defineSymbol(named:kind:)` method returns the `SymbolInfo` it creates, since it is often needed by the caller shortly after defining the symbol.

One can search a particular scope for a symbol definition using the subscript operator, much as one would a Swift dictionary.

	let symbolInfo = currentScope["x"]

Like Swift's dictionary, `nil` is returned if no matching symbol is found in the scope.

While a linked list is no longer used to store the the symbol information, the `SymbolScope` objects themselves do still use a linked list to internally represent the scope hierarchy.  The parser does not need to know anything about this though.  It merely calls the `openScope` and `closeScope` methods.  The caller is responsible, however, for keeping track of its current scope by storing the `SymbolScope` objects returned from these methods.

	currentScope = currentScope.openScope()
	// Do stuff in the scope
	currentScope = currentScope.closeScope()
	
To help keep client code from having to know about the internels of `SymbolScope`, and to make it more natural to work with in Swift, it conforms to Swift's `Sequence` protocol, with its iterator returning  `SymbolInfo?`.   So you can iterate over its symbol information just like you would any other sequence:

	for symbolInfo in currentScope {
		// Do stuff with symbolInfo
	}
	
## The `SymbolScope.Hierarchy` struct

One often needs to search the entire scope hierarchy for a symbol.  To provide a convenient, Swift-like way to do that, `SymbolScope` provides a computed `hierarchy` property that returns a `SymbolScope.Hierarchy` struct, which is light-weight view of the whole scope hierarchy anchored at the `SymbolScope` object that produced it.   It provides the same symbol look-up API as `SymbolScope`.

You can search the scope hierarchy for a symbol like so:

	if let symbolInfo = currentScope.hierarchy["x"] {
		// Do stuff with symbolInfo
	}
	
This example searches the scope hierarchy, starting with `currentScope`, returning the first `SymbolInfo` that matches the specified name, `"x"`.  If global scope is reached and searched without finding a match, `nil` is returned.

`SymbolScope.Hierarchy` also conforms to Swift's `Sequence` protocol, so you can iterate over all of the symbols visible from the current hierarchy in the same simple way you can iterate over the symbols in a particular scope:

	for symbolInfo in currentScope.hierarchy {
		// Do stuff with symbolInfo
	}

Since `SymbolInfo` objects have reference semantics, you can modifiy the information in them as easily when obtaining them from a `SymbolScope.Hierarchy` as you can from a `SymbolScope` itself; however, `SymbolScope.Hierarchy` does not provide any means of defining or appending new symbols, nor does it provide a means for opening or closing `SymbolScope` objects.

## The `TypeInfo` class

The `TypeInfo` class is largely identical to Wirth's original `TypeDesc` type.  I think the name, `TypeInfo`, is a little bit more informative than `TypeDesc`, though I admit it's only a minor improvement.  Of course, I could have just used `Type`, which Wirth used as an alias for a pointer to a `TypeDesc`, but like `class`, `Type` is a Swift keyword, requiring backticks to use it as an identifier, and shares all the same problems.  

	public class TypeInfo: Equatable
	{
		public enum Form: Int, Comparable
		{
			case boolean = 0
			case integer = 1
			case array = 2
			case record = 3
		}
		
		public var form: Form = .boolean
		public var fields: [SymbolInfo] = []
		public var base: TypeInfo? = nil
		public var size: Int = 0
		public var len: Int = 0
	}

The two changes I did make to `TypeInfo` were to redefine its `fields` property as an array of `SymbolType` rather than a linked list as it was in the original, and to redefine the `form` property to be of the type `enum Form`. 
