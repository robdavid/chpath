# chpath

A utility to manipulate environment variable search paths, such as PATH. It can:

* Add and remove entries from a path
* Control the ordering of entries
* Avoid unnecessary duplicate entries
* Associate path changes and environment settings with a directory tree, such as a user's home directory.

## Usage
```
chpath-bin [-r] [-s] <pathmod...>
```
```
chpath [-r] <pathmod...>
```
The `chpath-bin` binary is a native executable that performs path modifications and produces a shell script that sets the required environment variables. The `chpath` command is a shell function that calls `chpath-bin` and executes the created script within the current shell. This is the normal way to invoke chpath. (See shell/profile.sh and shell/bashrc.sh).

`pathmod` is a path modification statement, which specifies one or more transformations to be performed to a specific environment variable.

### Options
#### `-s`
Write the shell script to a file rather than standard out. The file name will be a unique temporary file, whose name is echoed to standard out. The final command in the script will be to delete itself. Therefore executing the script with ``.`` or ``source`` will perform the required environment changes and then clean up. This allows shell invocations such as ``source `chpath-bin -s <pathmod>` `` to be performed. This option is used by the chpath shell function.
#### `-r`
Reverses the effects of a path modification. The actions performed by the reverse modification are determined via static analysis of the modification expressions, consisting broadly of removing search entries that are unambiguously added.

### Path Modification Statements
The core functionality of chpath is to transform environment search paths, which is achieved by providing one or more path modification statements. A path modification statement has one of the following forms:

* `dir`|`directory` _&lt;path&gt;_
* `include` _&lt;path&gt;_
* _&lt;envvar&gt;_ `=` _&lt;path-expr&gt;_ [`^` _&lt;reverse-path-expr&gt;_ ]
* _&lt;envvar&gt;_ `+=` _&lt;path-expr&gt;_
* _&lt;envvar&gt;_ `=+` _&lt;path-expr&gt;_
* _&lt;envvar&gt;_ `-=` _&lt;path-expr&gt;_

#### Directory Statement
`dir`|`directory` _&lt;path&gt;_

The directory statement applies a script of path modification statements associated with the directory given by _&lt;path&gt;_. This script is taken from either the contents of a `.chpath` file in the top level of that directory, if present, or from the appropriate sections of the `chpathrc` file.  In both cases all relative paths are assumed to be relative to that directory.

#### Include Statement
`include` _&lt;path&gt;_

The include statement applies path modification statements contained in the file given by _&lt;path&gt;_. The effect is the same as if the include statement is textually replaced by the contents of the file.

#### Assignment Statement
_&lt;envvar&gt;_ `=` _&lt;path-expr&gt;_ [`^` _&lt;reverse-path-expr&gt;_]

The assignment statement modifies the value of the environment variable named _&lt;envvar&gt;_. Very often the name of the environment variable will be PATH. The _&lt;path-expr&gt;_ is a list of path terms in form:

  _&lt;path-term&gt;_[`:`_&lt;path-term&gt;_...]

The sequence of path terms defines a colon separated list of entries, once all the terms are evaluated.

Duplicate entries in the evaluated search path are removed with the leftmost occurrence retained, _at each level of nesting_. Any path term that can evaluate to a list of entries - that is an environment variable or terms in parenthesis - is regarded as a level of nesting. When a search path is evaluated, each nested level will also have any entry removed that occurs at the parent level. This allows the parent level to decide the placement of that entry.

Each path term can be one of:
  * A relative or absolute path
  * A literal value, of the form `[`_&lt;value&gt;_`]`
  * An _optional_ term, of the form `{`_&lt;path-term&gt;_`}`
  * An environment variable, of the form `@`_&lt;envvar&gt;_
  * A subtraction term, expressed as _&lt;path-term&gt;_ `-` _&lt;path-term&gt;_
  * A nested path expression in parenthesis, as `(`_&lt;path-expr&gt;_`)`

##### Relative or Absolute Path
Simple paths are always resolved to a absolute canonical path, with any `..` or `.` occurrences removed. Symbolic links are _not_ followed, however. Relative paths are assumed relative to a current directory, which may change if the directory statement is used.

##### Literal Value
`[`_&lt;value&gt;_`]`

Literal values may be placed in square brackets and will be treated as literal text, and no path resolution will be attempted. A common use case is to allow the current directory to be added to a path, which can be expressed as `[.]`; if it were simply `.`, it would be replaced by a canonical path to the current directory.

##### Optional Value
'{' _&lt;path-term&gt;_ '}'

Optional values are values that are only included if they also occur somewhere else in the resolved list, including nested levels. This is typically used to relocate entries if they are present, but not add them if they are not.

##### Environment Variable
`@`_&lt;envvar&gt;_

Environment variables are expanded as a nested list of literal terms. None of the terms in the list are converted to an absolute path or in any way modified.

##### Subtraction Term
_&lt;path-term&gt;_ `-` _&lt;path-term&gt;_

A subtraction term is evaluated by removing from the left hand side all occurrences of matching terms found on the right hand side. Both the left hand side and/or the right hand side can be a list.

##### Parenthesis Expression
`(`_&lt;path-expr&gt;_`)`

Any path expression can be placed in parenthesis, which is assumed to be a nested list.

##### Reverse Path Expressions

Chpath supports the ability to reverse a path transformation, via the `-r` option. To facilitate this, a reverse expression to undo the effects of the forward expression is needed. By default, this is statically derived from the forward expression in the following manner.

Assuming the assignment is of the form

_E_ `=` _X_

where ___E___ is an environment variable, and ___X___ is the forward expression, then the reverse expression will be

`@`_E_ `-`_X'_

where ___X'___ is ___X___ transformed such that all positive references to `@`_E_ and all optional items are removed. Positive references to `@`_E_ are all those that do not appear to the right hand side of a subtraction (`-` operator). Or, in other words, the reverse assignment tries to remove from the environment variable any terms that are unambiguously added in the forward assignment.

For example, if the assignment is

`PATH=@PATH:bin`

Then the forward expression is `@PATH:bin`, which is adding `bin` to `PATH` and the reverse expression is

`@PATH - bin`

which will remove `bin` from `PATH`.

###### The `^` Operator

In some cases, the automatically derived reverse expression is not what is required. In such cases, an explicit reverse expression may be placed after the forward expression, separated by the `^` character. For instance, the above example could be written as:

`PATH = @PATH:bin ^ @PATH - bin`

###### Other examples

`PATH = {[.]}:@PATH:@OTHERPATH` is equivalent to `PATH = {[.]}:@PATH:bin ^ @PATH - @OTHERPATH`

`MYVAR = a` is equivalent to `MYVAR = a ^ @MYVAR - a`

If a variable should always be unassigned when reversed, you can write

`VAR = a ^`

The empty expression after the `^` is taken as an empty list, and variables that are empty are __unset__.

#### Compound Assignments
There are three forms.

 1. ___&lt;envvar&gt;___ `+=` ___&lt;path-expr&gt;___ is equivalent to
 ___&lt;envvar&gt;___ `=` ` @`___&lt;envvar&gt;___`:`___&lt;path-expr&gt;___

 2. ___&lt;envvar&gt;___ `=+` ___&lt;path-expr&gt;___ is equivalent to
 ___&lt;envvar&gt;___ `= `___&lt;path-expr&gt;___ `:` `@`___&lt;envvar&gt;___

 3. ___&lt;envvar&gt;___ `-=` ___&lt;path-expr&gt;___ is equivalent to
 ___&lt;envvar&gt;___ `=` ` @`___&lt;envvar&gt;___ `-` ___&lt;path-expr&gt;___

### Directory Based Modification
A set of path modifications can be associated with a directory, which can be activated or deactivated with the chpath `directory` command. This can be used, for example, to set up environment paths in a user's home directory. There are two ways to create these associations. The first is with a .chpath file placed in the root of the directory, which contains a list of path modification statements. The second is from a .chpathrc in the user's home directory. This file should contain definitions in the form

`dirdef` directory `{`
  statements ...
`}`

Where _directory_ is a directory path and _statements_ is a list of path modification statements to be associated with that directory.

### Examples
#### Adding a user's bin directory to the path

If a user has a directory `bin` in their $HOME directory, and they wish to add this directory to the path, then they can create a `.chpath` file their home directory with the following contents:

`PATH+=bin`

Then, the absolute path of `bin` can be appended to the path by executing

`chpath dir $HOME`

and can be removed again with

`chpath -r dir $HOME`

#### Adding subdirectories

If a user has .cabal directory in their $HOME directory, then path definitions for that directory can be added by adding the following lines to $HOME/.chpathrc

```
dirdef .cabal {
  PATH+=bin
}
```

This definition can then be referenced in $HOME/.chpath

```
directory .cabal
PATH+=bin
```

Then, the absolute path of `bin` and `.chpath/bin` can be appended to $PATH by running

`chpath dir $HOME`

#### Adding a directory to the front of the path

`chpath PATH=+bin`

#### Adding an optional directory to the front of the path

The following will add (or move) `bin` to the front of the but give priority to current directory "." if it already appears in the path.

`chpath PATH={[.]}:$HOME/bin:@PATH`
