### Primitives

| Name    | Size (bytes) |
| ------- | -----------: |
| int     | 4            |
| long    | 8            |
| short   | 2            |
| byte    | 1            |
| bool    | 1            |
| float   | 4            |
| double  | 8            |
| char    | 2?           |


### Built-in structures
##### string (16 bytes)
long length

char* data




### Arrays
Arrays are 0-indexed continuous blocks of memory


### Examples
#### Hello World

```
using console;

int main()
{
	console.writeLine("Hello, World!");
	return 0;
}
```

## Statements
Statements are the basic building blocks of the program.
Each statement is ended using a semicolon.

### Variable declarations
Variable declarations use the following syntax:
```type name( = expression);```
##### Examples
```int x = 5;```  
```bool b = 5 > 9```  
```float temperature;```  

### If
If statements use the following syntax:
```if(boolean expression) statement```
```
if(boolean expression)
{
	code block
}
```

s

## Expressions and operators
Expressions are pieces of code that evalute to a value.
Expressions can be combined with operators to create more complex expressions.

##### Literals
The simplest forms expressions are literals. Literals are
constant values known at compile time.
There are three types of literals:  
**Integer literal**  
Any natural number: ```42```

**Boolean literal**  
Either ```true``` or ```false```

**Float literal**  
Float literals represent floating-point numbers and are
defined by writing a number suffixed by an 'f': ```3.14f```

**String literal**  
Strings literal are defined as 0 or more characters written between
double quotes ```"Hello, World!"```

### Operators
Operators are used on expressions to transform them into a new value.

##### + (Addition)
```
number + number -> number
```

##### - (Subtraction)
```
number + number -> number
```

##### == (Equals)
```
expression + expression -> boolean
```


##### < < (Multiple comparison)
All comparison operators can be chained
so instead of writing
```
x > 0 && x < 10
```
you can write
```
0 < x < 10
```

## Access modifiers
##### ```global```(Accessable everywhere)
##### ```internal```(Accessable within the file)
##### ```private```(Accessable within the type)