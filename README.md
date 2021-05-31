# kwoka

My take on the simplified version of the Koka language.

## Requirements

`Haskell` + `stack`

## Installation

Clone this repo and then run the following command inside it:
```
stack install
```
## Running
```
kwoka-exe filename [options]
```

#### Options:
- -debug - Pretty print program with inferred types to file
- -help - Print short manual

## Example
```
effect Hello
{
  Hello() :: String
}

fn makeGreeting(name)
{
  Hello() ^ " " ^ name
}

fn main()
{
  handle<Hello>(makeGreeting("General Kenobi."))
  {
    return(x) => PutStrLn(x),
    Hello() => resume("Hello there!")
  }
}
```