# Erlang HL7 NIF Parser

## Overview

This HL7 parser is an Erlang NIF interface into the C-based `hl7parser`.


## Requirements

You'll need Erlang/OTP R16B to be able to compile and use this parser. You'll
also need [rebar](https://github.com/rebar/rebar) installed and accessible in
your `PATH`.


## Installation

You'll need to retrieve the dependencies and the compile the project. You can
do this by running:
```
make deps
make
```
And you can check that everything is working properly by running:
```
make test
```
