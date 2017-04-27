#!/bin/sh
#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Damien Doligez, projet Para, INRIA Rocquencourt            #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id: ocamlmktop.tpl,v 1.4 2002/04/24 09:40:19 xleroy Exp $


# Multi-shell script.  Works under Bourne Shell, MPW Shell, zsh.

if : == x
then # Bourne Shell or zsh
     exec %%BINDIR%%/ocamlc -linkall toplevellib.cma "$@" topstart.cmo
else # MPW Shell
     ocamlc -linkall toplevellib.cma {"parameters"} topstart.cmo
     exit {status}
End # uppercase E because "end" is a keyword in zsh
fi
