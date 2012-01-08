from __future__ import with_statement

# Copyright (C) 2012 Tony Garnock-Jones. All rights reserved.
copyright_stmt = '(* Copyright (C) 2012 Tony Garnock-Jones. All rights reserved. *)'

import sys
import json

def mlify(s):
    s = s.replace('-', '_')
    s = s.replace(' ', '_')
    return s

class MessageType:
    def __init__(self, j):
        self.wire_selector = j['selector']
        self.selector = mlify(self.wire_selector)
        self.constructor = self.selector.capitalize()
        self.wire_argnames = j['args']
        self.argnames = map(mlify, self.wire_argnames)

    def format_args(self, template, separator = ', '):
        return separator.join([template % (x,) for x in self.argnames])

with file("messages.json") as f:
    spec = map(MessageType, json.load(f))

def print_list(o, xs, sep, c):
    sys.stdout.write(o)
    needsep = False
    for x in xs:
        if needsep:
            sys.stdout.write(sep)
        else:
            needsep = True
        sys.stdout.write(x)
    sys.stdout.write(c)

def print_codec():
    print copyright_stmt
    print
    print 'open Sexp'
    print
    print 'type t ='
    for t in spec:
        if t.argnames:
            print '  | %s of ' % (t.constructor),
            print_list('(', ['Sexp.t' for n in t.argnames], ' * ', ')\n')
        else:
            print '  | %s' % t.constructor
    print '  | UNKNOWN of Sexp.t'
    print
    print 'let sexp_of_message m = match m with'
    for t in spec:
        sys.stdout.write('  | %s' % t.constructor)
        if t.argnames:
            print_list(' (', [n for n in t.argnames], ', ', ')')
        print ' ->'
        sys.stdout.write('    Arr [Str "%s"' % t.wire_selector)
        if t.argnames:
            print_list('; ', t.argnames, '; ', '')
        print ']'
    print '  | UNKNOWN s -> s'
    print
    print 'let message_of_sexp s = match s with'
    for t in spec:
        sys.stdout.write('  | Arr [Str "%s"' % t.wire_selector)
        if t.argnames:
            print_list('; ', t.argnames, '; ', '')
        print '] ->'
        sys.stdout.write('    %s' % t.constructor)
        if t.argnames:
            print_list(' (', [n for n in t.argnames], ', ', ')')
        print
    print '  | _ -> UNKNOWN s'
    print
    for t in spec:
        sys.stdout.write('let %s' % t.selector)
        if t.argnames:
            print_list(' (', t.argnames, ', ', ')')
        sys.stdout.write(' = sexp_of_message (%s' % t.constructor)
        if t.argnames:
            print_list(' (', t.argnames, ', ', ')')
        sys.stdout.write(')\n')
    print

if __name__ == '__main__':
    print_codec()
