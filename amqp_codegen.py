## Copyright 2012 Tony Garnock-Jones <tonygarnockjones@gmail.com>.
##
## This file is part of Hop.
##
## Hop is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation, either version 3 of the License, or (at your
## option) any later version.
##
## Hop is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Hop.  If not, see <http://www.gnu.org/licenses/>.
##
from __future__ import with_statement

copyright_stmt = '''(* Copyright 2012 Tony Garnock-Jones <tonygarnockjones@gmail.com>. *)

(* This file is part of Hop. *)

(* Hop is free software: you can redistribute it and/or modify it *)
(* under the terms of the GNU General Public License as published by the *)
(* Free Software Foundation, either version 3 of the License, or (at your *)
(* option) any later version. *)

(* Hop is distributed in the hope that it will be useful, but *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *)
(* General Public License for more details. *)

(* You should have received a copy of the GNU General Public License *)
(* along with Hop.  If not, see <http://www.gnu.org/licenses/>. *)
'''

import sys
import xml.dom.minidom

###########################################################################
# XML utils
def attr(n,a,d=None): return n.getAttribute(a).strip() if n.hasAttribute(a) else d
def kids(e,t): return [k for k in e.getElementsByTagName(t) if k.parentNode is e]

##########################################################################
# Identifier utils

keywords = set('type'.split())

def mlify(s):
    s = s.replace('-', '_')
    s = s.replace(' ', '_')
    if s in keywords: s = s + '_'
    return s

def ctor(s):
    return mlify(s).capitalize()

def tname(s):
    return mlify(s) + '_t'

###########################################################################
# Load & parse the spec

with open('amqp0-9-1.stripped.xml') as f:
    spec_xml = xml.dom.minidom.parse(f)

amqp_elt = spec_xml.getElementsByTagName('amqp')[0]
major = int(attr(amqp_elt, 'major', '0'))
minor = int(attr(amqp_elt, 'minor', '0'))
port = int(attr(amqp_elt, 'port', '5672'))
revision = int(attr(amqp_elt, 'revision', '0'))

constant_elts = amqp_elt.getElementsByTagName('constant')
def constants():
    for e in constant_elts:
        yield (attr(e, 'name'), attr(e, 'value'))

domain_elts = amqp_elt.getElementsByTagName('domain')
domains = {}
for e in domain_elts:
    domains[attr(e, 'name')] = attr(e, 'type')
def resolve(typename):
    seen = set()
    while True:
        if typename in seen:
            return typename
        seen.add(typename)
        if typename in domains:
            typename = domains[typename]

class DatalikeMixin:
    @property
    def accessible_fields(self):
        return [f for f in self.fields if not f.reserved]

    def pattern(self, is_type = False, need_some = False):
        if not self.accessible_fields:
            return ctor(self.full_name)
        elif is_type:
            if need_some:
                types = [(tname(f.type)+' option' if f.type != 'bit' else 'bit_t')
                         for f in self.accessible_fields]
            else:
                types = [tname(f.type) for f in self.accessible_fields]
            return '%s of (%s)' % (ctor(self.full_name), ' * '.join(types))
        else:
            return '%s (%s)' % (ctor(self.full_name),
                                ', '.join((mlify(f.name) for f in self.accessible_fields)))

    @property
    def match_clause(self):
        return '  | ' + self.pattern() + ' ->'

class Class(DatalikeMixin):
    def __init__(self, index, name, fields, methods):
        self.index = index
        self.name = name
        self.fields = fields
        self.methods = methods

    @property
    def full_name(self):
        return self.name + '-properties'

class Method(DatalikeMixin):
    def __init__(self, class_name, class_index, has_content, deprecated,
                 index, name, synchronous, responses, fields):
        self.class_name = class_name
        self.class_index = class_index
        self.has_content = has_content
        self.deprecated = deprecated
        self.index = index
        self.name = name
        self.synchronous = synchronous
        self.responses = responses
        self.fields = fields

    @property
    def full_name(self):
        return self.class_name + '-' + self.name

class Field:
    def __init__(self, name, type, reserved):
        self.name = name
        self.type = type
        self.reserved = reserved

def load_fields(e):
    return [Field(attr(f, 'name'),
                  resolve(attr(f, 'domain', attr(f, 'type'))),
                  int(attr(f, 'reserved', '0'))) \
                for f in kids(e, 'field')]

class_elts = amqp_elt.getElementsByTagName('class')
classes = []
for e in class_elts:
    classes.append(Class(int(attr(e, 'index')),
                         attr(e, 'name'),
                         load_fields(e),
                         [Method(attr(e, 'name'),
                                 int(attr(e, 'index')),
                                 int(attr(m, 'content', '0')),
                                 int(attr(m, 'deprecated', '0')),
                                 int(attr(m, 'index')),
                                 attr(m, 'name'),
                                 int(attr(m, 'synchronous', '0')),
                                 [attr(r, 'name') for r in kids(m, 'response')],
                                 load_fields(m)) \
                              for m in kids(e, 'method')]))
methods = []
for c in classes:
    for m in c.methods:
        methods.append(m)

###########################################################################
# Implementation restrictions

for c in classes:
    if len(c.fields) > 15:
        # We'd need to deal with more than one flags word
        raise Exception("Having more than 15 fields in a class is not supported")

###########################################################################

class BitAccumulator:
    def __init__(self):
        self.acc = []

    @property
    def count(self):
        return len(self.acc)

    def add(self, x):
        self.acc.append(x)

    def flush(self):
        if self.acc:
            print '      write_octet output_buf (%s);' % (' lor '.join(self.acc),)
        self.acc = []

def print_codec():
    print copyright_stmt
    print '(* WARNING: Autogenerated code. Do not edit by hand! *)'
    print
    print 'open Amqp_wireformat'
    print 'open Sexp'
    print
    print 'let version = (%d, %d, %d)' % (major, minor, revision)
    print 'let port = %d' % (port,)
    print
    for (n, v) in constants():
        print 'let %s = %s' % (mlify(n), v)
    print
    for c in classes:
        print 'let %s_class_id = %d' % (mlify(c.name), c.index)
    print
    print 'type method_t ='
    for m in methods:
        print '  | ' + m.pattern(True)
    print
    print 'let has_content m = match m with '
    for m in methods:
        if m.has_content:
            print m.match_clause + ' true'
    print '  | _ -> false'
    print
    print 'type properties_t ='
    for c in classes:
        if c.fields:
            print '  | ' + c.pattern(True, True)
    print
    print 'let is_synchronous m = match m with '
    for m in methods:
        if not m.synchronous:
            print m.match_clause + ' false'
    print '  | _ -> true'
    print
    print 'let sexp_of_method m = match m with '
    for m in methods:
        print m.match_clause
        if m.accessible_fields:
            print '      Arr [Str "%s"; Str "%s"' % (m.class_name, m.name)
            for f in m.accessible_fields:
                print '           ; Arr [Str "%s"; sexp_of_%s(%s)]' % \
                    (f.name, mlify(f.type), mlify(f.name))
            print '          ]'
        else:
            print '      Arr [Str "%s"; Str "%s"]' % (m.class_name, m.name)
    print
    print 'let method_name class_index method_index = match (class_index, method_index) with'
    for m in methods:
        print '  | (%d, %d) -> "%s"' % (m.class_index, m.index, ctor(m.full_name))
    print '  | _ -> Printf.sprintf "unknown(%d/%d)" class_index method_index'
    print
    print 'let read_method class_index method_index input_buf = match (class_index, method_index) with'
    for m in methods:
        print '  | (%d, %d) ->' % (m.class_index, m.index)
        bits_remaining = 0
        for f in m.fields:
            target = '_' if f.reserved else mlify(f.name)
            if f.type == 'bit':
                if bits_remaining < 1:
                    print '      let bit_buffer = read_octet input_buf in'
                    bits_remaining = 8
                print '      let %s = (bit_buffer land %d) <> 0 in' % \
                    (target, 1 << (8 - bits_remaining))
                bits_remaining = bits_remaining - 1
            else:
                print '      let %s = read_%s input_buf in' % (target, mlify(f.type))
        print '      ' + m.pattern()
    print '  | _ -> raise (Amqp_exception (frame_error,'
    print '                                Printf.sprintf "Unknown method number %d/%d"'
    print '                                               class_index method_index))'
    print
    print 'let method_index m = match m with'
    for m in methods:
        print m.match_clause + ' (%d, %d)' % (m.class_index, m.index)
    print
    print 'let write_method m output_buf = match m with'
    for m in methods:
        print m.match_clause
        acc = BitAccumulator()
        for f in m.fields:
            source = 'reserved_value_'+mlify(f.type) if f.reserved else mlify(f.name)
            if f.type == 'bit':
                if acc.count >= 8:
                    acc.flush()
                acc.add('(if %s then %d else 0)' % (source, 1 << acc.count))
            else:
                acc.flush()
                print '      write_%s output_buf %s;' % (mlify(f.type), source)
        print '      ()'
    print
    print 'let sexp_of_properties p = match p with '
    for c in classes:
        if c.fields:
            print c.match_clause
            print '      let fields__ = [] in'
            for f in reversed(c.accessible_fields):
                print '      let fields__ = (match %s with Some v -> Arr [Str "%s"; sexp_of_%s(v)] :: fields__ | None -> fields__) in' % \
                    (mlify(f.name), f.name, mlify(f.type))
            print '      Arr fields__'
    print
    print 'let read_properties class_index input_buf = match class_index with'
    for c in classes:
        if c.fields:
            print '  | %d ->' % (c.index,)
            print '      let flags__ = read_short input_buf in'
            property_bit = 15
            for f in c.fields:
                target = '_' if f.reserved else mlify(f.name)
                if f.type == 'bit':
                    print '      let %s = if (flags__ land %d) <> 0 in' % \
                        (target, 1 << property_bit)
                else:
                    print ('      let %s = if (flags__ land %d) <> 0 then '+
                           'Some (read_%s input_buf) else None in') % \
                           (target, 1 << property_bit, mlify(f.type))
                property_bit = property_bit - 1
            print '      ' + c.pattern()
    print '  | _ -> raise (Amqp_exception (frame_error, Printf.sprintf "Unknown class number %d"'
    print '                                                            class_index))'
    print
    print 'let class_index p = match p with'
    for c in classes:
        if c.fields:
            print c.match_clause + ' ' + str(c.index)
    print
    print 'let properties_of_sexp class_id ps_sexp = match class_id with'
    for c in classes:
        if c.fields:
            print '  | %d ->' % (c.index,)
            for f in c.accessible_fields:
                print '      let %s = ref None in' % (mlify(f.name),)
            print '      (match ps_sexp with'
            print '      | Arr ps ->'
            print '        List.iter (fun (p) -> match p with'
            for f in c.accessible_fields:
                print '          | Arr [Str "%s"; v] -> %s := Some (%s_of_sexp v)' % \
                    (f.name, mlify(f.name), mlify(f.type))
            print '          | _ -> ()) ps'
            print '      | _ -> ());'
            print '      %s (%s)' % \
                (ctor(c.full_name),
                 ', '.join(('reserved_value_'+mlify(f.type) if f.reserved else '!'+mlify(f.name)
                            for f in c.fields)))
    print '  | _ -> die internal_error (Printf.sprintf "Bad content class %d" class_id)'
    print
    print 'let write_properties p output_buf = match p with'
    for c in classes:
        if c.fields:
            print c.match_clause
            print '      let flags__ = 0'
            property_bit = 15
            for f in c.fields:
                if not f.reserved:
                    if f.type == 'bit':
                        print '        lor (if %s then %d else 0)' % \
                            (mlify(f.name), 1 << property_bit)
                    else:
                        print '        lor (match %s with Some _ -> %d | None -> 0)' % \
                            (mlify(f.name), 1 << property_bit)
                property_bit = property_bit - 1
            print '      in'
            print '      write_short output_buf flags__;'
            for f in c.fields:
                source = 'reserved_value_%s' if f.reserved else mlify(f.name)
                if f.type != 'bit':
                    print ('      (match %s with Some v_ -> write_%s output_buf v_'+
                           ' | None -> ());') % \
                           (source, mlify(f.type))
            print '      ()'
    print

if __name__ == '__main__':
    print_codec()
