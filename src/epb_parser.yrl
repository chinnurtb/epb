Nonterminals
    protobuf
    statement
    import_decl
    package_decl
    option_decl strict_option_decl optvalue
    message_decl message_contents message_content
    service_decl service_contents service_content
    extend_decl extend_contents extend_content
    enum_decl enum_contents enum_content
    field field_type field_options field_option_pair
    extension
    nested_id
    rpc_decl rpc_options
    .

Terminals
    ';' '=' '{' '}' '[' ']' '(' ')' '.' ','
    message enum service extend
    option import package
    identifier type rule
    public extensions to max rpc returns
    string float integer.

Rootsymbol protobuf.

protobuf -> statement : ['$1'].
protobuf -> statement protobuf : ['$1'|'$2'].

statement -> import_decl : '$1'.
statement -> package_decl : '$1'.
statement -> option_decl : '$1'.
statement -> message_decl : '$1'.
statement -> service_decl : '$1'.
statement -> extend_decl : '$1'.
statement -> enum_decl : '$1'.

package_decl -> package nested_id ';' : #package{name='$2', line=line('$1')}.

option_decl -> strict_option_decl : '$1'.
%% This is really only necessary for the old-style, but some protos
%% have it, e.g. syntax = "2"; WTF Google
option_decl -> identifier '=' optvalue ';':  #option{key=unpack('$1'), value='$3', line=line('$1')}.

strict_option_decl -> option identifier '=' optvalue ';': #option{key=unpack('$2'), value='$4', line=line('$1')}.

optvalue -> string : unpack('$1').
optvalue -> integer : unpack('$1').
optvalue -> float : unpack('$1').
optvalue -> nested_id : '$1'.

import_decl -> import string ';'        : #import{file=unpack('$2'), line=line('$1')}.
import_decl -> import public string ';' : #import{file=unpack('$3'), public=true, line=line('$1')}.

message_decl -> message nested_id '{' '}' : #message{name='$2', line=line('$1')}.
message_decl -> message nested_id '{' message_contents '}' : #message{name='$2', decls='$4', line=line('$1')}.

message_contents -> message_content : ['$1'].
message_contents -> message_content message_contents : ['$1'|'$2'].

message_content -> field : '$1'.
message_content -> extension : '$1'.
message_content -> strict_option_decl : '$1'.
message_content -> message_decl : '$1'.
message_content -> enum_decl : '$1'.
message_content -> extend_decl : '$1'.

extend_decl -> extend nested_id '{' '}' : #extend{name='$2', line=line('$1')}.
extend_decl -> extend nested_id '{' extend_contents '}' : #extend{name='$2', decls='$4', line=line('$1')}.

extend_contents -> extend_content : ['$1'].
extend_contents -> extend_content extend_contents : ['$1'|'$2'].

extend_content -> strict_option_decl : '$1'.
extend_content -> field : '$1'.

service_decl -> service nested_id '{' '}' : #service{name='$2', line=line('$1')}.
service_decl -> service nested_id '{' service_contents '}' : #service{name='$2', decls='$4', line=line('$1')}.

service_contents -> service_content : ['$1'].
service_contents -> service_content service_contents : ['$1'|'$2'].

service_content -> rpc_decl : '$1'.
service_content -> strict_option_decl : '$1'.

rpc_decl -> rpc identifier '(' nested_id ')' returns '(' nested_id ')' '{' '}' : #rpc{call=unpack('$2'), request='$4',
                                                                                      response='$8', line=line('$1')}.
rpc_decl -> rpc identifier '(' nested_id ')' returns '(' nested_id ')' '{' rpc_options '}' : #rpc{call=unpack('$2'), request='$4',
                                                                                                  response='$8', options='$11',
                                                                                                  line=line('$1')}.
rpc_decl -> rpc identifier '(' nested_id ')' returns '(' nested_id ')' ';' : #rpc{call=unpack('$2'), request='$4',
                                                                                      response='$8', line=line('$1')}.

rpc_options -> strict_option_decl : ['$1'].
rpc_options -> strict_option_decl rpc_options : ['$1'|'$2'].

enum_decl ->  enum nested_id '{' '}' : #enum{name='$2', line=line('$1')}.
enum_decl ->  enum nested_id '{' enum_contents '}' : #enum{name='$2', decls='$4', line=line('$1')}.

enum_contents -> enum_content enum_contents : ['$1'|'$2'].
enum_contents -> enum_content : ['$1'].

enum_content -> strict_option_decl : '$1'.
enum_content -> identifier '=' integer ';' : #enumval{name=unpack('$1'), value=unpack('$3'), line=line('$1')}.

extension -> extensions integer to max ';'     : #extensions{min=unpack('$2'), max=max, line=line('$1')}.
extension -> extensions integer to integer ';' : #extensions{min=unpack('$2'), max=unpack('$4'), line=line('$1')}.


field -> rule field_type identifier '=' integer '[' field_options ']' ';'      : #field{id=unpack('$5'), name=unpack('$3'),
                                                                                        type='$2', rule=unpack('$1'),
                                                                                        options='$7', line=line('$1')}.

field -> rule field_type identifier '=' integer '[' ']' ';'      : #field{id=unpack('$5'), name=unpack('$3'),
                                                                          type='$2', rule=unpack('$1'),
                                                                          line=line('$1')}.

field -> rule field_type identifier '=' integer ';'      : #field{id=unpack('$5'), name=unpack('$3'),
                                                                  type='$2', rule=unpack('$1'),
                                                                  line=line('$1')}.

field_options -> field_option_pair : ['$1'].
field_options -> field_option_pair ',' field_options : ['$1'|'$2'].

field_option_pair -> identifier '=' optvalue : {unpack('$1'), '$3'}.

%% TODO: figure out wtf they mean with the parens
%% nested_id -> '(' nested_id ')'        : #id{names=['$2'], line=line('$1')}.
%% nested_id -> '(' nested_id ')' '.' nested_id  : #id{names=['$2'|('$5')#id.names], line=line('$1')}.
nested_id -> identifier               : #id{names=[unpack('$1')],
                                            line=line('$1')}.
nested_id -> identifier '.' nested_id : #id{names=[unpack('$1')|('$3')#id.names],
                                            line=line('$1')}.

field_type -> nested_id : '$1'.
field_type -> type      : unpack('$1').

Erlang code.
-include("epb_ast.hrl").

unpack({_,_,V}) -> V.

line({_,L,_}) -> L;
line({_,L}) -> L.
