Nonterminals
    protobuf
    statements statement
    import_decl
    package_decl
    option_decl strict_option_decl optvalue
    message_decl message_contents message_content
    service_decl service_contents service_content
    extend_decl extend_contents extend_content
    enum_decl enum_contents enum_content
    field field_type field_name field_options field_option_pair
    group_decl group_contents
    extension
    nested_id keyword_as_id
    rpc_decl rpc_options
    message_literal message_literal_fields field_literal field_literal_value
    end_block
    .

Terminals
    ';' ':' '=' '{' '}' '[' ']' '(' ')' '.' ','
    message enum service extend
    option import package
    identifier type rule
    public extensions to max rpc returns group
    string float integer.

Rootsymbol protobuf.

protobuf -> '$empty' : [].
protobuf -> statements : '$1'.

statements -> statement : ['$1'].
statements -> statement statements : ['$1'|'$2'].

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
option_decl -> nested_id '=' optvalue ';':  #option{key='$1', value='$3', line=line('$1')}.

strict_option_decl -> option nested_id '=' optvalue ';': #option{key='$2', value='$4', line=line('$1')}.

optvalue -> message_literal: '$1'.
optvalue -> string : unpack('$1').
optvalue -> integer : unpack('$1').
optvalue -> float : unpack('$1').
optvalue -> nested_id : '$1'.

import_decl -> import string ';'        : #import{file=unpack('$2'), line=line('$1')}.
import_decl -> import public string ';' : #import{file=unpack('$3'), public=true, line=line('$1')}.

message_decl -> message nested_id '{' end_block : #message{name='$2', line=line('$1')}.
message_decl -> message nested_id '{' message_contents end_block : #message{name='$2', decls='$4', line=line('$1')}.

message_contents -> message_content : ['$1'].
message_contents -> message_content message_contents : ['$1'|'$2'].

message_content -> group_decl : '$1'.
message_content -> field : '$1'.
message_content -> extension : '$1'.
message_content -> strict_option_decl : '$1'.
message_content -> message_decl : '$1'.
message_content -> enum_decl : '$1'.
message_content -> extend_decl : '$1'.

extend_decl -> extend nested_id '{' end_block : #extend{name='$2', line=line('$1')}.
extend_decl -> extend nested_id '{' extend_contents end_block : #extend{name='$2', decls='$4', line=line('$1')}.

extend_contents -> extend_content : ['$1'].
extend_contents -> extend_content extend_contents : ['$1'|'$2'].

extend_content -> group_decl : '$1'.
extend_content -> strict_option_decl : '$1'.
extend_content -> field : '$1'.

service_decl -> service nested_id '{' end_block : #service{name='$2', line=line('$1')}.
service_decl -> service nested_id '{' service_contents end_block : #service{name='$2', decls='$4', line=line('$1')}.

service_contents -> service_content : ['$1'].
service_contents -> service_content service_contents : ['$1'|'$2'].

service_content -> rpc_decl : '$1'.
service_content -> strict_option_decl : '$1'.

rpc_decl -> rpc identifier '(' nested_id ')' returns '(' nested_id ')' '{' end_block : #rpc{call=unpack('$2'), request='$4',
                                                                                      response='$8', line=line('$1')}.
rpc_decl -> rpc identifier '(' nested_id ')' returns '(' nested_id ')' '{' rpc_options end_block : #rpc{call=unpack('$2'), request='$4',
                                                                                                  response='$8', options='$11',
                                                                                                  line=line('$1')}.
rpc_decl -> rpc identifier '(' nested_id ')' returns '(' nested_id ')' ';' : #rpc{call=unpack('$2'), request='$4',
                                                                                      response='$8', line=line('$1')}.

rpc_options -> strict_option_decl : ['$1'].
rpc_options -> strict_option_decl rpc_options : ['$1'|'$2'].

enum_decl ->  enum nested_id '{' end_block : #enum{name='$2', line=line('$1')}.
enum_decl ->  enum nested_id '{' enum_contents end_block : #enum{name='$2', decls='$4', line=line('$1')}.

enum_contents -> enum_content enum_contents : ['$1'|'$2'].
enum_contents -> enum_content : ['$1'].

enum_content -> strict_option_decl : '$1'.
enum_content -> identifier '=' integer ';' : #enumval{name=unpack('$1'), value=unpack('$3'), line=line('$1')}.
enum_content -> identifier '=' integer '[' ']' ';' : #enumval{name=unpack('$1'), value=unpack('$3'), line=line('$1')}.
enum_content -> identifier '=' integer '[' field_options ']' ';' : #enumval{name=unpack('$1'), value=unpack('$3'), options='$5', line=line('$1')}.

extension -> extensions integer ';' : #extensions{min=unpack('$2'), max=unpack('$2'), line=line('$1')}.
extension -> extensions integer to max ';'     : #extensions{min=unpack('$2'), max=max, line=line('$1')}.
extension -> extensions integer to integer ';' : #extensions{min=unpack('$2'), max=unpack('$4'), line=line('$1')}.

group_decl -> rule group field_name '=' integer '{' end_block : #group{id=unpack('$5'),
                                                                       name='$3',
                                                                       rule=unpack('$1'),
                                                                       line=line('$1')}.
group_decl -> rule group field_name '=' integer '{' group_contents end_block : #group{id=unpack('$5'),
                                                                                      name='$3',
                                                                                      rule=unpack('$1'),
                                                                                      decls='$7',
                                                                                      line=line('$1')}.

group_contents -> field : ['$1'].
group_contents -> field group_contents : ['$1'|'$2'].

field -> rule field_type field_name '=' integer '[' field_options ']' ';' : #field{id=unpack('$5'), name='$3',
                                                                                   type='$2', rule=unpack('$1'),
                                                                                   options='$7', line=line('$1')}.

field -> rule field_type field_name '=' integer '[' ']' ';' : #field{id=unpack('$5'), name='$3',
                                                                     type='$2', rule=unpack('$1'),
                                                                     line=line('$1')}.

field -> rule field_type field_name '=' integer ';' : #field{id=unpack('$5'), name='$3',
                                                             type='$2', rule=unpack('$1'),
                                                             line=line('$1')}.

field_name -> identifier : unpack('$1').
field_name -> keyword_as_id : '$1'.

field_options -> field_option_pair : ['$1'].
field_options -> field_option_pair ',' field_options : ['$1'|'$2'].

field_option_pair -> identifier '=' optvalue : {unpack('$1'), '$3'}.
field_option_pair -> '(' identifier ')' '=' optvalue : {unpack('$2'), '$5'}.

nested_id -> '.' nested_id                    : ('$2')#id{line=line('$1')}.
nested_id -> '(' nested_id ')'                : ('$2')#id{line=line('$1')}.
nested_id -> '(' nested_id ')' '.' nested_id  : #id{names=('$2')#id.names ++ ('$5')#id.names, line=line('$1')}.
nested_id -> identifier                       : #id{names=[unpack('$1')], line=line('$1')}.
nested_id -> identifier '.' nested_id         : #id{names=[unpack('$1')|('$3')#id.names],
                                                    line=line('$1')}.

message_literal -> '[' nested_id ']' '{' '}' : #message_literal{fields=[], extend='$2', line=line('$1')}.
message_literal -> '[' nested_id ']' '{' message_literal_fields '}' : #message_literal{fields='$5', extend='$2', line=line('$1')}.
message_literal -> '{' '}' : #message_literal{fields=[], line=line('$1')}.
message_literal -> '{' message_literal_fields '}' : #message_literal{fields='$2', line=line('$1')}.
message_literal -> '{' message_literal '}' : '$2'.

message_literal_fields -> field_literal : ['$1'].
message_literal_fields -> field_literal message_literal_fields : ['$1'|'$2'].

field_literal -> identifier message_literal : #field_literal{name=unpack('$1'), value='$2', line=line('$1')}.
field_literal -> identifier ':' field_literal_value : #field_literal{name=unpack('$1'), value='$3', line=line('$1')}.

field_literal_value -> string : unpack('$1').
field_literal_value -> integer : unpack('$1').
field_literal_value -> float : unpack('$1').

field_type -> nested_id : '$1'.
field_type -> type      : unpack('$1').

keyword_as_id -> message :  atom_to_list(element(1, '$1')).
keyword_as_id -> enum : atom_to_list(element(1, '$1')).
keyword_as_id -> service : atom_to_list(element(1, '$1')).
keyword_as_id -> extend : atom_to_list(element(1, '$1')).
keyword_as_id -> option : atom_to_list(element(1, '$1')).
keyword_as_id -> import : atom_to_list(element(1, '$1')).
keyword_as_id -> package : atom_to_list(element(1, '$1')).
keyword_as_id -> to : atom_to_list(element(1, '$1')).
keyword_as_id -> max : atom_to_list(element(1, '$1')).
keyword_as_id -> rpc : atom_to_list(element(1, '$1')).
keyword_as_id -> returns : atom_to_list(element(1, '$1')).
keyword_as_id -> extensions : atom_to_list(element(1, '$1')).
keyword_as_id -> type : atom_to_list(unpack('$1')).
keyword_as_id -> rule : atom_to_list(unpack('$1')).

%% Some google .protos (e.g. descriptor.proto) allow a semicolon after
%% a block statement, especially when declaring enums.
end_block -> '}' : '$1'.
end_block -> '}' ';' : '$1'.

Erlang code.
-include("epb_ast.hrl").
-export([file/1, main/1]).

unpack({_,_,V}) -> V.

line({_,L,_}) -> L;
line({_,L}) -> L.

file(Filename) ->
    case epb_scanner:file(Filename) of
        {ok, Toks, _} ->
            parse(Toks);
        Other -> Other
    end.

main([Filename]) ->
    case file(Filename) of
        {ok, AST} ->
            io:format("~p~n", [AST]);
        Other ->
            io:format("ERROR: ~p~n", [Other]),
            halt(1)
    end.
