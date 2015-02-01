% A Brainfuck Interpreter in Erlang
%
% Copyright (c) 2015, Jens Luedicke <jens.luedicke@gmail.com>
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%     * Redistributions of source code must retain the above copyright
%       notice, this list of conditions and the following disclaimer.
%     * Redistributions in binary form must reproduce the above copyright
%       notice, this list of conditions and the following disclaimer in the
%       documentation and/or other materials provided with the distribution.
%     * Neither the name of the <organization> nor the
%       names of its contributors may be used to endorse or promote products
%       derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
% DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(brainfuck).
-export([run/1]).

parse(Data) ->
    case Data of
        "+" -> opplus;
        "-" -> opminus;
        ">" -> opnext;
        "<" -> opprev;
        "," -> opin;
        "." -> opout;
        "[" -> oploopbegin;
        "]" -> oploopend;
        _   -> opignore
    end.

findloop([], LoopCodes, _) ->
    LoopCodes;

findloop([Op|OpCodes], LoopCodes, LoopDepth) ->
%    io:format("findloop ~B ~n", [LoopDepth]),
    if
        (Op == oploopbegin) ->
            findloop(OpCodes, lists:flatten([LoopCodes] ++ [Op]), LoopDepth + 1);
        (Op == oploopend) and (LoopDepth > 1) ->
            findloop(OpCodes, lists:flatten([LoopCodes] ++ [Op]), LoopDepth - 1);
        (Op /= oploopbegin) and (Op /= oploopend) ->
            findloop(OpCodes, lists:flatten([LoopCodes] ++ [Op]), LoopDepth);
        (LoopDepth == 0) ->
            LoopCodes;
        true ->
            LoopCodes
    end.

evalloop(OpCodes, Tape, Pos) ->
    Value = maps:get(Pos, Tape, 0),
%    io:format("evalloop ~B ~B ~n", [Pos, Value]),

    if
        Value /= 0 ->
            {NewTape, NewPos} = eval(OpCodes, Tape, Pos),
            evalloop(OpCodes, NewTape, NewPos);
        true ->
%            io:format("evalloop ~B ~B is 0. Stop.~n", [Pos, Value]),
            {Tape, Pos}
    end.

eval([], Tape, Pos) -> {Tape, Pos};

eval([Op|OpCodes], Tape, Pos) ->
    case Op of
        opplus ->
            Value   = maps:get(Pos, Tape, 0) + 1,
            NewTape = maps:put(Pos, Value, Tape),
%            io:format("eval: opplus ~B ~B ~n", [Pos, Value]),
            eval(OpCodes, NewTape, Pos);
        opminus ->
            Value   = maps:get(Pos, Tape, 0) - 1,
            NewTape = maps:put(Pos, Value, Tape),
%            io:format("eval: opminus ~B ~B ~n", [Pos, Value]),
            eval(OpCodes, NewTape, Pos);
        opnext ->
%            io:format("eval: opnext ~B ~B ~n", [Pos, Pos + 1]),
            eval(OpCodes, Tape, Pos + 1);
        opprev ->
%            io:format("eval: opprev ~B ~B ~n", [Pos, Pos - 1]),
            eval(OpCodes, Tape, Pos - 1);
        opin ->
            Data = io:get_chars("", 1),
            Char = lists:nth(1, Data),
            io:format("eval: opin ~B ~B ~n", [Pos, Char]),
            NewTape = maps:put(Pos, Char, Tape),
            eval(OpCodes, NewTape, Pos);
        opout ->
            Value   = maps:get(Pos, Tape, 0),
%            io:format("eval: opout ~B ~B ~n", [Pos, Value]),
            io:format("~c", [Value]),
            eval(OpCodes, Tape, Pos);
        oploopbegin ->
%            io:format("eval: oploopbegin ~B ~n", [Pos]),
            LoopCodes = findloop(OpCodes, [], 1),
            {NewTape, NewPos} = evalloop(LoopCodes, Tape, Pos),
            eval(OpCodes -- LoopCodes, NewTape, NewPos);
        oploopend ->
%            io:format("eval: oploopend ~B ~n", [Pos]),
            eval(OpCodes, Tape, Pos);
        _ ->
          eval(OpCodes, Tape, Pos)
    end.

read_file(IoDevice, OpCodes) ->
    case file:read(IoDevice, 1) of
        {ok, Data} ->
            OpCode = parse(Data),
            read_file(IoDevice, lists:flatten([OpCodes] ++ [OpCode]));
        eof ->
            {ok, OpCodes};
        {error, Reason} ->
            {error, Reason}
    end.

run(File) ->
    case file:open(File, read) of
        {ok, IoDevice} ->
            case read_file(IoDevice, []) of
                {ok, OpCodes} ->
                    Tape = maps:new(),
                    Pos = 0,
                    eval(OpCodes, Tape, Pos);
                {error, Reason} ->
                    io:format("Unable to read file ~s: ~s ~n", [File, Reason])
            end,
            file:close(IoDevice);
        {error, Reason} ->
            io:format("Unable to open file ~s: ~s ~n", [File, Reason])
    end.
