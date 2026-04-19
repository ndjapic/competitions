# Problem: A_Not_Found.pas

```pascal
program A_Not_Found;
{$MODE DELPHI}
const
    nn = 25;
var
    n, i: int8;
    s: string;
    seen: array [0 .. nn] of boolean;

function o(ch: char): int8;
begin
    o := ord(ch) - ord('a');
end;

begin
    readln(s);
    n := length(s);

    for i := 0 to 25 do seen[i] := false;
    for i := 1 to n do seen[o(s[i])] := true;

    i := 0;
    while seen[i] do inc(i);

    writeln(chr(ord('a') + i));
end.

```
