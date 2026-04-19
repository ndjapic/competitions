# Problem: C_Word_Ladder.pas

```pascal
program C_Word_Ladder;
{$mode delphi}
var
    n, i, m: int8;
    s, t: string;

begin
    readln(s);
    readln(t);
    n := length(s);

    m := 0;
    for i := 1 to n do
        if s[i] <> t[i] then
            inc(m);

    writeln(m);

    for i := 1 to n do
        if s[i] > t[i] then begin
            s[i] := t[i];
            writeln(s);
        end;

    for i := n downto 1 do
        if s[i] < t[i] then begin
            s[i] := t[i];
            writeln(s);
        end;
end.

```
