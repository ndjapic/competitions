# Problem: A_Equal_Subsequences.pas

```pascal
program A_Equal_Subsequences;
{$MODE DELPHI}
var
    ntc, tci, n, k, i: int16;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);
        setlength(s, n);
        for i := 1 to k do s[i] := '1';
        for i := k+1 to n do s[i] := '0';
        writeln(s);

    end;
end.

```
