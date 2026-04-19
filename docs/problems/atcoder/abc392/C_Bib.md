# Problem: C_Bib.pas

```pascal
program C_Bib;
const
    nn = 300 * 1000;
var
    n, i: int32;
    p, q, invq, s: array [1 .. nn] of int32;

begin
    readln(n);

    for i := 1 to n do read(p[i]);
    readln;

    for i := 1 to n do begin
        read(q[i]);
        invq[q[i]] := i;
    end;
    readln;

    for i := 1 to n do begin
        s[i] := q[p[invq[i]]];
        write(s[i]);
        if i < n then write(' ');
    end;
    writeln;
end.

```
