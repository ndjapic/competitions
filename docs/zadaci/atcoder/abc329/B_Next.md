# Задатак: B_Next.pas

```pascal
program B_Next;
const
    maxn = 100;
var
    n, i, x: int8;
    seen: array [1 .. maxn] of boolean;

begin
    for x := 1 to maxn do seen[x] := false;

    readln(n);
    for i := 1 to n do begin
        read(x);
        seen[x] := true;
    end;
    readln;

    x := 100;
    while not seen[x] do dec(x);
    dec(x);
    while not seen[x] do dec(x);
    writeln(x);
end.

```
