# Problem: A_Not_Too_Hard.pas

```pascal
program A_Not_Too_Hard;
var
    n, x, i, si, ans: int32;

begin
    readln(n, x);
    ans := 0;
    for i := 1 to n do begin

        read(si);
        if si <= x then inc(ans, si);

    end;
    readln;
    writeln(ans);
end.

```
