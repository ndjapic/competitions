# Problem: B_Who_is_Missing_.pas

```pascal
program B_Who_is_Missing_;
const
    nn = 1000;
var
    m, n, i, x, c: int32;
    seen: array [1 .. nn] of boolean;

begin
    readln(n, m);
    c := n-m;

    for x := 1 to n do seen[x] := false;

    for i := 1 to m do begin
        read(x);
        seen[x] := true;
    end;
    readln;

    writeln(c);
    x := 0;
    for i := 1 to c do begin
        inc(x);
        while seen[x] do inc(x);
        write(x);
        if i < c then write(' ');
    end;
    writeln;
end.

```
