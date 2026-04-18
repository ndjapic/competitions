# Задатак: B_A_A.pas

```pascal
program B_A_A;
const
    maxn = 100;
var
    b: int64;
    a, e, ans: int8;
    apowa: array [1 .. 16] of int64;

begin
    readln(b);

    ans := -1;
    for a := 1 to 15 do begin
        apowa[a] := 1;
        for e := 1 to a do apowa[a] := apowa[a] * a;
        if apowa[a] = b then ans := a;
    end;

    writeln(ans);
end.

```
