# Задатак: A_System_of_Equations.pas

```pascal
program A_System_of_Equations;
var
    n, m, a, aa, b, ans: int32;

begin
    readln(n, m);

    ans := 0;
    a := 0;
    aa := 0;
    while aa <= n do begin
        b := n - aa;
        if a + b*b = m then inc(ans);
        inc(a);
        aa := a*a;
    end;

    writeln(ans);
end.

```
