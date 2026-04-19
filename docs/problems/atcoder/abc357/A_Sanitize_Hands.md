# Problem: A_Sanitize_Hands.pas

```pascal
program A_Sanitize_Hands;
var
    n, m, i, h, ans: int8;

begin
    readln(n, m);

    ans := 0;
    for i := 1 to n do begin
        read(h);
        if m >= h then inc(ans);
        if m >= 0 then dec(m, h);
    end;
    readln;

    writeln(ans);
end.

```
