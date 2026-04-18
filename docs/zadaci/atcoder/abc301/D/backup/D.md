# Задатак: D.pas

```pascal
program Bitmask;
var
    n, p2, ans: int64;
    c, i: int8;
    s: array [1 .. 60] of char;

begin
    c := 0;
    ans := 0;

    repeat
        inc(c);
        read(s[c]);
        inc(ans, ans);
        if s[c] = '1' then inc(ans);
    until eoln;
    readln;

    readln(n);

    for i := 1 to c do
        if s[i] = '?' then begin
            p2 := int64(1) shl (c-i);
            if ans + p2 <= n then inc(ans, p2);
        end;

    writeln(ans);
end.


```
