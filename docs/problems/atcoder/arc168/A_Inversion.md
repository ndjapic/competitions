# Problem: A_Inversion.pas

```pascal
program A_Inversion;
const
    maxn = 250 * 1000;
var
    n, l, r: int32;
    ans: int64;
    s: array [1 .. maxn] of char;

begin
    readln(n);

    ans := 0;
    l := 1;

    for r := 1 to n-1 do begin
        read(s[r]);
        if s[r] = '<' then l := r+1;
        inc(ans, r-l+1);
    end;
    readln;

    writeln(ans);
end.

```
