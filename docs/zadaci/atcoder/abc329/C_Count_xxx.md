# Задатак: C_Count_xxx.pas

```pascal
program C_Count_xxx;
uses
    math;
const
    maxn = 200 * 1000;
var
    n, l, r: int32;
    ans: int32;
    ch: char;
    s: array [1 .. maxn] of char;
    a: array ['a' .. 'z'] of int32;

begin
    for ch := 'a' to 'z' do a[ch] := 0;

    readln(n);
    l := 1;

    for r := 1 to n do begin
        read(s[r]);
        if s[r] <> s[l] then l := r;
        a[s[r]] := max(a[s[r]], r-l+1);
    end;
    readln;

    ans := 0;
    for ch := 'a' to 'z' do inc(ans, a[ch]);

    writeln(ans);
end.

```
