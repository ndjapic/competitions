# Задатак: B_Uppercase_and_Lowercase.pas

```pascal
program B_Uppercase_and_Lowercase;
{$H+}
var
    n, i, u, l, d: int8;
    s: string;

begin
    readln(s);
    n := length(s);

    u := 0;
    l := 0;
    for i := 1 to n do
        if s[i] < 'a' then
            inc(u)
        else
            inc(l);

    if u > l then
        for i := 1 to n do begin
            d := ord(s[i]) - ord('a');
            if d >= 0 then
                s[i] := chr(ord('A') + d)
        end
    else
        for i := 1 to n do begin
            d := ord(s[i]) - ord('A');
            if d < 26 then
                s[i] := chr(ord('a') + d)
        end;

    writeln(s);
end.

```
