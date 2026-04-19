# Problem: C_Many_Replacement.pas

```pascal
program C_Many_Replacement;
{$H+}
var
    n, q, i: int32;
    c, d, x: char;
    s: string;
    rep: array ['a' .. 'z'] of char;

begin
    readln(n);
    readln(s);

    for x := 'a' to 'z' do rep[x] := x;

    readln(q);
    for i := 1 to q do begin
        readln(c, d, d);
        for x := 'a' to 'z' do
            if rep[x] = c then rep[x] := d;
    end;

    for i := 1 to n do write(rep[s[i]]);
    writeln;
end.

```
