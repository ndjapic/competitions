# Problem: A_Hamming_Distance.pas

```pascal
program A_Hamming_Distance.pas;
{$H+}
var
    n, i, c: int8;
    s, t: string;

begin
    readln(n);
    readln(s);
    readln(t);

    c := 0;
    for i := 1 to n do
        if s[i] <> t[i] then inc(c);

    writeln(c);
end.

```
