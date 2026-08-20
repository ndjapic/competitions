program A_aaaadaa;
{$mode delphi}
var
    n, i: int8;
    c1, c2: char;
    s: string;

begin
    readln(n, c1, c1, c2, c2);
    readln(s);

    for i := 1 to n do
        if s[i] <> c1 then s[i] := c2;

    writeln(s);
end.
