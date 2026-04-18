program A_CBC;
{$MODE DELPHI}
var
    n, i: int8;
    s: string;

begin
    readln(s);
    n := length(s);

    for i := 1 to n do
        if s[i] <= 'Z' then write(s[i]);
    writeln;
end.
