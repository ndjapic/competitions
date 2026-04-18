program A_TLD;
{$H+}
var
    n, i: int32;
    s: string;

begin
    readln(s);
    n := length(s);

    i := n;
    while s[i] <> '.' do dec(i);

    while i < n do begin
        inc(i);
        write(s[i]);
    end;
    writeln;
end.
