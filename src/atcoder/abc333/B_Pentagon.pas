program B_Pentagon; {$H+}
var
    s, t: string;

function is_side(v: string): boolean;
var
    i, d: int8;
    a: array [1 .. 2] of int8;
begin
    for i := 1 to 2 do
        a[i] := ord(v[i]) - ord('A');
    d := abs(a[1] - a[2]);
    is_side := (d = 1) or (d = 4);
end;

begin
    readln(s);
    readln(t);
    if is_side(s) = is_side(t) then
        writeln('Yes')
    else
        writeln('No');
end.
