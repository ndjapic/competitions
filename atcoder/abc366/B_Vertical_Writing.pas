program B_Vertical_Writing;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 100;
var
    n, m, i, j: int8;
    s, t: array [1 .. nn] of string;

begin
    readln(n);
    m := 0;

    for i := 1 to n do begin
        readln(s[i]);
        m := max(m, length(s[i]));
    end;

    for j := 1 to m do begin

        setlength(t[j], n);

        for i := 1 to n do
            if j <= length(s[i]) then
                t[j][n-i+1] := s[i][j]
            else
                t[j][n-i+1] := '*';

        i := 1;
        while t[j][n-i+1] = '*' do inc(i);
        setlength(t[j], n-i+1);
        writeln(t[j]);

    end;
end.
