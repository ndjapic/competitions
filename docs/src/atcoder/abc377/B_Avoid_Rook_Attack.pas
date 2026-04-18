program B_Avoid_Rook_Attack;
{$mode delphi}
var
    i, j, n, ans: int8;
    s: array [1 .. 8] of string;
    r, c: array [1 .. 8] of boolean;

begin
    n := 8;
    for i := 1 to n do begin
        r[i] := true;
        c[i] := true;
    end;

    for i := 1 to n do begin
        readln(s[i]);
        for j := 1 to n do
            if s[i][j] = '#' then begin
                r[i] := false;
                c[j] := false;
            end;
    end;

    ans := 0;
    for i := 1 to n do
        for j := 1 to n do
            if r[i] and c[j] then inc(ans);
    writeln(ans);
end.
