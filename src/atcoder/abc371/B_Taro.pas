program B_Taro;
const
    nn = 100;
var
    n, m, i, a: int8;
    b: char;
    taro: array [1 .. nn] of boolean;

begin
    readln(n, m);
    for i := 1 to n do taro[i] := false;

    for i := 1 to m do begin
        readln(a, b, b);
        if (b = 'M') and not taro[a] then begin
            writeln('Yes');
            taro[a] := true;
        end else
            writeln('No');
    end;
end.
