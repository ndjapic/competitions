program A_Good_Permutation_2;
const
    nn = 200 * 1000;
var
    n, m, i, j, x: int32;
    seen: array [1 .. nn] of boolean;
    p: array [1 .. nn] of int32;

begin
    readln(n, m);

    for i := 1 to n do begin
        seen[i] := false;
        p[i] := i;
    end;

    for j := 1 to m do begin
        read(i);
        seen[i] := true;
    end;
    readln;

    if seen[1] or seen[n] then
        writeln(-1)
    else begin

        for i := 1 to n do
            if seen[i] then begin
                x := p[i];
                p[i] := p[i+1];
                p[i+1] := x;
            end;

        for i := 1 to n-1 do write(p[i], ' ');
        writeln(p[n]);

    end;
end.
