program C_Keys;
const
    nn = 15;
    p2nn = int32(1) shl nn;
var
    n, m, k, i, j, c, a: int8;
    p2n, x, y, ans: int32;
    r: char;
    b: boolean;
    popcount: array [0 .. p2nn] of int8;
    enabled: array [0 .. p2nn] of boolean;

begin
    readln(n, m, k);
    p2n := int32(1) shl n;

    popcount[0] := 0;
    for x := 0 to p2n div 2 - 1 do begin
        popcount[2*x] := popcount[x];
        popcount[2*x+1] := popcount[x]+1;
    end;

    for x := 0 to p2n-1 do enabled[x] := true;

    for i := 1 to m do begin

        read(c);

        x := 0;
        for j := 1 to c do begin
            read(a);
            x := x or (int32(1) shl (a-1));
        end;

        read(r);
        readln(r);

        for y := 0 to p2n-1 do begin
            case r of
                'o': b := popcount[x and y] >= k;
                'x': b := popcount[x and y] < k;
            end;
            enabled[y] := enabled[y] and b;
        end;

    end;

    ans := 0;
    for x := 0 to p2n-1 do
        if enabled[x] then inc(ans);

    writeln(ans);
end.
