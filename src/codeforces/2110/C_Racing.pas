program C_Racing;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i: int32;
    ans: boolean;
    d: array [1 .. nn] of int8;
    l, r: array [0 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(d[i]); readln;

        l[0] := 0;
        r[0] := 0;

        for i := 1 to n do begin
            readln(l[i], r[i]);

            if d[i] > -1 then begin
                l[i] := max(l[i], l[i-1] + d[i]);
                r[i] := min(r[i], r[i-1] + d[i]);
            end else begin
                l[i] := max(l[i], l[i-1] + 0);
                r[i] := min(r[i], r[i-1] + 1);
            end;

        end;

        for i := n downto 1 do begin
            if d[i] > -1 then begin
                l[i-1] := max(l[i-1], l[i] - d[i]);
                r[i-1] := min(r[i-1], r[i] - d[i]);
            end else begin
                l[i-1] := max(l[i-1], l[i] - 1);
                r[i-1] := min(r[i-1], r[i] - 0);
                d[i] := r[i] - r[i-1];
            end;
        end;

        ans := true;
        i := 0;
        while (i <= n) and ans do begin
            ans := l[i] <= r[i];
            inc(i);
        end;

        if ans then begin

            for i := 1 to n-1 do write(d[i], ' ');
            writeln(d[n]);

        end else
            writeln(-1);

    end;
end.
