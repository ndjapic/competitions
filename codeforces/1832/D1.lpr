program Contrast_Value;
uses
    math;
const
    maxn = 300 * 1000;
var
    notc: int16;
    n, m, i: int32;
    a, b: array [1 .. maxn] of int32;

begin
    readln(notc);
    while notc > 0 do begin
        dec(notc);

        readln(n);

        for i := 1 to n do read(a[i]);
        readln;

        b[1] := a[1];
        m := 1;

        for i := 2 to n do begin

            if a[i] <> a[i-1] then begin
                inc(m);
                b[m] := a[i];
            end;

            if (m > 2) and (
                (
                    (b[m-2] < b[m-1]) and (b[m-1] < b[m])
                ) or (
                    (b[m-2] > b[m-1]) and (b[m-1] > b[m])
                )
            ) then begin
                b[m-1] := b[m];
                dec(m);
            end;

        end;

        writeln(m);

    end;
end.

